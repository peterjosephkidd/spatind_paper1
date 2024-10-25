create_heatmap_data <- function(rocdata, group_vars = NULL) {
  heatmap_data <- rocdata %>%
    ungroup() %>%
    group_by_at(vars(group_vars)) %>%
    summarise(meanAUC = mean(AUC)) %>%
    arrange(across(all_of(group_vars)), meanAUC)
  
  return(heatmap_data)
}



pivot_long_heatmap <- function(heatmap_data, col_index) {
  heatmap_data_long <- heatmap_data %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = c(meanAUC),
                        names_to = "SummaryStat",
                        values_to = "Mean") %>%
    mutate(col_index = {{ col_index }},
           col_index_num = match(col_index, rev(sort(unique(col_index)))),
           cond = case_when(
             Mean >= 0.8 ~ "++",
             Mean >= 0.6 & Mean < 0.8 ~ "+",
             Mean <= 0.4 & Mean > 0.2 ~ "-",
             Mean <= 0.2 ~ "--",
             TRUE ~ NA_character_
           ))
  
  return(heatmap_data_long)
}


plot_heatmap <- function(heatmap_data, 
                         y_order = sort(unique(heatmap_data$col_index)), 
                         x_order = sort(unique(heatmap_data$`Spatial Indicator`)), 
                         filter_var = NULL, filter_val = NULL) {
  
  heatmap_data$col_index <- factor(heatmap_data$col_index, level = rev(y_order))
  heatmap_data$`Spatial Indicator` <- factor(heatmap_data$`Spatial Indicator`, level = x_order)
  
  if (!is.null(filter_var) && !is.null(filter_val)) {
    filter_conditions <- Map(function(var, val) heatmap_data[[var]] == val, filter_var, filter_val)
    plot_data <- heatmap_data[Reduce(`&`, filter_conditions), ]
  } else {
    plot_data <- heatmap_data
  }
  
  ggplot(plot_data, aes(y = col_index, 
                        x = `Spatial Indicator`, 
                        fill = Mean)) +
    geom_tile() +
    geom_text(aes(label = cond)) +
    scale_fill_gradient2(low = "red3", mid = "white", high = "green4", limits = c(0,1), midpoint = 0.5) +
    labs(title = "Heatmap of spatial indicator performance across groups",
         y = "Groups",
         x = "Spatial Indicator",
         fill = "AUC",
         color = "AUC") +
    scale_x_discrete(position = "top") +
    theme_minimal()
}

auc_stats <- function(bindata, group_vars = NULL, avg_across_L50 = FALSE) {
  stats_1 <- bindata %>%
    group_by_at(vars(all_of(c("Spatial Indicator", "L50lvl", group_vars)))) %>%
    summarise(ind_category = unique(ind_category),
              n = length(AUC),                # sample points (number of AUC values)
              mu = mean(AUC),                 # mean
              SS = sum((mu - AUC)^2),         # sm of squares
              stndrd_dev = sqrt(SS/n),        # standard deviation 
              se = stndrd_dev/sqrt(n),        # standard error
              CI95_margin = se * 1.96,        # 95% CI margin
              lower_ci = mu - CI95_margin,    # lower CI
              upper_ci = mu + CI95_margin,    # upper CI
              median_auc = median(AUC)) %>% # median 
    mutate(cond = case_when(
            mu >= 0.8 ~ "++",
            mu >= 0.6 & mu < 0.8 ~ "+",
            mu <= 0.4 & mu > 0.2 ~ "-",
            mu <= 0.2 ~ "--",
            TRUE ~ NA_character_)) %>%
    arrange(-mu)
  
  if (avg_across_L50 == F) {
    
    return(stats_1)
    
  } else {
    # Calculate the mean across L50 with SE representing variaince across sensitivity tests
    stats_2 <- stats_1 %>%
      group_by_at(vars(all_of(c("Spatial Indicator", "ind_category", group_vars)))) %>%
      summarise(n = length(mu),                   # sample points (number of means)
                mu2 = mean(mu),                   # mean of means
                SS = sum((mu2 - mu)^2),           # sum of squares
                stndrd_dev = sqrt(SS/n),          # standard deviation
                se = stndrd_dev/sqrt(n),          # standard error
                CI95_margin = se * 1.96,          # 95% CI margin 
                lower_ci = mu2 - CI95_margin,     # lower CI
                upper_ci = mu2 + CI95_margin) %>% # upper CI
      rename(mu = mu2) %>%
      mutate(lower_ci = if_else(n == 1, NA, lower_ci),
             upper_ci = if_else(n == 1, NA, upper_ci),
             cond = case_when(
               mu >= 0.8 ~ "++",
               mu >= 0.6 & mu < 0.8 ~ "+",
               mu <= 0.4 & mu > 0.2 ~ "-",
               mu <= 0.2 ~ "--",
               TRUE ~ NA_character_)) %>%
      arrange(-mu)
    
    return(stats_2)
  }
}

create_auc_bindata <- function(auc_summary_data, bins = seq(0,1, by = 0.1)) {
  bindata <- auc_summary_data %>%
    mutate(AUC = round(AUC, 10), # resolve floating point issue
           AUC_bin = cut( # split AUC into 0.1 groups
             AUC, breaks = bins, include.lowest = TRUE, right = FALSE, labels = FALSE
           )
    ) %>%
    na.omit()
  return(bindata)
}

create_auc_binfreq <- function(bindata){
  binfreq_data <- table(bindata[["Spatial Indicator"]], bindata[["AUC_bin"]], bindata[["L50lvl"]]) %>%
    as.data.frame(.) %>%
    rename(`Spatial Indicator` = Var1,
           L50lvl = Var3,
           AUC_bin = Var2) %>%
    mutate(
      AUC_bin = as.numeric(AUC_bin),
      AUC_cat = case_when(
        AUC_bin <= max(AUC_bin)*0.2                                ~ "Worst",
        AUC_bin >  max(AUC_bin)*0.2 & AUC_bin <= max(AUC_bin)*0.4  ~ "Bad",
        AUC_bin >  max(AUC_bin)*0.4 & AUC_bin <  max(AUC_bin)*0.6  ~ "Random",
        AUC_bin >= max(AUC_bin)*0.6 & AUC_bin <  max(AUC_bin)*0.8  ~ "Good",
        AUC_bin >= max(AUC_bin)*0.8                                ~ "Best",
        TRUE ~ NA_character_ # Return NA for unmatched values
      ), 
      AUC_cat = factor(AUC_cat, levels = c("Worst", "Bad", "Random", "Good", "Best")),
      L50lvl = factor(L50lvl, levels = c("lowerCI", "upperCI", "mean"), labels = c("Lower CI", "Upper CI", "Mean")),
      Freq = as.numeric(Freq))
  return(binfreq_data)
}

plot_auc_binfreq <- function(binfreq_data, auc_stats_data) {
  
  # Add Density
  Ndens <- binfreq_data %>%
    group_by(L50lvl, `Spatial Indicator`) %>%
    summarise(N = sum(Freq))

  binfreq_data <- left_join(binfreq_data, Ndens, by = c("L50lvl", "Spatial Indicator")) %>%
    mutate(Dens = Freq/N)
  
  
  ggplot() +

    geom_rect(data = auc_stats_data, 
              aes(xmin = lower_ci, xmax = upper_ci,
                  ymin = -Inf, ymax = Inf, fill = "95% CI"), linetype = 2, alpha = 0.4) +
    geom_point(data = binfreq_data, aes(x = AUC_bin/10, y = Dens, color = L50lvl)) +
    geom_line(data = binfreq_data, aes(x = AUC_bin/10, y = Dens, group = L50lvl, color = L50lvl)) +
    
    geom_area(data = binfreq_data[binfreq_data$L50lvl == "Mean",], 
              aes(x = ifelse(AUC_bin/10 >= 0, AUC_bin/10, 0), 
                  y = Dens, fill = L50lvl, group = L50lvl), fill = "cyan4", alpha = 0.1, position = "identity") +

    labs(x = "AUC Bins", y = "Frequency Density", title = "Frequency Density of AUC") +
    scale_color_manual(values = c("grey70", "grey40", "cyan4"), name = "L50 Sensitivity Tests") +
    scale_fill_manual(values = c("pink"), name = "") +
    scale_linetype_manual(values = c(3), name = "") +
    geom_vline(data = auc_stats_data, aes(xintercept = mu)) +
    scale_x_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 0.35), breaks = seq(0.1, 1, by = 0.1), expand = c(0,0)) +
    facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scale = "free_x") +
    theme(axis.text.x = element_text(hjust=1),
          # Panels
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.background   = element_blank(),
          panel.border       = element_rect(colour = "black", fill = NA),
          strip.background   = element_rect(colour = "black"),
          # Legend
          legend.position = "right") +
    labs(linetype = "Central Tendency")
}

plot_auc_hist <- function(binfreq_data, auc_stats_data, L50_test = "Mean"){
  
  auc_stats_data_long <- auc_stats_data %>%
    tidyr::pivot_longer(cols = c(n, mu, SS, stndrd_dev, se, CI95_margin, lower_ci, upper_ci, median_auc),
                        names_to = "SummaryStat",
                        values_to = "Value")
  
  ggplot() +
    geom_col(data = binfreq_data[binfreq_data$L50lvl == L50_test,], 
             aes(x = AUC_bin/10, y = Freq, 
                 fill = factor(AUC_cat, levels = c("Worst", "Bad", "Random", "Good", "Best")))) +
    scale_fill_manual(values = c("Worst" = "grey20", "Bad" = "grey40", "Random" = "grey60", "Good" = "grey80", "Best" = "cyan4")) +
    
    geom_vline(data = auc_stats_data_long[auc_stats_data_long$SummaryStat %in% c("mu", "median_auc") & auc_stats_data_long$L50lvl == "mean",], 
               aes(xintercept = Value, linetype = factor(SummaryStat, labels = c("Median", "Mean")))) +
    labs(x = "AUC Bins", y = "Frequency", title = paste0("Histogram of AUC Scores (L50 = ", L50_test, ")")) +
    facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scales = "free_x") +
    scale_x_continuous(breaks = seq(0,1, by=.1), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,20, by = 2), expand = c(0,0)) +
    theme(axis.text.x = element_text(hjust=1),
          # Panels
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.background   = element_blank(),
          panel.border       = element_rect(colour = "black", fill = NA),
          strip.background   = element_rect(colour = "black"),
          # Legend
          legend.position = "right") +
    labs(fill = "AUC Performance",
         linetype = "Central Tendency") +
    coord_cartesian(ylim = c(0,17))
}

plot_auc_barchart <- function(data, indorder = sort(unique(data$`Spatial Indicator`)), facet_var = NULL, facet_order = NULL) {
  
  if (!is.null(facet_order)) {
    data[[facet_var]] <- factor(data[[facet_var]], levels = facet_order)
  } else {
    data[[facet_var]] <- factor(data[[facet_var]])
  }  
  
  ggplot(data, aes(x = factor(`Spatial Indicator`, levels = c(indorder)))) +
    geom_col(     aes(y = mu, fill = ind_category)) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
    geom_text(    aes(y = mu + 0.1, label = cond)) +
    
    geom_hline(yintercept = c(0.4, 0.5, 0.6), linetype = 3) +
    facet_wrap({{ facet_var }}, scale = "free_x") +
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0),
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.background   = element_blank(),
          panel.border       = element_rect(colour = "black", fill = NA),
          strip.background   = element_rect(colour = "black"),
          legend.position = "right") +
    labs(fill = "Indicator Type", x = "Spatial Indicator", y = "AUC",
         title = "Mean Spatial Indicator AUC scores")
}

df_to_latex <- function(df, caption = "Generated Table", label = "tab:my_table", resize = TRUE, 
                        align = rep("l", ncol(df)), add_row_numbers = FALSE, 
                        row_spacing = "\\addlinespace[0.5ex]", header_lines = 1, 
                        footnotes = NULL, borders = c("top", "mid", "bottom"), 
                        format_rows = NULL, format_cols = NULL, format_style = NULL, 
                        bold_header = FALSE, wrap_text = NULL, text_width = NULL, 
                        font_size = NULL, file_path = NULL, 
                        nest_cols = NULL, nest_cols_rename = NULL, nest_header = NULL, longtable = FALSE, position = "ht") {
  
  # Check if the dataframe is empty
  if (nrow(df) == 0) {
    stop("The dataframe is empty.")
  }
  
  # Validate alignment input
  if (length(align) != ncol(df)) {
    stop("Length of alignment vector does not match number of columns in dataframe.")
  }
  
  # Validate format_style input
  if (!is.null(format_style)) {
    if (!all(format_style %in% c("bold", "italics"))) {
      stop("Invalid format_style values. Use 'bold' and/or 'italics'.")
    }
  }
  
  # Validate wrap_text and text_width input
  if (!is.null(wrap_text) && !is.null(text_width)) {
    if (length(wrap_text) != length(text_width)) {
      stop("Length of wrap_text and text_width must be the same.")
    }
  }
  
  # Check nested columns arguments
  if (!is.null(nest_cols)) {
    if (length(nest_cols) != length(nest_cols_rename)) {
      stop("Length of nest_cols should match the length of nest_cols_rename divided by the length of nest_header.")
    }
    if (length(nest_header) != length(nest_cols_rename)) {
      stop("Length of nest_header must match the number of nested column groups.")
    }
  }
  
  # Construct column alignment string with wrapping if needed
  align_str <- sapply(seq_along(align), function(i) {
    if (!is.null(wrap_text) && i %in% wrap_text) {
      index <- match(i, wrap_text)
      paste0("p{", text_width[index], "em}")
    } else {
      align[i]
    }
  })
  align_str <- paste(align_str, collapse = "")
  
  # Start constructing the LaTeX table
  if (longtable) {
    latex_str <- paste0("\\begin{table}[", position, "]\n\\centering\n")
    if (!is.null(font_size)) {
      latex_str <- paste0(latex_str, "{\\", font_size, " \n")
    }
    if (isTRUE(resize)) {
      warning("Resizing not available in longtable environment. Consider changing font size and wrapping text instead.")
    }
    latex_str <- paste0(latex_str, "\\begin{longtable}{", align_str, "}\n")
  } else {
    latex_str <- paste0("\\begin{table}[", position, "]\n\\centering\n")
    if (!is.null(font_size)) {
      latex_str <- paste0(latex_str, "{\\", font_size, " \n")
    }
    if (isTRUE(resize)) {
      latex_str <- paste0(latex_str, "\\resizebox{\\textwidth}{!}{%\n")
    }
    latex_str <- paste0(latex_str, "\\begin{tabular}{", align_str, "}\n")
  }
  
  # Add borders
  if ("top" %in% borders) {
    latex_str <- paste0(latex_str, "\\toprule\n")
  }
  
  # Add nested headers if specified
  if (!is.null(nest_cols)) {
    num_groups <- length(nest_cols)
    
    # Create the header row and sub-header rows
    header_str <- "& "
    subheader_str <- "& "
    cmidrule_str <- ""
    
    col_count <- 0
    spacer_len <- min(unlist(nest_cols))-1
    header_str <- paste0("\\multicolumn{", spacer_len, "}{c}{} & ")
    
    for (i in seq_len(num_groups)) {
      num_cols <- length(nest_cols[[i]])
      group_header <- nest_header[i]#nest_cols_rename[((i - 1) * length(nest_header)) + 1]
      subheaders <- paste(nest_cols_rename[[i]], collapse = " & ")
      
      if (bold_header) {
        header_str <- paste0(header_str, "\\multicolumn{", num_cols, "}{c}{\\textbf{", group_header, "}} & ")
        subheaders <- paste0("\\textbf{", nest_cols_rename[[i]], "}", collapse = " & ")
      } else {
        header_str <- paste0(header_str, "\\multicolumn{", num_cols, "}{c}{", group_header, "} & ")
        subheaders <- paste(nest_cols_rename[[i]], collapse = " & ")
      }
      
      subheader_str <- paste0(subheader_str, subheaders, " & ")
      cmidrule_str <- paste0(cmidrule_str, "\\cmidrule(lr){", paste0(min(nest_cols[[i]])), "-", paste0(max(nest_cols[[i]])), "} ")
      col_count <- col_count + num_cols
    }
    # Need to add back in the subheaders that do not have multilevel headers and place in correct order
    og_headers <- which(colnames(df) %in% colnames(df)[!1:ncol(df) %in% unlist(nest_cols)])
    
    if (bold_header) {
      og_headers_1 <- paste0("\\textbf{", colnames(df)[which(og_headers < min(unlist(nest_cols)))], "} & ", collapse = "")
      og_headers_2 <- paste0("\\textbf{", colnames(df)[which(og_headers > max(unlist(nest_cols)))], "} & ", collapse = "")
    } else{
      og_headers_1 <- paste0(colnames(df)[which(og_headers < min(unlist(nest_cols)))], " & ", collapse = "")
      og_headers_2 <- paste0(colnames(df)[which(og_headers > max(unlist(nest_cols)))], " & ", collapse = "")
    }
    
    # Remove trailing "&" from og headers and join to subheader
    og_headers_1 <- sub("& $", "", og_headers_1)
    og_headers_2 <- sub("& $", "", og_headers_2)
    
    if (og_headers_2 == "\\textbf{} ") {
      subheader_str <- paste0(og_headers_1, subheader_str)
    } else if (og_headers_1 == "\\textbf{} ") {
      subheader_str <- paste0(subheader_str, og_headers_2)
      } else {
        subheader_str <- paste0(og_headers_1, subheader_str, og_headers_2)
      }
    
    # Remove trailing "&" from header and subheader rows
    header_str <- sub("& $", "", header_str)
    subheader_str <- sub("& $", "", subheader_str)
    cmidrule_str <- sub("\\ $", "", cmidrule_str)
    
    # Add the headers and sub-headers
    latex_str <- paste0(latex_str, header_str, "\\\\\n")
    latex_str <- paste0(latex_str, cmidrule_str, "\n")
    #latex_str <- paste0(latex_str, "\\cmidrule(lr){", paste0((seq(1, col_count, by = num_groups)), collapse = "}, {"), "}\n")
    latex_str <- paste0(latex_str, subheader_str, "\\\\\n")
  }
  
  # Add column names with formatting if needed
  if (isFALSE(bold_header) & is.null(nest_cols)) {
    col_names <- colnames(df)
    latex_str <- paste0(latex_str, paste(col_names, collapse = " & "), " \\\\ \n")
  }
  if (bold_header & is.null(nest_cols)) {
    col_names <- colnames(df)
    col_names <- paste0("\\textbf{", col_names, "}")
    latex_str <- paste0(latex_str, paste(col_names, collapse = " & "), " \\\\ \n")
  }
  
  if ("mid" %in% borders) {
    latex_str <- paste0(latex_str, "\\midrule\n")
  }
  
  # Add rows with formatting if needed
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    row <- sapply(seq_along(row), function(j) {
      value <- as.character(row[j])
      if (!is.null(format_rows) && i %in% format_rows && !is.null(format_cols) && j %in% format_cols) {
        if ("bold" %in% format_style) {
          value <- paste0("\\textbf{", value, "}")
        }
        if ("italics" %in% format_style) {
          value <- paste0("\\textit{", value, "}")
        }
      }
      if (!is.null(wrap_text) && j %in% wrap_text) {
        value <- gsub(" ", "\\\\ ", value) # Handle line breaks
      }
      value
    })
    if (add_row_numbers) {
      latex_str <- paste0(latex_str, i, " & ", paste(row, collapse = " & "), " \\\\ \n")
    } else {
      latex_str <- paste0(latex_str, paste(row, collapse = " & "), " \\\\ \n")
    }
    latex_str <- paste0(latex_str, row_spacing, "\n")
  }
  
  # Close the tabular environment
  if ("bottom" %in% borders) {
    latex_str <- paste0(latex_str, "\\bottomrule\n")
  }
  
  if (longtable) {
    latex_str <- paste0(latex_str, "\\end{longtable}\n")
    if (!is.null(font_size)) {
      latex_str <- paste0(latex_str, "}\n")
    }
  } else {
    latex_str <- paste0(latex_str, "\\end{tabular}\n")
    if (isTRUE(resize)) {
      latex_str <- paste0(latex_str, "}\n")
    }
    if (!is.null(font_size)) {
      latex_str <- paste0(latex_str, "}\n")
    }
  }
  
  # Add caption and label
  latex_str <- paste0(latex_str, "\\caption{", caption, "}\n")
  latex_str <- paste0(latex_str, "\\label{", label, "}\n")
  
  # Add footnotes if provided
  if (!is.null(footnotes)) {
    footnotes_str <- paste0("\\footnotesize\n", paste(footnotes, collapse = "\n"))
    latex_str <- paste0(latex_str, "\\end{table}\n", footnotes_str)
  } else {
    latex_str <- paste0(latex_str, "\\end{table}\n")
  }
  
  # Save to file if file_path is provided
  if (!is.null(file_path)) {
    writeLines(latex_str, file_path)
  }
  
  # Print the LaTeX string
  cat(latex_str)
}

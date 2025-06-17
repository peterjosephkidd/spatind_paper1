#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               4ai. Time Lags
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script uses the Cross Correlation Function to identify lags between 
#> the spatial indicator and SSB. This lag will then be used to "correct" the 
#> spatial indicator time series before testing their ability to detect stock
#> status vis ROC curves.

library(dplyr)
library(icesVocab)
library(readxl)
library(ggplot2) 

rm(list = ls())

#suppressWarnings(dir.create(paste0(getwd(), "/Data/Generated/DR_Stocks/SSB_and_spatinds/"), recursive = T))

# 1. Load Data ####
sa_data  <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-69stks-AY2022-SA-data.xlsx")) # Stock Assessment Data 
load(paste0(getwd(), "/Output/Data/SpatInds/spatinds_4.rds")) # Spatial Indicator Data
load(paste0(getwd(), "/Data/Generated/DR_Stocks/StockNames/stk_names_3a.rds")) # stk_names from data_3a_Mature.R

suppressWarnings(dir.create(paste0(getwd(), "/Output/Data/CCF/"), recursive = T))
suppressWarnings(dir.create(paste0(getwd(), "/Output/Plots/CCF/"), recursive = T))

# 2. Prepare Data ####
# Check we are not missing any stocks
identical(stk_names, unique(spatinds$StockKeyLabel))
all(stk_names %in% sa_data$StockKeyLabel)

# Combine SSB, MSY Btrigger, and Spatinds data
ssb.ref <- sa_data %>% 
  select(StockKeyLabel, Year, SSB) %>%
  filter(StockKeyLabel %in% stk_names) %>%
  full_join(., spatinds, by = c("StockKeyLabel", "Year")) %>%
  mutate(SurveyNameIndex = paste0(SurveyName, ", ", SurveyIndex)) %>%
  filter(SurveyNameIndex != "NA, NA")

all(unique(spatinds$StockKeyLabel) %in% unique(ssb.ref$StockKeyLabel))

# Indicators
inds <- c(
  "CoG (x)", "CoG (y)",       # Location
  "Inertia", "EOO", "ELA",   # Dispersion
  "POPR", "POPH",            # Occupancy
  "Gini Index", "D95", "SA", # Aggregation 
  "EA", "SPI")

# Long format
ssb.ref.long <- tidyr::pivot_longer(ssb.ref, cols = all_of(inds), names_to = "Indicator", values_to = "Value") %>%
  select(L50lvl, StockKeyLabel, SurveyName, SurveyIndex, Quarter, Year, SSB, Indicator, Value)

loc <- c("CoG (x)", "CoG (y)")                   # Location
ran <- c("Inertia", "EOO", "ELA")                # Dispersion
occ <- c( "POPR", "POPH")                        # Occupancy
agg <- c("Gini Index", "D95", "SA", "EA", "SPI") # Aggregation

ssb.ref.long <- ssb.ref.long %>%
  mutate(ind_category = case_when(
    Indicator %in% loc ~ "Location",
    Indicator %in% ran ~ "Dispersion",
    Indicator %in% occ ~ "Occupancy",
    Indicator %in% agg ~ "Aggregation",
    TRUE ~ NA_character_
  ))

ssb.ref.long$Indicator    <- factor(ssb.ref.long$Indicator, levels = inds)
ssb.ref.long$ind_category <- factor(ssb.ref.long$ind_category, levels = c("Location", "Dispersion", "Occupancy", "Aggregation"))

# 3. CCF ####
## 3.1 Compute CCF ####
ccf.df <- ssb.ref.long %>%
  group_by(L50lvl, StockKeyLabel, SurveyName, SurveyIndex, Quarter, Indicator, ind_category) %>%
  select(Year, SSB, Value) %>%
  na.omit() %>%
  arrange(Year) %>%
  tidyr::nest() %>%
  mutate(CCF_res = purrr::map(data, ~{
    bio <- .x$SSB
    spatind <- .x$Value
    stats::ccf(bio, spatind, plot = FALSE)
  }))

## 3.2 Confidence limits ####
ccf.df <- ccf.df %>%
  mutate(n_data_rows = purrr::map_int(data, ~ nrow(.x)),
         conf_limit = 1.96 / sqrt(n_data_rows)) # 95% CI

save(ccf.df, file = paste0(paste0(getwd(), "/Output/Data/CCF/ccf_rawdf.rds")))

## 3.3. Identify best lag ####
ccf_summary <- ccf.df %>%
  mutate(
    max_idx = purrr::map_int(CCF_res, ~{
      tryCatch({
        acf_vals <- .x$acf
        if (is.null(acf_vals) || length(acf_vals) == 0) NA_integer_
        else {
          idx <- which.max(abs(acf_vals))
          if (length(idx) == 0) NA_integer_ else idx
        }
      }, error = function(e) NA_integer_)
    }),
    
    best_lag = purrr::map2_dbl(CCF_res, max_idx, ~{
      tryCatch({
        if (!is.null(.x) && !is.na(.y) && length(.x$lag) >= .y) .x$lag[.y]
        else NA_real_
      }, error = function(e) NA_real_)
    }),
    
    best_acf = purrr::map2_dbl(CCF_res, max_idx, ~{
      tryCatch({
        if (!is.null(.x) && !is.na(.y) && length(.x$acf) >= .y) .x$acf[.y]
        else NA_real_
      }, error = function(e) NA_real_)
    }),
    signal = case_when(
      best_acf > conf_limit ~ "signal",
      best_acf < -conf_limit ~ "signal",
      TRUE ~ "noise")
  ) %>%
  select(L50lvl, StockKeyLabel, SurveyName, SurveyIndex, Quarter, Indicator, ind_category, n_data_rows, conf_limit, best_lag, best_acf, signal)

ccf_summary$Analysis <- paste0(ccf_summary$StockKeyLabel, ": ", ccf_summary$SurveyName, ", Qr ", ccf_summary$Quarter, ", Index = ", ccf_summary$SurveyIndex)

save(ccf_summary, file = paste0(paste0(getwd(), "/Output/Data/CCF/ccf_summary.rds")))

### 3.3.1 Explore NAs ####
na_cases <- ccf_summary %>%
  filter(is.na(best_lag) | is.na(best_acf))

#### Case 1
problem_case1 <- ccf.df %>%
  filter(
    StockKeyLabel == "her.27.20-24",
    SurveyName == "NS-IBTS",
    SurveyIndex == "Q1",
    Quarter == "1",
    Indicator == "POPR"
    #L50lvl == "mean"
  )

print(problem_case1$data[[1]], n=50) # mean L50
print(problem_case1$data[[2]], n=50) # lower
print(problem_case1$data[[3]], n=50) # upper
problem_case1$CCF_res

#### Case 2
problem_case2 <- ccf.df %>%
  filter(
    StockKeyLabel == "sol.27.8ab",
    SurveyName == "BTS-VIII",
    SurveyIndex == "Q4",
    Quarter == "4",
    Indicator == "POPR"
  )

print(problem_case2$data[[1]], n=50) # mean L50
print(problem_case2$data[[2]], n=50) # lower
print(problem_case2$data[[3]], n=50) # upper
problem_case2$CCF_res

# POPR = 1 across time series, no variation:
ccf.df %>%
  mutate(
    has_variation = purrr::map_lgl(data, ~{
      x <- .x$Value
      length(unique(x[!is.na(x)])) > 1
    })
  ) %>%
  filter(!has_variation)


## 3.4. Quick plot ####
### 3.4.1 Individaul Case ####
ccfplot <- ccf.df %>%
  filter(L50lvl == "mean",
         StockKeyLabel == "ple.27.420",
         SurveyName =="NS-IBTS",
         Quarter == "3",
         Indicator == "POPR")

ccfvec <- ccfplot$CCF_res[[1]]

ccfplotdf <- data.frame(
  Lag = as.numeric(ccfvec$lag),
  CCF = as.numeric(ccfvec$acf), 
  conf_limit = ccfplot$conf_limit
)

ggplot(ccfplotdf, aes(x = Lag, y = CCF)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = ccfplot$conf_limit, colour = "black", lty = 2) +
  geom_hline(yintercept = 0-ccfplot$conf_limit, colour = "black", lty = 2) +
  labs(
    title = "Cross-Correlation Function (CCF)",
    x = "Lag (Years)",
    y = "Cross-correlation"
  ) +
  theme_minimal()

## 3.5 Long data ####
ccf_long <- ccf.df %>%
  mutate(
    ccf_tidy = purrr::map(CCF_res, ~ {
      data.frame(
        Lag = as.numeric(.x$lag),
        CCF = as.numeric(.x$acf)
      )
    })
  ) %>%
  tidyr::unnest(cols = c(ccf_tidy)) %>%
  mutate(
    signal = case_when(
      CCF > conf_limit ~ "signal",
      CCF < -conf_limit ~ "signal",
      TRUE ~ "noise"
    )
  )

ccf_long <- ccf_long %>%
  mutate(Indicator = forcats::fct_recode(Indicator, "Gini" = "Gini Index"))

ccf_long$Analysis <- paste0(ccf_long$StockKeyLabel, ": ", ccf_long$SurveyName, ", Qr ", ccf_long$Quarter, ", Index = ", ccf_long$SurveyIndex)

save(ccf_long, file = paste0(paste0(getwd(), "/Output/Data/CCF/ccf_long.rds")))

# 4. L50 Sensitivity test ####
# Are the best time lags different between L50 conditions?
ccf_filtered <- ccf_long %>%
  group_by(L50lvl, StockKeyLabel, SurveyIndex, SurveyName, Quarter,
           Indicator, ind_category, Analysis) %>%
  mutate(best_lag = if_else(row_number(desc(abs(CCF))) == 1, "best", "x")) %>%
  ungroup() %>%
  filter(best_lag == "best",
         !Indicator %in% c("CoG (x)", "CoG (y)"))

ccf_filtered %>%
  count(L50lvl)

## 4.1. Kruskal ####
kruskal_lag <- kruskal.test(Lag ~ L50lvl, data = ccf_filtered)      # Lag comparison
kruskal_ccf <- kruskal.test(CCF ~ L50lvl, data = ccf_filtered)      # Correlation strength comparison

kruskal_overall_results <- bind_rows(
  tibble(
    variable = "Lag",
    statistic = kruskal_lag$statistic,
    df = kruskal_lag$parameter,
    p_value = kruskal_lag$p.value,
    method = kruskal_lag$method,
    data = kruskal_lag$data.name
  ),
  tibble(
    variable = "CCF",
    statistic = kruskal_ccf$statistic,
    df = kruskal_ccf$parameter,
    p_value = kruskal_ccf$p.value,
    method = kruskal_ccf$method,
    data = kruskal_ccf$data.name
  )
)

print(kruskal_overall_results)
save(kruskal_overall_results, file = paste0(paste0(getwd(), "/Output/Data/CCF/ccf_kruskal_L50.rds")))

## 4.2 Wilcoxon ####
# Are the best time lags different between L50 conditions
# Wilcoxon test comparing each combination of L50lvls
get_wilcox_result <- function(df) {
  test <- wilcox.test(Lag ~ L50lvl, data = df)
  tibble(
    statistic = test$statistic,
    p_value = test$p.value,
    method = test$method,
    data = test$data.name
  )
}

wilcox_results <- bind_rows(
  get_wilcox_result(filter(ccf_filtered, L50lvl %in% c("mean", "lowerCI"))) %>% mutate(comparison = "mean vs lowerCI"),
  get_wilcox_result(filter(ccf_filtered, L50lvl %in% c("mean", "upperCI"))) %>% mutate(comparison = "mean vs upperCI"),
  get_wilcox_result(filter(ccf_filtered, L50lvl %in% c("lowerCI", "upperCI"))) %>% mutate(comparison = "lowerCI vs upperCI")
)

print(wilcox_results)
save(wilcox_results, file = paste0(paste0(getwd(), "/Output/Data/CCF/ccf_wilcoxon_L50.rds")))

# 5. Plots ####
## 5.1 Propplot: SSB or Indicator Lead? ####
# When there is signal does the indicator lead or SSB?
propbest <- ccf_filtered %>%
  ungroup() %>%
  filter(signal == "signal", 
         !Indicator %in% c("CoG (x)", "CoG (y)"),
         L50lvl == "mean") %>%
  #filter(Lag < 5, Lag > -5) %>%
  mutate(lag_direction = case_when(
    Lag > 0 ~ "Indicator leads",
    Lag < 0 ~ "SSB leads",
    Lag == 0 ~ "Concurrent"
  ),
  lag_direction = forcats::fct_relevel(lag_direction,"SSB leads" , "Concurrent", "Indicator leads")) %>%
  group_by(ind_category, Indicator) %>%
  count(Indicator, lag_direction) %>%
  mutate(prop = n / sum(n),
         label = scales::percent(prop, accuracy = 1)) %>%
  
  ggplot(aes(x = Indicator, y = prop)) +
  geom_col(position = "stack", colour = "grey30", aes(fill = ind_category, alpha = c(lag_direction))) +
  geom_text(
    aes(label = label, group = lag_direction),
    position = position_stack(vjust = 0.5),
    size = 5,
    colour = "black"
  ) +
  scale_alpha_manual(
    values = c(
      "Indicator leads" = 1,
      "Concurrent" = 0.5,
      "SSB leads" =   0.2
    ),
    name = "Lag Direction"
  ) +
  scale_fill_manual(
    breaks = c("Dispersion", "Occupancy", "Aggregation"),
    values = c("Dispersion" = "#F8766D",
               "Occupancy" = "#00BFC4",
               "Aggregation" = "#7CAE00"),
    name = "Indicator Category"
  ) +
  labs(
    y = "Proportion of Best Lags",
    x = "Indicator",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 15, hjust=1),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        #panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        strip.text         = element_text(size = 20),
        # Legend
        legend.position = "right",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))

propbest

ggsave(paste0(getwd(), "/Output/Plots/CCF/CCF_propplot.png"), propbest, height = 10, width = 14)

## 5.2. Best Lags (signal and noise) ####
bestlag <- ccf_long %>%
  group_by(L50lvl, StockKeyLabel, SurveyIndex, SurveyName, Quarter, Indicator, ind_category, Analysis) %>%
  mutate(best_lag = if_else(row_number(desc(abs(CCF))) == 1, "best", "x")) %>%
  filter(best_lag == "best",
         L50lvl == "mean",
         !Indicator %in% c("CoG (x)", "CoG (y)")) %>%
  select(-c(data, CCF_res, CCF)) %>%
  ggplot(aes(x = Analysis, y = Lag)) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_segment(aes(x = Analysis, xend = Analysis,
                   y = 0, yend = Lag,
                   alpha = signal, colour = signal)) +
  geom_point(aes(fill = ind_category, colour = signal, alpha = signal),
             size = 3, shape = 21, stroke = 0.5) +
  facet_grid(~Indicator) +
  coord_flip() +
  scale_alpha_manual(
    values = c("signal" = 1, "noise" = 0.3),
    labels = c("signal" = "Significant", "noise" = "Noise"),
    name = "Signal Strength"
  ) +
  scale_colour_manual(
    values = c("signal" = "grey30", "noise" = "transparent"),
    labels = c("signal" = "Significant", "noise" = "Noise"),
    name = "Signal Strength2"
  ) +
  scale_fill_manual(
    breaks = c("Dispersion", "Occupancy", "Aggregation"),
    values = c("Dispersion" = "#F8766D",
               "Occupancy" = "#00BFC4",
               "Aggregation" = "#7CAE00"),
    name = "Indicator Category"
  ) +
  labs(
    y = "Lag (Years)",
    x = "Stock & Survey"
  ) +
  guides(colour = "none",
         fill = guide_legend(
           nrow = 1, 
           override.aes = list(size = 5)
           ),
         alpha = guide_legend(
           nrow=1,
           override.aes = list(size = 5)
           )
         ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 12, hjust=1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 20),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        strip.text         = element_text(size = 20),
        # Legend
        legend.position = "bottom",
        legend.justification = c(0,0),
        legend.box.just = "left",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))

bestlag

ggsave(paste0(getwd(), "/Output/Plots/CCF/CCF_bestlag_lollipop.png"), bestlag, height = 20, width = 18)

## 5.3 Boxplot of best lags (signal) ####
inds[8] <- "Gini"
boxp <- ccf_long %>%
  group_by(L50lvl, StockKeyLabel, SurveyIndex, SurveyName, Quarter, Indicator, ind_category, Analysis) %>%
  mutate(best_lag = if_else(row_number(desc(abs(CCF))) == 1, "best", "x")) %>%
  filter(best_lag == "best",
         signal == "signal",
         L50lvl == "mean",
         !Indicator %in% c("CoG (x)", "CoG (y)")) %>%
  
  ggplot(aes(x = factor(Indicator, rev(inds[3:12])), y = Lag, fill = ind_category)) +
  geom_hline(yintercept = 0, colour = "grey30", size = 0.3, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.5, width = 0.8, outlier.shape = 4) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.3, size = 1, color = "black") +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 1)), 
               position = position_nudge(x = 0.3), size = 3, color = "black") +
  scale_fill_manual(
    breaks = c("Dispersion", "Occupancy", "Aggregation"),
    values = c("Dispersion" = "#F8766D",
               "Occupancy" = "#00BFC4",
               "Aggregation" = "#7CAE00"),
    name = "Indicator Category"
  ) +
  labs(
    y = "Lag (Years)",
    x = "Indicator",
  ) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 15, hjust=1),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        strip.text         = element_text(size = 20),
        # Legend
        legend.position = "bottom",
        legend.justification = c(0,0),
        legend.box.just = "left",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))

boxp

ggsave(paste0(getwd(), "/Output/Plots/CCF/CCF_boxplot.png"), boxp, height = 10, width = 14)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               5. Plot ROC curve outputs
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(ggplot2) 
library(readxl)
library(writexl)
library(rfishbase)
library(SPMpriors)
library(FishLife)

rm(list = ls())

load(paste0(getwd(), "/Output/Data/ROC/rocRem_long_meta.rds"))    # ROC data with poor surveys removed & stock info added
load(paste0(getwd(), "/Output/Data/ROC/rocRem_long_surveys.rds")) # 52 sources of survey data
load(paste0(getwd(), "/Output/Data/ROC/rocRem_long_stocks.rds"))  # 25 stocks
load(paste0(getwd(), "/Output/Data/ROC/auc_summary.rds"))
metadata <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Stocks")
source(paste0(getwd(), "/Functions/results_funs.R"))              # user functions 

# Create directories for saving ouptuts
suppressWarnings(dir.create(paste0(getwd(), "/Output/Tables/Supplementary"), recursive = T))
suppressWarnings(dir.create(paste0(getwd(), "/Output/Plots/Supplementary"), recursive = T))

# _________________________________________________________________________ ####

# 1. Preamble ####

# Remove location indicators
rocRem_long_meta <- filter(rocRem_long_meta, !Indicator %in% c("CoG (x)", "CoG (y)"))

# Unlist some vars if they are of class list
for (i in 1:ncol(rocRem_long_meta)) {
  if (is.list(rocRem_long_meta[i])) {
    colnames(rocRem_long_meta[i])
    rocRem_long_meta[i] <- unlist(rocRem_long_meta[i])
  }
}
rm(i)

# Summary of analyses
rocRem_long_sum <- rocRem_long_meta %>% 
  select(StockKeyLabel, FisheriesGuild, SpeciesCommonName, SurveyName, Quarter, SurveyNameIndex) %>% 
  distinct() %>%
  arrange(FisheriesGuild, StockKeyLabel, SpeciesCommonName)
rocRem_long_sum
save(rocRem_long_sum, file = paste0(getwd(), "/Output/Data/ROC/rocRem_long_sum.rds"))

# Order stocks by fisheries guild, k, and survey coverage
stkorder.auc <- auc_summary %>% 
  ungroup() %>%
  select(StockKeyLabel, xaxis, FisheriesGuild, GrowthRateK) %>%
  distinct() %>%
  na.omit() %>%
  arrange(FisheriesGuild, GrowthRateK, StockKeyLabel) %>%
  print(n = nrow(.))

# factorize columns for plotting
auc_summary$xaxis <- factor(auc_summary$xaxis,  levels = stkorder.auc$xaxis)
auc_summary$StockKeyLabel <- factor(auc_summary$StockKeyLabel, levels = unique(stkorder.auc$StockKeyLabel))

# Total number of unique datasets
Nsurvs <- length(unique(as.character(auc_summary$xaxis[!is.na(auc_summary$AUC)]))) # where AUC is available. 

# _________________________________________________________________________ ####
# 2. Tables ####
## T1: Stock Summary ###########################################################
# Information on the stocks analysed in this paper

refpts <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Stocks")
load(paste0(getwd(), "/Data/Generated/DR_Stocks/FishBaseMaturity/FishBase-L50.rds"))

Lmat2 <- Lmat[c(1:3,7,9:10)]

T1_stocksummary2 <- left_join(Lmat2, refpts[c(1:2, 5:6, 8:15)], by = "StockKeyLabel")

T1_stocksummary <- T1_stocksummary2 %>%
  filter(StockKeyLabel %in% stk_names_rem) %>%
  arrange(FisheriesGuild, SizeGuild, StockKeyLabel) %>%
  relocate("Stock ID"        = StockKeyLabel,
           "Species"         = SpeciesCommonName, 
           "Scientific Name" = SpeciesScientificName, 
           "Guild"           = FisheriesGuild, 
           "Size Guild"      = SizeGuild,
           "Trophic Guild"   = TrophicGuild, 
           "Eco Region"      = EcoRegion,
           
           "L50"             = mu.stk,
           "CI (-)"          = lc.stk, 
           "CI (+)"          = uc.stk,
           
           "AY"              = AssessmentYear, 
           "Model"           = ModelName,
           "Data Category"   = DataCategory, 
           "MSY Btrigger"    = MSYBtrigger,
           
           "Expert Group"    = ExpertGroup, 
           "Advice Category" = AdviceCategory, 
           "Assessment Key"  = AssessmentKey 
  ) %>%
  mutate(L50 = round(L50, 2),
         `CI (-)` = round(`CI (-)`, 2),
         `CI (+)` = round(`CI (+)`, 2)) %>%
  select(-c(`Eco Region`, AY))

# Some stocks dont have the assessment model info
missing_am_stocks <- c("tur.27.4", "ldb.27.8c9a", "meg.27.7b-k8abd", "meg.27.8c9a", "whg.27.6a", "dgs.27.nea")
missing_am <- data.frame("Stock ID" = missing_am_stocks, "Model" = c("SAM", "a4a", "a4a", "a4a", "SAM", "Unknown")) %>% rename("Stock ID" = "Stock.ID")
missing_am <- merge(T1_stocksummary, missing_am, by = "Stock ID", suffixes = c(".NA", "")) %>% select(-`Model.NA`)

T1_stocksummary <- T1_stocksummary %>% 
  filter(!`Stock ID` %in% missing_am_stocks) %>%
  rbind(., missing_am) %>%
  arrange(Guild, L50, Species, `Stock ID`)

T1_stocksummary_short <- T1_stocksummary[c("Stock ID", "Species", "Scientific Name", "Guild", "L50", "CI (-)", "CI (+)", "Model", "MSY Btrigger", "Expert Group")]

# Save
save(T1_stocksummary,        file = paste0(getwd(), "/Output/Tables/T1_stocksummary.rds"))
save(T1_stocksummary_short,  file = paste0(getwd(), "/Output/Tables/T1_stocksummary_short.rds"))

# Transform to Latex and save
df_to_latex(T1_stocksummary_short, 
            format_rows = 1:nrow(T1_stocksummary_short),
            format_cols = 3,
            format_style = "italics",
            bold_header = TRUE,
            label = "tab: stocks", 
            caption = "Stocks selected for analysis and other important metrics including length at maturity (L50), assessment year (AY), stock assessment model, MSY B\\textsubscript{trigger} estimate, and the ICES expert group that conducted the assessment.",
            file_path = paste0(getwd(), "/Output/Tables/T1_stocksummary_short.tex"))

## T2: Spatial Indicators ######################################################
# Description of spatial indicators 

T2_inds <- read_xlsx(paste0(getwd(), "/Data/Initial/Other/inds.xlsx"), sheet = "Indicators")
T2_inds <- filter(T2_inds, Characteristic != "Location")

T2_inds$Code <- factor(T2_inds$Code, level = c("I", "EOO", "ELA", "POPR", "POPH", "G", "D95", "SA", "EA", "SPI"))
T2_inds      <- arrange(T2_inds, Code)
T2_inds$Code <- as.character(T2_inds$Code)
T2_inds$Description         <- stringr::str_replace_all(T2_inds$Description, "%", "\\\\%")
T2_inds$`Spatial Indicator` <- stringr::str_replace_all(T2_inds$`Spatial Indicator`, "%", "\\\\%")

# Transform to Latex and save
df_to_latex(T2_inds, 
            bold_header = TRUE,
            font_size = "small",
            resize = TRUE,
            wrap_text = c(1,4), 
            text_width = c(10, 18),
            label = "tab: inds", 
            caption = "Description of the ten spatial indicators that are compared to stock assessment outputs.",
            file_path = paste0(getwd(), "/Output/Tables/T2_inds.tex")
)


## S1: All AUC & TSS Values ####################################################
# Find highest TSS and corresponding spatial indicator value
t.auctss <- rocRem_long_meta %>%
  group_by(Quarter, StockKeyLabel, SurveyNameIndex, Indicator, L50lvl) %>%
  summarise(OptIndVal = max(Value),
            OptTSS = max(TSS), 
            AUC = unique(AUC),
            Years  = paste0(min(Year[Year!=min(Year)]), "-", max(Year)), 
            K = unique(GrowthRateK),
            Survey = unique(SurveyName)) %>%
  relocate(StockKeyLabel, Survey, Years, Quarter) %>%
  tidyr::pivot_wider(names_from = L50lvl, values_from = c(AUC, OptTSS, OptIndVal)) %>%
  as.data.frame() %>%
  relocate(StockKeyLabel, Survey, Quarter, Years, K, Indicator,
           AUC_mean, OptTSS_mean, OptIndVal_mean,
           AUC_lowerCI, OptTSS_lowerCI, OptIndVal_lowerCI,
           AUC_upperCI, OptTSS_upperCI, OptIndVal_upperCI) %>%
  arrange(StockKeyLabel, Indicator, Survey) %>%
  ungroup() %>%
  select(-SurveyNameIndex) %>%
  mutate(OptIndVal_mean    = as.numeric(OptIndVal_mean),
         OptIndVal_lowerCI = as.numeric(OptIndVal_lowerCI),
         OptIndVal_upperCI = as.numeric(OptIndVal_upperCI)) %>%
  mutate_if(is.numeric, ~round(., 3)) %>%
  rename(`Stock ID` = StockKeyLabel, Qr = Quarter)

t.auctss$Indicator <- gsub("Gini Index", "G", t.auctss$Indicator)
t.auctss$Indicator <- gsub("Inertia"   , "I", t.auctss$Indicator)

t.auctss

# Save
save(t.auctss, file = paste0(getwd(), "/Output/Tables/Supplementary/S1-AUC-TSS_AllStocks.rds"))  

## S2: Individual AUC & TSS Values ##############################################
# Create Supplementary Tables in Latex format 
# This creates an individual tex file for each stock which can be uploaded to overleaf
# Or you can copy the outputs from the console
t.auctss2 <- select(t.auctss, -c(K)) 

for (i in stk_names_rem) {
  writeLines("#################################################################")
  message(i)
  writeLines("\n")
  
  df <- filter(t.auctss2, `Stock ID` == i)
  
  save(df, file = paste0(getwd(), "/Output/Tables/Supplementary/S2-AUC-TSS-", i, ".rds"))
  
  # Transform to Latex and save
  df_to_latex(df,
              bold_header = TRUE,
              font_size = "Huge",
              longtable = FALSE,
              resize = TRUE,
              position = "!htb",
              nest_cols = list(c(6, 7, 8), c(9, 10, 11), c(12, 13, 14)),
              nest_cols_rename = list(c("AUC", "TSS", "Thresh"), c("AUC", "TSS", "Thresh"), c("AUC", "TSS", "Thresh")),
              nest_header = c("Mean", "Lower CI", "Upper CI"),
              wrap_text = c(1,5), 
              text_width = c(6,5),
              label = "sup: auc_l50", 
              caption = paste0("ROC performance metrics for ", i, " across L50 sensitivity tests."),
              file_path = paste0(getwd(), "/Output/Tables/Supplementary/S2-AUC-TSS-", i, ".tex"))
  writeLines("\n")
}

# Copy this into tex file to place supp tables in same order as stocks in Table 1 in methods
writeLines(paste0(paste0("\\input{suppmaterial/tables/S2-AUC-TSS-", stk_names_rem, ".tex}"), collapse = "\n"))

## T3: AUC bins ################################################################
# The number and percentage of analyses in specific bin range of AUC

T3_AUC_bins <- auc_summary %>% 
  filter(L50lvl == "mean",
         ind_category != "Location") %>%
  group_by(StockKeyLabel, SurveyNameIndex, Indicator, Quarter, xaxis) %>%
  mutate(AUClvl = if_else(all(AUC >= 0.8), "Best", 
                  if_else(all(AUC >= 0.6 & AUC <  0.8), "Good", 
                  if_else(all(AUC >  0.4 & AUC <  0.6), "Random", 
                  if_else(all(AUC >  0.2 & AUC <= 0.4), "Bad", 
                  if_else(all(AUC <= 0.2), "Worst", "Inconsistent")))))) %>%
  ungroup() %>%
  select(-AUC) %>%
  distinct() %>%
  group_by(`ind_category`, Indicator) %>%
  summarise(`1.0 $>=$ AUC $>=$ 0.8` =    paste0( format(round(length(AUClvl[AUClvl=="Best"  ])/.52,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Best"  ])), ")"),
            `0.8 $>$  AUC $>=$ 0.6` =    paste0( format(round(length(AUClvl[AUClvl=="Good"  ])/.52,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Good"  ])), ")"),
            `0.6 $>$  AUC $>$ 0.4`  =    paste0( format(round(length(AUClvl[AUClvl=="Random"])/.52,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Random"])), ")"),
            `0.4 $>=$ AUC $>$ 0.2`  =    paste0( format(round(length(AUClvl[AUClvl=="Bad"   ])/.52,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Bad"   ])), ")"),
            `0.2 $>=$ AUC $>=$ 0`   =    paste0( format(round(length(AUClvl[AUClvl=="Worst" ])/.52,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Worst" ])), ")"), .groups = "keep")

# Some formatting
T3_AUC_bins$Indicator <- gsub("Gini Index", "G", T3_AUC_bins$Indicator)
T3_AUC_bins$Indicator <- gsub("Inertia", "I", T3_AUC_bins$Indicator)
T3_AUC_bins$Indicator <- factor(T3_AUC_bins$Indicator, levels = c("I", "EOO", "ELA", "POPR", "POPH", "G", "D95", "SA", "EA", "SPI"))

T3_AUC_bins <- T3_AUC_bins %>%
  arrange(Indicator) %>%
  rename(`Indicator Category` = ind_category)

# Add column sums 
colsums <- auc_summary %>% 
  filter(L50lvl == "mean",
         ind_category != "Location") %>%
  group_by(StockKeyLabel, SurveyNameIndex, Indicator, Quarter, xaxis) %>%
  mutate(AUClvl = if_else(all(AUC >= 0.8), "Best", 
                  if_else(all(AUC >= 0.6 & AUC <  0.8), "Good", 
                  if_else(all(AUC >  0.4 & AUC <  0.6), "Random", 
                  if_else(all(AUC >  0.2 & AUC <= 0.4), "Bad", 
                  if_else(all(AUC <= 0.2), "Worst", "Inconsistent")))))) %>%
  ungroup() %>%
  select(-AUC) %>%
  distinct() %>%
  summarise(`1.0 $>=$ AUC $>=$ 0.8` =    paste0( format(round(length(AUClvl[AUClvl=="Best"  ])/5.20,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Best"  ])), ")"),
            `0.8 $>$  AUC $>=$ 0.6` =    paste0( format(round(length(AUClvl[AUClvl=="Good"  ])/5.20,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Good"  ])), ")"),
            `0.6 $>$  AUC $>$ 0.4`  =    paste0( format(round(length(AUClvl[AUClvl=="Random"])/5.20,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Random"])), ")"),
            `0.4 $>=$ AUC $>$ 0.2`  =    paste0( format(round(length(AUClvl[AUClvl=="Bad"   ])/5.20,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Bad"   ])), ")"),
            `0.2 $>=$ AUC $>=$ 0`   =    paste0( format(round(length(AUClvl[AUClvl=="Worst" ])/5.20,2), nsmall = 2), "% (", (length(AUClvl[AUClvl=="Worst" ])), ")"), .groups = "keep")

colsums$`Indicator Category` <- ""
colsums$Indicator <- ""

T3_AUC_bins <- rbind(T3_AUC_bins, colsums)

# Add Median
stats <- auc_summary %>% 
  filter(L50lvl == "mean",
         ind_category != "Location") %>%
  group_by(Indicator) %>%
  summarise(M = format(round(median(AUC), 3), nsmall = 3))
stats$Indicator <- gsub("Gini Index", "G", stats$Indicator)
stats$Indicator <- gsub("Inertia", "I", stats$Indicator)
stats <- rbind(stats, "")
T3_AUC_bins <- left_join(T3_AUC_bins, stats)

save(T3_AUC_bins, file = paste0(getwd(), "/Output/Tables/T3_AUC_bins.rds"))  

# Remove factoring for Latex
T3_AUC_bins_tex <- T3_AUC_bins
T3_AUC_bins_tex$`Indicator Category` <- as.character(T3_AUC_bins_tex$`Indicator Category`)
T3_AUC_bins_tex$Indicator <- as.character(T3_AUC_bins_tex$Indicator)

# Add backslash to %
T3_AUC_bins_tex <- T3_AUC_bins_tex %>%
  mutate(across(where(is.character), ~ gsub("%", "\\%", ., fixed = TRUE)))

# Transform to Latex and save
df_to_latex(T3_AUC_bins_tex, 
            bold_header = TRUE,
            font_size = "footnotesize",
            resize = TRUE,
            borders = c("top", "mid", "penult", "bottom"),
            wrap_text  = c(1,2), 
            text_width = c(5,4),
            nest_cols = list(c(3:7)),
            align = c("l","l","l","l","l","l","l", "l"),
            nest_header = "AUC Category",
            nest_cols_rename = list(c("++", "+", "", "-", "- -")),
            position = "!t",
            label = "tab: auc_hist", 
            caption = "The frequency of AUC outputs within performance categories and median AUC for each spatial indicator. The AUC Category defines bins for categorizing the performance of indicators based on AUC: ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6). M = median AUC." ,
            file_path = paste0(getwd(), "/Output/Tables/T3_AUC_bins.tex"))

## S3: Summary Stats ############################################################
# This table calculates some sumamry statistics of the AUC values

# Create columns to put AUC values in bins
bindata <- create_auc_bindata(auc_summary, bins = seq(0,1, by = 0.1))
bindata <- filter(bindata, ind_category != "Location")
### Stats
# Calculate mean and variance in AUCs for indicator for each L50 method
S3_stats <- bindata %>%
  group_by_at(vars(all_of(c("Indicator", "L50lvl")))) %>%
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

S3_stats$Indicator <- gsub("Gini Index", "G", S3_stats$Indicator)
S3_stats$Indicator <- gsub("Inertia", "I", S3_stats$Indicator)
S3_stats$Indicator <- factor(S3_stats$Indicator, levels = c("I", "EOO", "ELA", "POPR", "POPH", "G", "D95", "SA", "EA", "SPI"))
S3_stats$L50lvl <- factor(S3_stats$L50lvl, levels = c("mean", "lowerCI", "upperCI"), labels = c("Mean", "Lower CI", "Upper CI"))
S3_stats[5:12] <- round(S3_stats[5:12], 3)
S3_stats$cond <- ifelse(is.na(S3_stats$cond), "", S3_stats$cond)

S3_stats <- S3_stats %>%
  arrange(Indicator, L50lvl) %>%
  rename(`L50 Test` = L50lvl, `Indicator Category` = ind_category, 
         `Mean AUC` = mu, SD = stndrd_dev, SE = se, 
         `Lower CI (95\\%)` = lower_ci, `Upper CI (95\\%)` = upper_ci, 
         `Median AUC` = median_auc, `AUC Category` = cond) %>%
  select(-CI95_margin)

S3_stats <- S3_stats %>%
  mutate(across(where(is.numeric), ~ format(., nsmall = 2)))

print(S3_stats, n=30)

save(S3_stats, file = paste0(getwd(), "/Output/Tables/Supplementary/S3_stats.rds"))

# LaTex table
S3_stats$Indicator <- as.character(S3_stats$Indicator)
S3_stats$`L50 Test` <- as.character(S3_stats$`L50 Test`)
S3_stats$`Indicator Category` <- as.character(S3_stats$`Indicator Category`)

df_to_latex(S3_stats, 
            bold_header = TRUE,
            font_size = "small",
            resize = TRUE,
            wrap_text  = c(1,3,5,9,10,11,12), 
            text_width = c(5,5,5,5,5 ,5 ,5),
            label = "tab: ind_stats", 
            caption = "Summary statistics of AUC outputs for each spatial indicator under each L50 sensitivity test. n = the number of contributing AUC values to the mean AUC; SS = sum of squares; SD = standard deviation; SE = standard error; CI = confidence interval. AUC Category refers to the performance category that the mean AUC falls within; ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6)." ,
            file_path = paste0(getwd(), "/Output/Tables/Supplementary/S3_stats.tex"))

## S4: AUC Category Counts ######################################################
# This table counts occurrences in each AUC performance category at the survey level

auc_catcount <- auc_summary %>%
  filter(L50lvl == "mean", ind_category != "Location") %>%
  mutate(NGG = ifelse(AUC >= 0.8, 1, 0),
         NG = ifelse(AUC >= 0.6 & AUC < 0.8, 1, 0),
         NR = ifelse(AUC < 0.6 & AUC > 0.4, 1, 0),
         NB = ifelse(AUC <= 0.4 & AUC > 0.2, 1, 0),
         NBB = ifelse(AUC <= 0.2, 1, 0)) %>%
  group_by(SpeciesCommonName,
           StockKeyLabel,
           SurveyName, 
           Quarter
           ) %>%
  summarise(`++` = sum(NGG),
            `+` = sum(NG),
            ` ` = sum(NR),
            `-` = sum(NB),
            `- -` = sum(NBB)) %>%
  arrange(StockKeyLabel, 
          SurveyName
          ) %>%
  ungroup() %>%
  bind_rows(
    summarise(., 
              SpeciesCommonName = "", 
              StockKeyLabel = "", 
              SurveyName = "", 
              Quarter = "",
              `++` = sum(`++`), 
              `+` = sum(`+`), 
              ` ` = sum(` `), 
              `-` = sum(`-`), 
              `- -` = sum(`- -`))) %>%
  rename(
    Species = SpeciesCommonName,
    `Stock ID` = StockKeyLabel,
    Survey = SurveyName
  ) %>%
  print(., n = nrow(.))

save(auc_catcount, file = paste0(getwd(), "/Output/Tables/Supplementary/S4-AUC-catcount.rds"))  

# LaTex table
df_to_latex(auc_catcount, 
            bold_header = TRUE,
            font_size   = "footnotesize",
            resize      = TRUE,
            wrap_text   = c(1,2), 
            text_width  = c(12,12),
            nest_cols   = list(c(5:9)),
            nest_header = "AUC Category",
            nest_cols_rename = list(c("++", "+", "", "-", "- -")),
            align = c("l","l","l","l", "c","c","c","c","c"),
            label = "tab: auc_catcount", 
            caption = "The counts of indicators within each AUC category for each combination of stock and survey AUC Category refers to the performance category that the mean AUC falls within; ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6).",
            file_path =  paste0(getwd(), "/Output/Tables/Supplementary/S4-AUC-catcount.tex")
)

## T4: Regression Tree Rules #######################################################
load(paste0(getwd(), "/Output/Data/RegTree/RegRules.rds"))
rule_tbl_l$SplitCriteria <- gsub(",", ", ", rule_tbl_l$SplitCriteria)
rule_tbl_l <- rule_tbl_l %>%
  rename("Branch No." = "SplitNo",
         "Splitting Feature" = "SplitVar",
         "Splitting Criteria" = "SplitCriteria") %>%
  mutate(`Splitting Feature` = ifelse(`Splitting Feature` == "StockKeyLabel", "Stock ID", `Splitting Feature`),
         `Splitting Feature` = ifelse(`Splitting Feature` == "SurveyName", "Survey", `Splitting Feature`),
         `Splitting Feature` = ifelse(`Splitting Feature` == "ind_category", "Indicator Category", `Splitting Feature`),
         `Splitting Feature` = ifelse(`Splitting Feature` == "SpeciesScientificName", "Species", `Splitting Feature`),
         `Branch No.` = ifelse(as.numeric(`Branch No.`) %in% c(7,8,4,11,12,15,16,14,6), paste0(`Branch No.`, "*"), `Branch No.`))

Path <- c("", # 1
  "", # 2
  "1",# 3
  "1",# 4
  "2",# 5
  "2",# 6
  "1, 3",# 7
  "1, 3",# 8
  "2, 5",# 9
  "2, 5",# 10
  "2, 5, 9",# 11
  "2, 5, 9",# 12
  "2, 5, 10",# 13
  "2, 5, 10",# 14 
  "2, 5, 10, 13",# 15
  "2, 5, 10, 13"# 16
  )

rule_tbl_l$Path <- Path
rule_tbl_l <- select(rule_tbl_l, `Branch No.`, `Path`, `Splitting Feature`, `Splitting Criteria`)

df_to_latex(rule_tbl_l,
            bold_header  = TRUE,
            font_size    = "footnotesize",
            resize       = TRUE,
            wrap_text    = c(1, 4),
            text_width   = c(3, 25),
            format_rows  = c(5,6),
            format_cols  = c(4),
            format_style = "italics",
            position     = "!t", 
            label = "tab: regrules",
            caption = "The splitting criteria at each split in the regression tree. Row numbers with * are terminal nodes in the regression tree. Path represents the splits that have occurred prior to the current split.",
            file_path =  paste0(getwd(), "/Output/Tables/T4_RegRules.tex")
)

# _________________________________________________________________________ ####

# 3. Plots ####
## AUC Histogram ###############################################################
# Plot histogram of AUC bins for L50 Mean test
ind_stats <- bindata %>%
  group_by_at(vars(all_of(c("Indicator", "L50lvl")))) %>%
  filter(ind_category != "Location") %>%
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

bin_freq <- table(bindata[["Indicator"]], bindata[["AUC_bin"]], bindata[["L50lvl"]]) %>%
  as.data.frame(.) %>%
  rename(Indicator = Var1,
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

indorder <- c(
  "Inertia", "EOO", "ELA",   # Dispersion
  "POPR", "POPH",            # Occupancy
  "Gini Index", "D95", "SA", # Aggregation 
  "EA", "SPI")

ran <- c("Inertia", "EOO", "ELA")                # Dispersion
occ <- c( "POPR", "POPH")                        # Occupancy
agg <- c("Gini Index", "D95", "SA", "EA", "SPI") # Aggregation

bin_freq <- bin_freq %>%
  mutate(ind_category = case_when(
    Indicator %in% ran ~ "Dispersion",
    Indicator %in% occ ~ "Occupancy",
    Indicator %in% agg ~ "Aggregation",
    TRUE ~ NA_character_
  ))

ind_stats <- ind_stats %>%
  tidyr::pivot_longer(cols = c(n, mu, SS, stndrd_dev, se, CI95_margin, lower_ci, upper_ci, median_auc),
                      names_to = "SummaryStat",
                      values_to = "Value")
L50_test <- "Mean"

histplot <-  ggplot() +
    geom_col(data = bin_freq[bin_freq$L50lvl == L50_test,], 
             aes(x = AUC_bin/10, y = Freq,
                 fill = factor(ind_category, levels = c("Dispersion", "Occupancy", "Aggregation"))),
             colour = "black", 
             just = 1,
             width = 0.1, 
             alpha = 0.4) +
    scale_fill_brewer(palette = "Set2") +
    geom_vline(data = ind_stats[ind_stats$SummaryStat %in% c("mu", "median_auc") & ind_stats$L50lvl == "mean",], 
               aes(xintercept = Value, linetype = factor(SummaryStat, labels = c("Median", "Mean"))), linewidth = 1, colour = c("grey10")) +
    labs(x = "AUC Bin", y = "Frequency") +
    facet_wrap(vars(factor(Indicator, levels = c(indorder))), scales = "free_x") +
    scale_x_continuous(breaks = seq(0,1, by=.1), expand = c(0,0.05)) +
    scale_y_continuous(breaks = seq(0,20, by = 2), expand = c(0,0)) +
    theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          # Panels
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.background   = element_blank(),
          panel.border       = element_rect(colour = "black", fill = NA),
          strip.background   = element_rect(colour = "black"),
          strip.text         = element_text(size = 20),
          # Legend
          legend.position = "right",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
    labs(linetype = "Central Tendency",
         fill = "Indicator Category") +
    coord_cartesian(ylim = c(0,17))
histplot
ggsave(paste0(getwd(), "/Output/Plots/Supplementary/AUC-histplot-mean.png"), histplot, height = 10, width = 14)

## ROC curves ##################################################################
# Plot all ROC curves for each indicator for L50 mean condition
ROC_plot <-ggplot(data= filter(rocRem_long_meta, 
                               L50lvl == "mean")) +
  geom_path(aes(x = FPR, y = TPR, colour = AUC), alpha = 1) +
  scale_colour_continuous(type = "viridis") +
  geom_abline(intercept = c(0,0), linetype = 2, colour = "black") +
  facet_wrap(vars(factor(Indicator, levels = c(indorder))), scale = "free_x") +
  labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", 
       #title = paste0("ROC Curves for each spatial indicator")
  ) +
  theme(axis.text.x = element_text(size = 15, hjust=1),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        strip.text         = element_text(size = 20),
        # Legend
        legend.position = "right",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))
ROC_plot

ggsave(paste0(getwd(), "/Output/Plots/ROC-curves.png"), ROC_plot, height = 10, width = 14)

## Heatmap #####################################################################
# This plot provides a visual summary of individuals AUCs, and orders the 
# x axis by the leaves/groups identified in the regression tree

load(paste0(getwd(), "/Output/Data/RegTree/stockleaves.rds"))

group_vars <- c("Indicator", 
                "FisheriesGuild", 
                "SpeciesCommonName", 
                "StockKeyLabel", 
                "SurveyName", 
                "Quarter")

marks <- c("Bins:",
           "++  (AUC >= 0.8)",
           "+    (0.6 <= AUC < 0.8)",
           "-     (0.2 < AUC <= 0.6)",
           "--    (AUC <= 0.2)")

hmap_data <- bindata %>%
  ungroup() %>%
  filter(L50lvl == "mean") %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(meanAUC = mean(AUC)) %>%
  arrange(across(all_of(group_vars)), meanAUC)

hmap_data <- left_join(hmap_data, stk_leaves, by = c("StockKeyLabel", "SurveyName", "Quarter"))

hmap_data_long <- hmap_data %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = c(meanAUC),
                      names_to = "SummaryStat",
                      values_to = "Mean") %>%
  mutate(col_index = paste0(LeafNo, ": ", SpeciesCommonName, " (", StockKeyLabel, ") ", SurveyName, " Q", Quarter),
         col_index_num = match(col_index, rev(sort(unique(col_index)))),
         cond = case_when(
           Mean >= 0.8 ~ "++",
           Mean >= 0.6 & Mean < 0.8 ~ "+",
           Mean <= 0.4 & Mean > 0.2 ~ "-",
           Mean <= 0.2 ~ "--",
           TRUE ~ NA_character_
         ))

y_order = sort(unique(hmap_data_long$col_index))
x_order = sort(unique(hmap_data_long$Indicator))

hmap_data_long$col_index <- factor(hmap_data_long$col_index, level = rev(y_order))
hmap_data_long$Indicator <- factor(hmap_data_long$Indicator, level = indorder)
levels(hmap_data_long$Indicator)[levels(hmap_data_long$Indicator) == "Gini Index"] <- "Gini"


hmap_data_long <- mutate(hmap_data_long, cond = tidyr::replace_na(cond, ""))

hmap <- ggplot(hmap_data_long, aes(y = col_index, x = Indicator, fill = Mean)) +
  geom_tile() +
  geom_text(aes(label = cond)) +
  scale_fill_gradient2(low = "red3", mid = "white", high = "green4", limits = c(0,1), midpoint = 0.5) +
  geom_vline(xintercept = c(3.5, 5.5), linetype = "solid") +
  labs(title = "",
       y     = "",
       x     = "",
       fill  = "AUC",
       color = "AUC") +
  scale_x_discrete(position = "top") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background= element_blank(), 
        panel.border    = element_blank(),
        legend.position = "bottom",         
        legend.box      = "horizontal",
        axis.text.x     = element_text(angle = 45, hjust = 0)        
        )

hmap

ggsave(paste0(getwd(), "/Output/Plots/AUC-heatmap.png"),
       hmap, height = 12, width = 8)

## Regression Tree #############################################################
load(paste0(getwd(), "/Output/Data/RegTree/RegTreePruned.rds"))

regtree <- rpart.plot(pruned_tree, cex = 0.8, type = 1,
           extra = 101, box.palette= "BuGn")

save(regtree, file = paste0(getwd(), "/Output/Plots/Supplementary/RegTreePruned.png"))
# Export through IDE if MS does not display figure


## Regression Tree Variable Importance #########################################

vip(pruned_tree, num_features = 40, bar = FALSE, geom = "col")

importance <- data.frame("vip" = pruned_tree$variable.importance)
importance$feature <- rownames(importance)
rownames(importance) <- NULL
head(importance)

importance$feature <- factor(importance$feature, 
                             levels = arrange(importance, vip)$feature, 
                             labels = rev(c("Stock ID",
                                            "Species", 
                                            "Genus",
                                            "Survey", 
                                            "Indicator Category",
                                            "Survey Quarter")))

vip_plot <- ggplot(data = importance, aes(x = vip, y = feature, fill = type)) +
  geom_col(fill = "cyan4", colour = "black") + 
  scale_x_continuous(expand = c(0.01,0)) +
  xlab("Importance") +
  ylab("Feature") +
  theme(axis.text.x = element_text(hjust=1),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right")
vip_plot

ggsave(paste0(getwd(), "/Output/Plots/Supplementary/variable_importance.png"), 
       vip_plot, height = 2.5, width = 3.5)

## ROC matrix example ##########################################################

y <- c("Positive", "Negative")
x <- c("Negative", "Positve")
dt <- as.data.frame(cbind(y, x))

x <- abs(
  c(
    jitter(seq(1.5,1.95, length.out = 5), factor = 20),
    jitter(seq(0.3,0.5, length.out = 10), factor = 40),
    jitter(seq(0.6,0.8, length.out = 10), factor = 40),
    jitter(seq(0.7,1.4, length.out = 5), factor = 20)
  )
)

y <- abs(
  c(
    jitter(seq(0.9,1.2, length.out = 3), factor = 20),
    jitter(seq(0.1,0.5, length.out = 10), factor = 10),
    jitter(seq(0.6,0.8, length.out = 10), factor = 40),
    jitter(seq(0.7,1.4, length.out = 7), factor = 20)
  )
) 

dt2 <- as.data.frame(cbind(x,y))
dt2$Year <- seq(1990, to = 1990+nrow(dt2)-1)

#Observations
indseries <- ggplot(data = dt2) +
  geom_line(aes(Year,x), colour = "black") +
  geom_hline(yintercept = 1, lty = 2) +
  ylab("Biomass / MSY B trigger") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        aspect.ratio = 1) 
indseries
# Biomass
bioseries <- ggplot(data = dt2) +
  geom_line(aes(Year,y), colour = "black") +
  ylab("Indicator Values") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        #axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        aspect.ratio = 1) 
bioseries

fp <- dt2 %>% filter(y >= 1 & x < 1 ) %>% nrow()
tn <- dt2 %>% filter(y <  1 & x < 1 ) %>% nrow()
tp <- dt2 %>% filter(y >= 1 & x >= 1) %>% nrow()
fn <- dt2 %>% filter(y <  1 & x >= 1) %>% nrow()

ROCex <- ggplot(data = dt2) +
  geom_point(aes(x,y), colour = "darkgrey")+
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(breaks = 1, labels = expression("MSY B"[trigger])) +
  scale_y_continuous(breaks = 1, labels = "Threshold") +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(xintercept = 1) +
  geom_text(x = 0.45, y = 1.55, label = "FP", colour = "red2",   size = 4) +
  geom_text(x = 0.45, y = 1.40, label = fp, size = 3) +
  geom_text(x = 0.45, y = 0.45, label = "TN", colour = "green4", size = 4) +
  geom_text(x = 0.45, y = 0.30, label = tn, size = 3) +
  geom_text(x = 1.55, y = 0.45, label = "FN", colour = "red2",   size = 4) +
  geom_text(x = 1.55, y = 0.30, label = fn, size = 3) +
  geom_text(x = 1.55, y = 1.55, label = "TP", colour = "green4", size = 4) +
  geom_text(x = 1.55, y = 1.40, label = tp, size = 3) +
  
  ylab("Indicator Values (Predictions) \n\nNegative                   Positive") +
  xlab("Negative                   Positive \n\nBiomass Values (Observations)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        aspect.ratio = 1) 
ROCex

ggsave(paste0(getwd(), "/Output/Plots/indicator_series.png"), indseries, width = 4, height = 3)
ggsave(paste0(getwd(), "/Output/Plots/biomass_series.png"  ), bioseries, width = 4, height = 3)
ggsave(paste0(getwd(), "/Output/Plots/ROCmatrix.png"       ), ROCex,     width = 4, height = 3)


# Quick plot as ROC curve
dt2 <- dt2 %>%
  mutate(
    state = ifelse(x >= 1, 1, 0)
  )

pROC::roc(dt2, state, y, plot = T)

## ROC Curve guide ##############################################################
# Perf
tpr1 <- c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1)
fpr1 <- c(0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 1)
d <- as.data.frame(cbind(tpr1, fpr1))

rocperf <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", linewidth = 0.6) +
  annotate("text", label = "AUC = 1", x = 0.3, y = 0.75, size = 2.75) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("TPR") + 
  xlab("FPR") 

# Random
tpr1 <- seq(0,1, length.out = 9)
fpr1 <- tpr1
d <- as.data.frame(cbind(tpr1, fpr1))

rocrand <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", linewidth = 0.6) +
  annotate("text", label = "AUC = 0.5", x = 0.3, y = 0.75, size = 2.75) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("FPR") 

# Imperf
fpr1 <- c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1)
tpr1 <- c(0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 1)

d <- as.data.frame(cbind(tpr1, fpr1))

rocimp <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", linewidth = 0.6) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  annotate("text", label = "AUC = 0", x = 0.3, y = 0.75, size = 2.75) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("FPR") 

exampleroc <- cowplot::plot_grid(rocperf, rocrand, rocimp, nrow = 1, 
                        labels = c("(a)", "(b)", "(c)"), 
                        #vjust = 10,
                        label_size = 10)

ggsave(paste0(getwd(), "/Output/Plots/ROCurveGuide.png"), exampleroc, width = 10, height = 3)

## Spatial Indicator Map #######################################################
source(paste0(getwd(), "/Functions/spatinds_funs.R"))
load(paste0(getwd(), "/Output/Data/ROC/rocRem_long_surveys.rds"))
load(paste0(getwd(), "/Data/Initial/ICES Rect/ices_rect.rds"))
stksurveys <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")

# Stock
i <- 9
stk <- stk_names_rem[i]
stk_divs <- unique(filter(stksurveys, StockKeyLabel == stk)$Divisions)

# Species
species <- unique(auc_summary$SpeciesScientificName[auc_summary$StockKeyLabel==stk])
species_aphia <- icesVocab::findAphia(species, latin = TRUE)

# Survey
j <- 1
srv <- filter(srvys_rem, StockKeyLabel == stk)$SurveyName[j]
qrs <- filter(srvys_rem, StockKeyLabel == stk)$Quarter[j]

# Survey Data
load(
  list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/"), 
             pattern = paste0("^", srv, ".*\\.HLHH\\.L50\\.mean--", stk, "\\.rds$"), 
             full.names = TRUE)
  )

yrs <- unique(hlhh$Year)

# Snapshot
yr <- 2007

occ_ran_plot <- mapdis(hlhh, yr, qrs, species_aphia, stk_divs, ices_rect, matures = TRUE, # data specifics
                       cog = F, inertia = F, EOO = T, ELA = T, # spatial indicators
                       density = T,                           # weight cog and inertia
                       km2lonlat = F,                                  # convert km to lonlat
                       title = "",                                     # plot title
                       xlim = c(-11, -2), ylim = c(51, 61)) +
  theme_classic() +
  labs(title =  bquote(italic(.(species)) ~ "(whg.27.6a)"), subtitle = paste0(srv, " ",yr, " Q", qrs)) +
  #labs(title = "", subtitle = "") +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black")
  )

occ_ran_plot

ggsave(filename = paste0(getwd(), "/Output/Plots/POP-EOO-ELA-title.png"), occ_ran_plot, width = 7, height = 7)

## Lorenz Curve ################################################################
lorenz <- lorenz_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = TRUE)
gni <- Gini(lorenz, matures = TRUE)
D95 <- d95(lorenz)

lorplot <- ggplot(data = lorenz[lorenz$Year %in% c(yr),], aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_area(aes(group = Year), fill = "cyan4", alpha = 0.5) +
  geom_line(aes(group = Year), colour = "black") +
  coord_cartesian(ylim= c(-0.02,1.02), xlim = c(0,1.02), expand = FALSE) +
  labs(x = "Culmuative Proportion of ICES Rectangles", 
       y = "Culmuative Proportion of Density") +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))+
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none") +
  annotate("segment", x = 0.95, y = 0, yend = 1, xend = 0.95, colour = "red2", size = 0.3) +
  annotate("label", x = 0.75, y = 0.375, label = "A", size = 3) +
  annotate("label", x = 0.875, y = 0.125, label = "B", size = 3) + 
  annotate("text", x = 0.2, y = 0.9, label = paste0("1-Gini = ", round(gni[gni$Year == yr,]$`Gini Index`,2)), size = 3) +
  annotate("text", x = 0.2, y = 0.8, label = paste0("D95 = ", round(D95[D95$Year == yr,]$D95,2)), size = 3) +
  annotate("rect", xmin = 0, ymin = 0, ymax = 0, xmax = 1, colour = "black") +
  annotate("rect", xmin = 1, ymin = 0, ymax = 1, xmax = 1, colour = "black") +
  annotate("segment", x = 0, y = 0, yend = 1, xend = 1, colour = "black", linetype = 2, size = 0.3) +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    #panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black")
  ) +
  labs(x = "T", y = "Q(T)/Q")

lorplot

ggsave(filename = paste0(getwd(), "/Output/Plots/lorenz_curve.png"), lorplot, width = 3.5, height = 3)

## Spreading Area Curve ########################################################
sa_data <- spreadingarea_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = TRUE)
spreadingarea_calc(sa_data[sa_data$Year == yr,]$TotalNoMature_Dur, plot = T)

z <- lorenz[lorenz$Year == yr,]$TotalNoMature_Dur

# extract data
nb <-length(z)

# sort data in increasing order
#zi <- sort(z,index.return=T)
z <- sort(z)
w <- rep(1, length(z))

# computation of the spreading area 
Q <- sum(z*w)
QT <- c(0,cumsum(z*w))
QT_Q <- QT/Q
SA <- sum((QT[1:nb]+QT[2:(nb+1)])*w)/Q

# computation of (Q-Q(T))/Q as a function of T
fT <- c(0,cumsum(w))
fT <- fT[nb+1] - fT
fT <- rev(fT)
Tprop <- fT/max(fT)
QT <- QT[nb+1] - QT
QT <- rev(QT)

df <- as.data.frame(cbind(Year = yr, QT_Q, Tprop, fT))

sa_plot <- ggplot(data = df, aes(x = fT, y = QT_Q)) + 
  geom_area(aes(group = Year), fill = "cyan4", alpha = 0.5) +
  geom_line(aes(group = Year), colour = "black") +
  coord_cartesian(ylim= c(-0.02,1.02), xlim = c(0,max(df$fT)+max(df$fT)*0.01), expand = FALSE) +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8)) +
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none") +
  annotate("text", x = 15, y = 0.9, label = paste0("SA = ", round(SA,2)), size = 3) + 
  annotate("label", x = 58, y = 0.1, label = "B", size = 3) + 
  annotate("rect", xmin = 0, ymin = 0, ymax = 0, xmax = max(df$fT), colour = "black") +
  annotate("rect", xmin = max(df$fT), ymin = 0, ymax = 1, xmax = max(df$fT), colour = "black") +
  annotate("segment", x = 0, y = 0, yend = 1, xend = max(df$fT), colour = "black", linetype = 2, size = 0.3) +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    #panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black")
  ) +
  labs(x = "T", y = "Q(T)/Q")

sa_plot

ggsave(filename = paste0(getwd(), "/Output/Plots/sa_curve.png"), sa_plot, width = 3.5, height = 3)

joint_plot <- cowplot::plot_grid(lorplot, sa_plot, labels = c("a.", "b."), label_size = 10)
ggsave(filename = paste0(getwd(), "/Output/Plots/lorenz-sa-curves.png"), joint_plot, width = 7, height = 3)
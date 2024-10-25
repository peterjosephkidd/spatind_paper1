#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               9. Tables
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(icesVocab)
library(readxl)
library(writexl)
library(ggplot2) 

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/SummaryTables/"

refpts <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-69stks-AY2022-stkdescrptn.xlsx"))
load(paste0(load.path, "Data/DR_Stocks/FishBaseMaturity/FishBase-L50.rds"))
load(paste0(load.path, "Data/DR_Stocks/SurveyData/stks.rds")) # saved stks from data_2_DownloadDATRAS
load(file = paste0(load.path, "Data/DR_Stocks/Outputs/ROC/expected-surveys.rds"))
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/results_funs.R")


# Table 1: Stock summary #######################################################
T1stksum2 <- Lmat[c(1:3,7,9:10)]

T1stksum2 <- left_join(T1stksum2, refpts[c(1:2, 5:6, 8:15)], by = "StockKeyLabel")

final_stks <- unique(srvys$StockKeyLabel)

T1stksum <- T1stksum2 %>%
  filter(StockKeyLabel %in% final_stks) %>%
  arrange(FisheriesGuild, SizeGuild, StockKeyLabel) %>%
  relocate("Stock ID" = StockKeyLabel,
           "Species" = SpeciesCommonName, 
           "Scientific Name" = SpeciesScientificName, 
           "Guild" = FisheriesGuild, 
           "Size Guild" = SizeGuild,
           "Trophic Guild" = TrophicGuild, 
           "Eco Region" = EcoRegion,
           
           "L50"    = mu.stk,
           "CI (-)" = lc.stk, 
           "CI (+)" = uc.stk,
           
           "AY" = AssessmentYear, 
           "Model" = ModelName,
           "Data Category" = DataCategory, 
           "MSY Btrigger" = MSYBtrigger,
           
           "Expert Group" = ExpertGroup, 
           "Advice Category" = AdviceCategory, 
           "Assessment Key" = AssessmentKey 
  ) %>%
  mutate(L50 = round(L50, 2),
         `CI (-)` = round(`CI (-)`, 2),
         `CI (+)` = round(`CI (+)`, 2)) %>%
  select(-`Eco Region`) # too wide

# Some stocks dont have the assessment model info
missing_am_stocks <- c("tur.27.4", "ldb.27.8c9a", "meg.27.7b-k8abd", "meg.27.8c9a", "whg.27.6a", "dgs.27.nea")

missing_am <- data.frame("Stock ID" = missing_am_stocks,
                         "Model" = c("SAM", "a4a", "a4a", "a4a", "SAM", "Unknown")) %>%
  rename("Stock ID" = "Stock.ID")

missing_am <- merge(T1stksum, missing_am, by = "Stock ID", suffixes = c(".NA", "")) %>% select(-`Model.NA`)

T1stksum <- T1stksum %>% 
  filter(!`Stock ID` %in% missing_am_stocks) %>%
  rbind(., missing_am) %>%
  arrange(Guild, L50, Species, `Stock ID`)

T1stksum.short <- T1stksum[c("Stock ID", "Species", "Scientific Name", "Guild", "L50", "CI (-)", "CI (+)", "AY", "Model", "MSY Btrigger", "Expert Group")]

# Save Table
suppressWarnings(dir.create(save.path, recursive = T))

save(T1stksum, file = paste0(save.path, "/T1.stksum.rds"))
save(T1stksum.short, file = paste0(save.path, "/T1.stksum_short.rds"))
write.csv(T1stksum, paste0(save.path, "/T1.stksum.csv"), row.names = F)
write.csv(T1stksum.short, paste0(save.path, "/T1.stksum_short.csv"), row.names = F)


df_to_latex(T1stksum.short, 
            format_rows = 1:nrow(T1stksum.short),
            format_cols = 3,
            format_style = "italics",
            bold_header = TRUE,
            label = "tab: stocks", 
            caption = "Stocks selected for analysis and other important metrics including length at maturity (L50), assessment year (AY), stock assessment model, MSY B\\textsubscript{trigger} estimate, and the ICES expert group that conducted the assessment.",
            file_path = paste0(save.path, "/tab_stocks.tex"))

# Table 2: Spatial indicators  #################################################
rm(list = ls())
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/SummaryTables/"
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/results_funs.R")
tab_inds <- read_xlsx(paste0(load.path, "Data/DR_Stocks/Outputs/SpatInds/Tables.xlsx"), sheet = "Indicators")
tab_inds <- tab_inds %>% 
  select(-`Units/Range`) %>% 
  filter(Characteristic != "Location")

tab_inds$Code <- factor(tab_inds$Code, level = c("I", "EOO", "ELA", "POPR", "POPH", "G", "D95", "SA", "EA", "SPI"))
tab_inds <- arrange(tab_inds, Code)
tab_inds$Code <- as.character(tab_inds$Code)
tab_inds$Description <- stringr::str_replace_all(tab_inds$Description, "%", "\\\\%")
tab_inds$`Spatial Indicator` <- stringr::str_replace_all(tab_inds$`Spatial Indicator`, "%", "\\\\%")

df_to_latex(tab_inds, 
            bold_header = TRUE,
            font_size = "small",
            resize = TRUE,
            wrap_text = c(1,4), 
            text_width = c(10, 18),
            label = "tab: inds", 
            caption = "Description of the ten spatial indicators that are compared to stock assessment outputs.",
            file_path = paste0(save.path, "/tab_inds.tex")
            )

# S Table 1: Supplementary Table of AUCs and TSS for All L50 sensitivty test ##############
rm(list = ls())
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/SummaryTables/"
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/T1-AUC-TSS.rds")
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/results_funs.R")
head(t.auctss)

tab_aucL50 <- t.auctss %>%
  rename(`Stock ID` = StockKeyLabel, Qr = Quarter) %>%
  select(-c(K, AvgSurveyCoverage)) 

tab_aucL50$`Spatial Indicator` <- gsub("Gini Index", "G", tab_aucL50$`Spatial Indicator`)
tab_aucL50$`Spatial Indicator` <- gsub("Inertia", "I", tab_aucL50$`Spatial Indicator`)

df <- head(tab_aucL50)

for (i in unique(tab_aucL50$`Stock ID`)) {
  df <- filter(tab_aucL50, `Stock ID` == i)
  df_to_latex(df,
              bold_header = TRUE,
              font_size = "tiny",
              longtable = FALSE,
              resize = TRUE,
              nest_cols = list(c(6, 7, 8), c(9, 10, 11), c(12, 13, 14)),
              nest_cols_rename = list(c("AUC", "TSS", "Thresh"), c("AUC", "TSS", "Thresh"), c("AUC", "TSS", "Thresh")),
              nest_header = c("Mean", "Lower CI", "Upper CI"),
              wrap_text = c(1,5), 
              text_width = c(6,5),
              label = "sup: auc_l50", 
              caption = paste0("ROC performance metrics for ", i, " across L50 sensitivity tests."),
              file_path = paste0(save.path, "/sup_aucL50_", i, ".tex"))
}


# Stock Info of the 31 stocks selected after optimising stock selection
# This contains all colums
read_xlsx("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/icesData-31stks-AY2022-SA-data-optim.xlsx")
# This filters some columns out
stk_info <- read_xlsx("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/icesData-31stks-AY2022-stkdescrptn-optim.xlsx")

# Survey data is saved here:
"~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/all.stocks"
# Here is a summary of the surveys (though I am not sure it is accurate)
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/all.stocks/SurveyDownloadSummary.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/icesData-stksurveys_full.rds") # selection of columns from icesData-AllSurveyData-manual.xlsx
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/stks.rds") # the 39 stks in icesData-AllSurveyData-manual.xlsx

# Lmat of 68 stocks
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/FishBaseMaturity/FishBase-L50.rds")

# Survey coverage
read_xlsx("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/icesData-stksurveys-survcoverage.xlsx")

# Spatial Indicators Data
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/SpatInds/Spatial.indicators.rds")
head(spatinds)

# ROC Data
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/ROCdata.rda")
head(rocAll_long)

# RFE
# To see if L50 is important
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/feature_selection.rds")
summary(feature_sel)

# Using just mean L50
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/feature_selection_meanL50.rds")
summary(feature_sel2)

# Regression Tree Leaves
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/stockleaves.rds")
head(stk_leaves2)

# Expected Surveys after removing poor surveys
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/expected-surveys.rds")
head(srvys)
dim(srvys)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               5. Assess Spatial Indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(icesVocab)
library(readxl)
library(ggplot2) 

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/"

# Stock Assessment Data 
sa_data <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-69stks-AY2022-SA-data.xlsx"))

# Spatial Indicator Data
si.files <- list.files(paste0(load.path, "Data/DR_Stocks/Outputs/Spatinds/"), full.names = T)
load(si.files[1])

# ROC Functions
source(paste0(load.path, "Functions/ROC_funs.R")) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ssb.ref <- sa_data %>% 
  select(StockKeyLabel, Year, SSB, MSYBtrigger) %>%
  filter(StockKeyLabel %in% unique(spatinds$StockKeyLabel)) %>%
  mutate(`SSB/MSYBtrigger` = SSB/MSYBtrigger) %>%
  full_join(., spatinds, by = c("StockKeyLabel", "Year")) %>%
  mutate(SurveyNameIndex = paste0(SurveyName, ", ", SurveyIndex))

rbind(head(ssb.ref), tail(ssb.ref))

all(unique(spatinds$StockKeyLabel) %in% unique(ssb.ref$StockKeyLabel))

# Parameters
state <- "SSB/MSYBtrigger"
inds <- c("CoG (x)", "CoG (y)", "Inertia", "EOO", "ELA", "POPR", "POPH", "Gini Index", "D95", "SA", "EA", "SPI")
stks <- unique(ssb.ref$StockKeyLabel)
roc_longlist <- list()
iter <- 1

# Loop
for (i in 1:length(stks)) {
  
  stk <- stks[i]
  #stk <- "ple.27.7a"
  #i <- which(stks == stk)
  ssb.ref.stk <- filter(ssb.ref, StockKeyLabel == stk)
  srvys <- unique(ssb.ref.stk$SurveyNameIndex)
  
  for (j in 1:length(srvys)) {
    
    srvindx <- srvys[j]
    ssb.ref.stk.srvindx <- filter(ssb.ref.stk, SurveyNameIndex == srvindx)
    qrs <- sort(unique(ssb.ref.stk.srvindx$Quarter))
    if (length(qrs) == 0) {qrs = NA}
    
    for (w in 1:length(qrs)) {
      qr <- qrs[w]
      message(paste0("#>>>>>> ", stk, ": ", srvindx, ", Quarter: ", qr, " >>>>>>#"))
      
      if (is.na(qr)) {
        ssb.ref.stk.srvindx.qr <- ssb.ref.stk.srvindx
      } else{
      ssb.ref.stk.srvindx.qr <- filter(ssb.ref.stk.srvindx, Quarter == qr)
      }
      
      L50levels <- unique(ssb.ref.stk.srvindx.qr$L50lvl)
      if (length(L50levels) == 0) {L50levels = NA}
      
      for (lvl in 1:length(L50levels)) {
        
        L50 <- L50levels[lvl]
        ssb.ref.stk.srvindx.qr.lvl <- filter(ssb.ref.stk.srvindx.qr, L50lvl == L50) 
        
        roc_long <- rocR(ssb.ref.stk.srvindx.qr.lvl, state, inds, format.df = "long", p = F)
        roc_long$L50lvl <- L50
        roc_longlist[[iter]] <- roc_long
        iter <- iter + 1
      }
    }
  }
}

# Output
rocAll_long <- do.call(rbind, roc_longlist)

# Save
suppressWarnings(dir.create(paste0(save.path, "/ROC"), recursive = T))
save(rocAll_long, file = paste0(save.path, "ROC/ROCdata.rda"))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Apparent issue
# There is spatial indicator data but no AUC for many cases
missdata <- rocAll_long %>%
  ungroup() %>%
  filter(!is.na(`Spatial Indicator Value`),
         is.na(AUC)) %>%
  select(StockKeyLabel, Quarter, SurveyIndex, SurveyName) %>%
  distinct() %>%
  arrange(StockKeyLabel) 

# But these are stocks with no stock status contrast in the time series that the spatial indicators are calculated for
# Must be contrast in stock status for ROC to be run
no.contrast <- rocAll_long %>% 
  group_by(Quarter, StockKeyLabel, SurveyIndex, SurveyName, SurveyNameIndex, `Spatial Indicator`) %>%
  filter(!is.na(`Spatial Indicator Value`), 
         Year > 1000) %>%
  mutate(contrast = if_else(all(status == TRUE), 0, 
                            if_else(all(status == FALSE), 0, 1))) %>%
  filter(contrast == 0)  %>%
  ungroup() %>%
  select(StockKeyLabel, Quarter, SurveyIndex, SurveyName) %>%
  distinct() %>%
  filter(!is.na(SurveyName)) %>%
  arrange(StockKeyLabel) %>%
  print(n = nrow(.))

# The stocks in issues that are not accounted for in no contrast
probstk <- missdata[!missdata$StockKeyLabel %in% no.contrast$StockKeyLabel,] # none

View(rocAll_long %>%
  filter(StockKeyLabel == probstk$StockKeyLabel[1],
         Quarter == probstk$Quarter,
         SurveyIndex == probstk$SurveyIndex,
         SurveyName == probstk$SurveyName) %>%
  filter(!is.na(`Spatial Indicator Value`),
         is.na(AUC)))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Explore
roc_summary <- rocAll_long %>%
  select(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`, AUC, L50lvl) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`, L50lvl) %>%
  na.omit() %>%
  summarise(AUC) %>%
  print(n = nrow(.))

table(roc_summary$`Spatial Indicator`)

roc_summary %>%
  group_by(`Spatial Indicator`, L50lvl) %>%
  summarise(n.good = length(which(AUC >= 0.75)),
            n.avg  = length(which(AUC < 0.75 & AUC >= 0.35)),
            n.bad  = length(which(AUC < 0.35))) %>%
  arrange(L50lvl, -n.good) %>%
  print(n = nrow(.))

roc_summary %>%
  group_by(StockKeyLabel, L50lvl) %>%
  summarise(n.good = length(which(AUC >= 0.75)),
            n.avg  = length(which(AUC < 0.75 & AUC >= 0.35)),
            n.bad  = length(which(AUC < 0.35))) %>%
  arrange(L50lvl, -n.good) %>%
  print(n = nrow(.))

# Years of data requested vs actual
stksurveys <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")

stksurveys %>%
  select(StockKeyLabel, SurveyAcronymn, SurveyIndex, YearStart, YearEnd, Quarter, InDatras, Divisions) %>%
  filter(InDatras == 1) %>%
  distinct() %>%
  na.omit() %>%
  rename(SurveyName  = SurveyAcronymn,
         YearStrtReq = YearStart,
         YearEndReq  = YearEnd)

YrsAct <- rocAll_long %>%
  filter(!is.na(SurveyName),
         Year > 1000) %>%
  select(-c(`SSB/MSYBtrigger`, status, TP, FP, TN, FN, TPR, FPR, TSS, `Spatial Indicator Value`)) %>%
  group_by(Quarter, StockKeyLabel, SurveyIndex, SurveyName, SurveyNameIndex, AUC, `Spatial Indicator`) %>%
  mutate(YearStrtAct = min(Year),
         YearEndAct  = max(Year))

YrsAct <- YrsAct %>%
  group_by(Quarter, StockKeyLabel, SurveyIndex, SurveyName, SurveyNameIndex, AUC, `Spatial Indicator`) %>%
  mutate(YearSeries = paste0(sort(Year), collapse = ", "),
         YearRange  = paste0(YearStrtAct:YearEndAct, collapse = ", ")) %>%
  select(-Year) %>%
  distinct() %>%
  ungroup() %>%
  mutate(t = if_else(
    is.null(dim(unlist(strsplit(YearRange, ", "))[unlist(strsplit(YearRange, ", ")) %in% unlist(strsplit(YearSeries, ", "))])),
    "none", 
    paste0(unlist(strsplit(YearRange, ", "))[unlist(strsplit(YearRange, ", ")) %in% unlist(strsplit(YearSeries, ", "))], collapse = ", ")
  ))

unlist(strsplit(YrsAct$YearRange[1], ", "))[!unlist(strsplit(YrsAct$YearRange[1], ", ")) %in% unlist(strsplit(YrsAct$YearSeries[1], ", "))]
is.null(dim(unlist(strsplit(YrsAct$YearRange[1], ", "))[!unlist(strsplit(YrsAct$YearRange[1], ", ")) %in% unlist(strsplit(YrsAct$YearSeries[1], ", "))]))

         YearMiss   = if_else(is.null(dim(as.integer(unlist(strsplit(YearRange, ",")))[
           !as.integer(unlist(strsplit(YearRange, ", "))) %in% 
             as.integer(unlist(strsplit(YearSeries, ", ")))])), "none", paste0(as.integer(unlist(strsplit(YearRange, ", ")))[
               !as.integer(unlist(strsplit(YearRange, ", "))) %in% 
                 as.integer(unlist(strsplit(YearSeries, ", ")))])

 
YrsAct <- YrsAct %>% filter(StockKeyLabel == "mac.27.nea", SurveyNameIndex == "NS-IBTS, IBTS Q1", `Spatial Indicator` == "D95")



# Define the character strings
YearRange <- c("2000, 2001, 2002, 2003, 2004, 2005, 2006")
YearAct <- c("2000, 2001, 2003, 2006, 2005")

# Convert character strings to numeric vectors
YearRange_vec <- as.integer(unlist(strsplit(YearRange, ", ")))
YearAct_vec   <- as.integer(unlist(strsplit(YearAct, ", ")))

# Convert vectors to sets
YearRange_set <- as.integer(unique(YearRange_vec))
YearAct_set <- as.integer(unique(YearAct_vec))

# Find the missing years using set operations
missing_years <- setdiff(YearAct_set, YearRange_set)
YearRange_set[!YearRange_set %in% YearAct_set]

# Print the missing years
print(missing_years)


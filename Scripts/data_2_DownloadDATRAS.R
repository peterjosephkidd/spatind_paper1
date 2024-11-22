#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                        2. Download DATRAS Survey data
#>                         
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script downloads all the survey data used for the stocks selected in their
#> assessment that is available on DATRAS. 
#> 
#> These survey files are already provided in the folder 
#> /Data/Initial/DR_Stocks/SurveyData/
#> 
#> And there is no need to run this script except to see how data was downloaded.
#> The script takes a while to download data. 
#> Re-downloading data may also give different results later on due to 
#> retrospecitve corrections to the data made by ICES
#> 
#> To reproduce the results of the paper, DO NOT overwrite existing survey data 
#> by running this script and saving the downloaded survey data. 

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)

rm(list = ls())

save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/")
paste0(getwd(), "/Data/Initial/DR_Stocks/SurveyData/")

source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/dataprep_funs.R")

stksurveys <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")

# Stock with enough info to run code
# AVailable surveys in DATARS:
datrassrvys <- c("BITS", "BTS", "BTS-GSA17", "BTS-VIII", "Can-Mar", "DWS", "DYFS", "EVHOE", "FR-CGFS", "FR-WCGFS",
"IE-IAMS", "IE-IGFS", "IS-IDPS", "NIGFS", "NL-BSAS", "NS-IBTS", "NS-IBTS_UNIFtest", "NS-IDPS", "NSSS", "PT-IBTS",
"ROCKALL", "SCOROC", "SCOWCGFS", "SE-SOUND", "SNS", "SP-ARSA", "SP-NORTH", "SP-PORC", "SWC-IBTS")

stksurveys_full <- stksurveys %>%
  select(-c(Ship, Country, YrsExclude, Ages, inRcntStkAnX, inMatCalc, MatYrs, Usage, Notes, `Full Name`)) %>%
  na.omit() %>%
  filter(SurveyAcronymn %in% datrassrvys) %>%
  print(n = nrow(.))

length(unique(stksurveys_full$StockKeyLabel))

# Method 1: Surveys within each stock >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This method is inefficient when downloading data for many stocks. 
# Some surveys will be downloaded numerous times (e.g. NS-IBTS, EVHOE)
# 
# IGNORE THIS CHUNK --> RUN METHOD 2

#stks <- unique(stksurveys_full$StockKeyLabel)
# stks <- "ank.27.78abd"
  
#for (i in 1:length(stks)) {
#  stk <- stks[i]

#  srvys <- stksurveys %>%
#    filter(StockKeyLabel == stk) %>%
#    select(SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)
  
#  srvy.list <- unique(srvys$SurveyAcronymn)
  
#  suppressWarnings(dir.create(paste0(save.path, stk, "/raw"), recursive = T))
  
#  for (j in 1:length(srvy.list)) {
    
#    srvy <- srvy.list[j] 

#    srvys.filt <- srvys[srvys$SurveyAcronymn == srvy,]

#    qrs <- sort(unique(srvys.filt$Quarter))
#    yrs <- min(srvys.filt$YearStart, na.rm = T):max(srvys.filt$YearEnd, na.rm = T)

#    hh <- data.frame()
#    hl <- data.frame()
#    ca <- data.frame()
    
#    message(paste0(stk, ": ", srvy, " --- HH"))
#    hh.df <- try(getDATRAS(record = "HH", srvy, years = yrs, quarters = c(qrs)))
#    message(paste0(stk, ": ", srvy, " --- HL"))
#    hl.df <- try(getDATRAS(record = "HL", srvy, years = yrs, quarters = c(qrs)))
#    message(paste0(stk, ": ", srvy, " --- CA"))
#    ca.df <- try(getDATRAS(record = "CA", srvy, years = yrs, quarters = c(qrs)))
    
#    hh <- rbind(hh, hh.df)
#    hl <- rbind(hl, hl.df)
#    ca <- rbind(ca, ca.df)
    
#    table(hh$Year, hh$Quarter)
#    table(hl$Year, hl$Quarter)
#    table(ca$Year, ca$Quarter)
      
#    save(hh, file = paste0(save.path, stk, "/raw/", namefile(stk, hh)))
#    save(hl, file = paste0(save.path, stk, "/raw/", namefile(stk, hl)))
#    save(ca, file = paste0(save.path, stk, "/raw/", namefile(stk, ca)))
#  }
#}

# Method 2: All Surveys >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This method downloads each survey once only, 
# instead of repeating downloads of the same surveys
# USE THIS

allsrvys2 <- stksurveys %>%
  select(SurveyAcronymn, SurveyRefNo, YearStart, YearEnd, Quarter) %>%
  group_by(SurveyAcronymn, Quarter) %>%
  mutate(minYr = min(YearStart), maxYr = max(YearEnd)) %>%
  distinct()

allsrvys <- allsrvys2 %>% 
  arrange(SurveyAcronymn) %>%
  select(-YearStart, -YearEnd, -SurveyRefNo) %>%
  group_by(SurveyAcronymn) %>%
  mutate(Quarters = paste0(sort(unique(Quarter)), collapse = ", "),
         minYr = min(minYr), maxYr = max(maxYr)) %>%
  select(-Quarter) %>%
  distinct() %>%
  filter(SurveyAcronymn %in% datrassrvys) %>%
  mutate(minYr    = if_else(is.na(minYr), 1980, minYr),
         maxYr    = if_else(is.na(maxYr), 2023, maxYr),
         Quarters = if_else(is.na(Quarters), "1, 2, 3, 4", Quarters)) %>%
  print(n = nrow(.))

stk <- "all.stocks"

suppressWarnings(dir.create(paste0(getwd(), "/Data/Initial/DR_Stocks/SurveyData/all.stocks"), recursive = T))

for(i in 1:nrow(allsrvys)){
  
  srv <- allsrvys[i,]$SurveyAcronymn
  yrs <- allsrvys[i,]$minYr:allsrvys[i,]$maxYr
  qrs <- as.integer(unlist(strsplit(allsrvys[i,]$Quarters, ", ")))
  
  message(paste0(srv, ": ", min(yrs),"-", max(yrs), ", Qrs ", allsrvys[i,]$Quarters, " --- HH"))
  hh <- try(getDATRAS(record = "HH", srv, years = yrs, quarters = c(qrs)))
  message(paste0(srv, ": ", min(yrs),"-", max(yrs), ", Qrs ", allsrvys[i,]$Quarters, " --- HL"))
  hl <- try(getDATRAS(record = "HL", srv, years = yrs, quarters = c(qrs)))
  message(paste0(srv, ": ", min(yrs),"-", max(yrs), ", Qrs ", allsrvys[i,]$Quarters, " --- CA"))
  ca <- try(getDATRAS(record = "CA", srv, years = yrs, quarters = c(qrs)))
  
  save(hh, file = paste0(getwd(), "/Data/Initial/DR_Stocks/SurveyData/all.stocks/", namefile(stk, hh)))
  save(hl, file = paste0(getwd(), "/Data/Initial/DR_Stocks/SurveyData/all.stocks/", namefile(stk, hl)))
  save(ca, file = paste0(getwd(), "/Data/Initial/DR_Stocks/SurveyData/all.stocks/", namefile(stk, ca)))
}


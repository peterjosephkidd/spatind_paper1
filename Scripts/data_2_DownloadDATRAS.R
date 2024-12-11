#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                        2. Download DATRAS Survey data
#>                         
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script downloads all the survey data used for the stocks selected in their
#> assessment that is available on DATRAS. The survey data used in assessment was 
#> identified from ICES advice sheets, WG reports, and stock annexes were for the
#> stocks identified in data_1_SelectStocks.R and is available in 
#> /Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx. 
#> Stocks that used DATRAS survey data were retained in the analysis. 
#> 
#> These survey data files are already provided in the folder 
#> /Data/Initial/DR_Stocks/SurveyData/
#> 
#> There is no need to run this script except to see how data was downloaded.
#> The script takes a long time to download data. Re-downloading data may produce
#> differences in results due to retrospective corrections to the data made by ICES
#> 
#> To reproduce the results of the paper, DO NOT run this script. It will overwrite 
#> existing survey data in the repo. 

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
# Available surveys in DATARS:
datrassrvys <- c("BITS", "BTS", "BTS-GSA17", "BTS-VIII", "Can-Mar", "DWS", "DYFS", "EVHOE", "FR-CGFS", "FR-WCGFS",
"IE-IAMS", "IE-IGFS", "IS-IDPS", "NIGFS", "NL-BSAS", "NS-IBTS", "NS-IBTS_UNIFtest", "NS-IDPS", "NSSS", "PT-IBTS",
"ROCKALL", "SCOROC", "SCOWCGFS", "SE-SOUND", "SNS", "SP-ARSA", "SP-NORTH", "SP-PORC", "SWC-IBTS")

stksurveys_full <- stksurveys %>%
  select(-c(Ship, Country, YrsExclude, Ages, inRcntStkAnX, inMatCalc, MatYrs, Usage, Notes, `Full Name`)) %>%
  na.omit() %>%
  filter(SurveyAcronymn %in% datrassrvys) %>%
  print(n = nrow(.))

length(unique(stksurveys_full$StockKeyLabel))

# All Surveys >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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


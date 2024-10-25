#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                        2. Download DATRAS Survey data
#>                         
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)

rm(list = ls())

load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/dataprep_funs.R")

stksurveys <- read_xlsx(paste0(load.path, "icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")

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
save(stksurveys_full, file = paste0(save.path, "icesData-stksurveys_full.rds"))

# Method 1: Surveys within each stock >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This method is inefficient when downloading data for many stocks. 
# Some surveys will be downloaded numerous times (e.g. NS-IBTS, EVHOE)
# See method 2

stks <- unique(stksurveys_full$StockKeyLabel)
save(stks, file = paste0(save.path, "/stks.rds")) # save stks for later scripts
# stks <- "ank.27.78abd"
  
for (i in 1:length(stks)) {
  stk <- stks[i]

  srvys <- stksurveys %>%
    filter(StockKeyLabel == stk) %>%
    select(SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)
  
  srvy.list <- unique(srvys$SurveyAcronymn)
  
  suppressWarnings(dir.create(paste0(save.path, stk, "/raw"), recursive = T))
  
  for (j in 1:length(srvy.list)) {
    
    srvy <- srvy.list[j] 

    srvys.filt <- srvys[srvys$SurveyAcronymn == srvy,]

    qrs <- sort(unique(srvys.filt$Quarter))
    yrs <- min(srvys.filt$YearStart, na.rm = T):max(srvys.filt$YearEnd, na.rm = T)

    hh <- data.frame()
    hl <- data.frame()
    ca <- data.frame()
    
    message(paste0(stk, ": ", srvy, " --- HH"))
    hh.df <- try(getDATRAS(record = "HH", srvy, years = yrs, quarters = c(qrs)))
    message(paste0(stk, ": ", srvy, " --- HL"))
    hl.df <- try(getDATRAS(record = "HL", srvy, years = yrs, quarters = c(qrs)))
    message(paste0(stk, ": ", srvy, " --- CA"))
    ca.df <- try(getDATRAS(record = "CA", srvy, years = yrs, quarters = c(qrs)))
    
    hh <- rbind(hh, hh.df)
    hl <- rbind(hl, hl.df)
    ca <- rbind(ca, ca.df)
    
    table(hh$Year, hh$Quarter)
    table(hl$Year, hl$Quarter)
    table(ca$Year, ca$Quarter)
      
    save(hh, file = paste0(save.path, stk, "/raw/", namefile(stk, hh)))
    save(hl, file = paste0(save.path, stk, "/raw/", namefile(stk, hl)))
    save(ca, file = paste0(save.path, stk, "/raw/", namefile(stk, ca)))
  }
}

# Method 2: All Surveys >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This method downloads each survey once only, 
# instead of repeating downloads of the same surveys

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
suppressWarnings(dir.create(paste0(save.path, stk, "/raw"), recursive = T))

surveysummary <- data.frame()

for(i in 6:nrow(allsrvys)){
  
  srv <- allsrvys[i,]$SurveyAcronymn
  yrs <- allsrvys[i,]$minYr:allsrvys[i,]$maxYr
  qrs <- as.integer(unlist(strsplit(allsrvys[i,]$Quarters, ", ")))
  
  message(paste0(srv, ": ", min(yrs),"-", max(yrs), ", Qrs ", allsrvys[i,]$Quarters, " --- HH"))
  hh <- try(getDATRAS(record = "HH", srv, years = yrs, quarters = c(qrs)))
  message(paste0(srv, ": ", min(yrs),"-", max(yrs), ", Qrs ", allsrvys[i,]$Quarters, " --- HL"))
  hl <- try(getDATRAS(record = "HL", srv, years = yrs, quarters = c(qrs)))
  message(paste0(srv, ": ", min(yrs),"-", max(yrs), ", Qrs ", allsrvys[i,]$Quarters, " --- CA"))
  ca <- try(getDATRAS(record = "CA", srv, years = yrs, quarters = c(qrs)))
  
  save(hh, file = paste0(save.path, stk, "/raw/", namefile(stk, hh)))
  save(hl, file = paste0(save.path, stk, "/raw/", namefile(stk, hl)))
  save(ca, file = paste0(save.path, stk, "/raw/", namefile(stk, ca)))
  
  
  for (j in 1:length(qrs)) {
    
    rhh <- unique(hh$RecordType)
    qhh <- unique(hh$Quarter)[j]
    yrminhh <- min(yrs)
    yrmaxhh <- max(yrs)
    yrmisshh <- paste0(yrs[!yrs %in% unique(hh[hh$Quarter == j,]$Year)], collapse = " ")
    if(yrmisshh == paste(yrs, collapse = " ")){yrmisshh = "All"}
    hhsum <- cbind("Survey" = srv, "RecordType" = rhh, "QuarterReq" = qhh, "YearMinReq" = yrminhh, "YearMaxReq" = yrmaxhh, "YearMiss" = yrmisshh)
    
    rhl <- unique(hl$RecordType)
    qhl <- unique(hl$Quarter)[j]
    yrminhl <- min(yrs)
    yrmaxhl <- max(yrs)
    yrmisshl <- paste0(yrs[!yrs %in% unique(hl[hl$Quarter == j,]$Year)], collapse = " ")
    if(yrmisshl == paste(yrs, collapse = " ")){yrmisshl = "All"}
    hlsum <- cbind("Survey" = srv, "RecordType" = rhl, "QuarterReq" = qhl, "YearMinReq" = yrminhl, "YearMaxReq" = yrmaxhl, "YearMiss" = yrmisshl)
    
    rca <- unique(ca$RecordType)
    qca <- unique(ca$Quarter)[j]
    yrminca <- min(yrs)
    yrmaxca <- max(yrs)
    yrmissca <- paste0(yrs[!yrs %in% unique(ca[ca$Quarter == j,]$Year)], collapse = " ")
    if(yrmissca == paste(yrs, collapse = " ")){yrmissca = "All"}
    casum <- cbind("Survey" = srv, "RecordType" = rca, "QuarterReq" = qca, "YearMinReq" = yrminca, "YearMaxReq" = yrmaxca, "YearMiss" = yrmissca)
    
    surveysummary <- rbind(surveysummary, hhsum, hlsum, casum)
  }
}

surveysummary # ignore "All" - code not working
save(surveysummary, file = paste0(save.path, "/SurveyDownloadSummary.rds"))

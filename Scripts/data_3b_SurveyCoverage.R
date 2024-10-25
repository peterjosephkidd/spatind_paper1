#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               3.b. Filter to surveys with 'good' coverage
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(writexl)
library(readxl)
library(dplyr)

rm(list = ls())

load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/"

stksurveys2 <- read_xlsx(paste0(load.path, "icesSA_data/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Divs/ices_divs.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")

load(paste0(load.path, "SurveyData/stks.rds")) # saved stks from data_2_DownloadDATRAS
#stks <- unique(stksurveys$StockKeyLabel)
#stks <- "ank.27.78abd"
stksurveys <- filter(stksurveys2, InDatras == 1)
stksurveys$AvgSurveyCoverage <- rep(NA, nrow(stksurveys))
stksurveys$StkRects <- rep(NA, nrow(stksurveys))

ICESareas <- sort(unique(ices_rect$Area_27))

srvys.coverage <- data.frame()

for (i in 1:length(stks)) {
  
  stk <- stks[i]
  #stk <- "cod.27.6a"
  #i <- which(stks == stk)
  
  message(paste0("\n", i, ": ",stk))
  
  divs <- unique(stksurveys[stksurveys$StockKeyLabel == stk,]$Divisions)
  stkdivs <- unlist(strsplit(divs, ", "))
  
  if(paste(stkdivs, collapse = ", ") == "NEA") {stkdivs = unique(ices_rect$Area_27)[!is.na(unique(ices_rect$Area_27))]}
  
  if(!all(stkdivs %in% ICESareas)){
    warning("The following stock divisions are not in the ICES StatRec shapefile:", immediate. = T)
    writeLines(stkdivs[!stkdivs %in% ICESareas])
  }

  # No. ICES Rectangles in Stock Area
  TotRects <- ices_rect %>%
    filter(Area_27 %in% stkdivs) %>%
    select(Area_27, ICESNAME) %>%
    distinct() %>%
    nrow()

  srvys <- stksurveys[stksurveys$StockKeyLabel == stk,]

  for(j in 1:nrow(srvys)){
    srv <- srvys[j,]$SurveyAcronymn
    indx <- srvys[j,]$SurveyIndex
    tryCatch({
      
      yrs <- srvys[j,]$YearStart:srvys[j,]$YearEnd
      qrs <- srvys[j,]$Quarter

      message(paste0(i, ".", j, ": ", srv, ", ", indx))
      files <- list.files(paste0(load.path, "SurveyData/", stk, "/matures/"), pattern = paste0("^", srv, "\\.Yr.*L50\\.mean"), full.names = T)
      files.hlhh <- files[stringr::str_detect(files, "HLHH")]
      do.call(list, lapply(files.hlhh, load, envir = .GlobalEnv))
      
      # Average across years and quarters
      avg <- hlhh %>%
        filter(Area_27 %in% stkdivs,
               Year %in% yrs,
               Quarter %in% qrs) %>%
        group_by(Year) %>%
        select(Year, StatRec) %>%
        distinct() %>%
        summarise(N.SampledRects = length(StatRec)) %>%
        mutate(N.RectsInStkDiv = TotRects,
               SurveyCoverage = (N.SampledRects/N.RectsInStkDiv)*100,
               AvgSurveyCoverage = ((sum(N.SampledRects)/nrow(.))/N.RectsInStkDiv)*100) %>%
        select(N.RectsInStkDiv, AvgSurveyCoverage) %>%
        distinct()
      
      srvys[j,]$AvgSurveyCoverage <- avg$AvgSurveyCoverage
      srvys[j,]$StkRects <- avg$N.RectsInStkDiv
      srvys.coverage <- rbind(srvys.coverage, srvys[j,])
      
    }, error = function(e){
      message(paste0(stk, ": ", srv, ", ", indx, ". ", "Data missing. Skipping this survey index."))
    })

    next
      
  }
}

srvys.coverage %>%
  select(StockKeyLabel, SpeciesCommonName, SurveyAcronymn, SurveyIndex, Quarter, YearStart, YearEnd, AvgSurveyCoverage, SurveyName) %>%
  distinct() %>%
  arrange(StockKeyLabel, -AvgSurveyCoverage) %>%
  print(n = nrow(.))

write_xlsx(srvys.coverage, path = paste0(save.path, "/icesData-stksurveys-survcoverage.xlsx"))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                      3. Process DATRAS Survey data
#>                           
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script loads DATRAS survey data and performs some basic data cleaning 
#> and manipulation. This script also merges the hh and hl exchange data into 
#> hlhh which is used throughout further scripts. ICES divisions are also added
#> to the survey data so that we know what rectangle belongs in which division. 
#> This is important when we come to filter survey data to a stocks management 
#> area (e.g. 27.7d for ple.27.7d). We calculate indicators later on based on 
#> data from within the management area only. 

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)
library(stringr)

rm(list = ls())

# Set-up ####
load.path <- paste0(getwd(), "/Data/Initial/")
save.path <- paste0(getwd(), "/Data/Generated/")

stksurveys <- read_xlsx(paste0(load.path, "DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")
refpts     <- read_xlsx(paste0(load.path, "DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Stocks")

load(paste0(load.path, "/ICES Divs/ices_divs.rds"))
load(paste0(load.path, "/ICES Rect/ices_rect.rds"))

source(paste0(getwd(),"/Functions/dataprep_funs.R"))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Check stock divisions exist in ICES rect shapefile
areas    <- sort(unique(ices_rect$Area_27))
divs     <- unique(unlist(strsplit(stksurveys$Divisions, ", ")))
missdivs <- divs[!divs %in% areas]
missdivs
rm(areas, divs, missdivs)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# load(paste0(save.path, "/stks.rds")) # saved stks from data_2_DownloadDATRAS
# stks <- "ank.27.78abd"               # method 1, to run for individual stocks
stks <- "all.stocks"                   # method 2, USE THIS

# Start with clear directory (might be issues with permissions)
#do.call(file.remove, list(list.files(paste0(save.path, "DR_Stocks/SurveyData/Cleaned/"), full.names = TRUE)))

# Clean survey data ####
for (i in 1:length(stks)) {
  stk <- stks[i]
  message(stk)
  
  if(stk == "all.stocks") {
    srvy.list <- unique(str_split_i(list.files(paste0(load.path, "DR_Stocks/SurveyData/", stk)), pattern = ".Yr", i=1))
  } else {
    srvys <- stksurveys %>%
      filter(StockKeyLabel == stk) %>%
      select(SpeciesCommonName, SurveyAcronymn, SurveyRefNo, SurveyIndex, YearStart, YearEnd, Quarter)
    srvy.list <- unique(srvys$SurveyAcronymn)
  }
  
  # Create directory to save cleaned survey data
  suppressWarnings(dir.create(paste0(save.path, "DR_Stocks/SurveyData/Cleaned/", stk), recursive = T))
  
  for (srvy in 1:length(srvy.list)) {
    
    srv <- srvy.list[srvy]
    message(srv)
    
    # Load survey data 
    files <- list.files(paste0(load.path, "DR_Stocks/SurveyData/", stk), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = TRUE)

    load(files[str_detect(files, "\\.HH--")])
    load(files[str_detect(files, "\\.HL--")])
    load(files[str_detect(files, "\\.CA--")])
    
    # Remove duplicates
    hh <- unique(hh)
    hl <- unique(hl)
    ca <- unique(ca)
    
    # Add ICES Divisions
    area_div <- dplyr::distinct(ices_rect[c("ICESNAME", "Area_27", "Shape_Area")])
    hh <- merge.data.frame(hh, area_div, by.x = "StatRec", by.y = "ICESNAME", all.x = TRUE)
    ca <- merge.data.frame(ca, area_div, by.x = "AreaCode", by.y = "ICESNAME", all.x = TRUE)
    
    # Edit -9 NA placeholder
    hl$TotalNo[hl$TotalNo == -9] <- NA
    hl$HLNoAtLngt[hl$HLNoAtLngt == -9] <- NA
    
    # Remove invalid hauls
    hh <- filter(hh, !HaulVal %in% c("I", "P")) # p = partly valid, it is deprecated 
    
    # Create haul.id
    hh$haul.id <- as.character(
      paste(hh$Year, hh$Quarter, hh$Country, hh$Ship, hh$Gear, hh$StNo, hh$HaulNo, 
            sep = ":"))
    hl$haul.id <- as.character(
      paste(hl$Year, hl$Quarter, hl$Country, hl$Ship, hl$Gear, hl$StNo, hl$HaulNo, 
            sep = ":"))
    ca$haul.id <- as.character(
      paste(ca$Year, ca$Quarter, ca$Country, ca$Ship, ca$Gear, ca$StNo, ca$HaulNo, 
            sep = ":"))
    
    # Merge HL HH 
    m <- hh[c("haul.id", "Year", "Quarter", "Month", "Survey","Country", "Ship", 
              "Gear", "GearEx", "DoorType", "HaulDur", "HaulNo", "StNo", "SweepLngt", 
              "StatRec", "Area_27", "ShootLong", "ShootLat", "HaulVal", "Depth")]
    hlhh <- merge(hl, dplyr::distinct(m),
                  c("haul.id", "Year", "Quarter", "HaulNo", "StNo", "Gear", "GearEx", 
                    "DoorType","Ship", "SweepLngt", "Country", "Survey"))
    
    hlhh <- hlhh %>% 
      group_by(haul.id, Valid_Aphia) %>%
      mutate(SumHLNoAtLngt = sum(HLNoAtLngt),
             "SumHlNoAtLngt - TotalNo" = SumHLNoAtLngt - TotalNo) # should = 0    
    
    #any(hlhh$`SumHlNoAtLngt - TotalNo` != 0)
    
    save(hh,   file = paste0(save.path, "DR_Stocks/SurveyData/Cleaned/", stk, "/", namefile(stk, hh)))
    save(hl,   file = paste0(save.path, "DR_Stocks/SurveyData/Cleaned/", stk, "/", namefile(stk, hl)))
    save(ca,   file = paste0(save.path, "DR_Stocks/SurveyData/Cleaned/", stk, "/", namefile(stk, ca)))
    save(hlhh, file = paste0(save.path, "DR_Stocks/SurveyData/Cleaned/", stk, "/", namefile(stk, hlhh, r = "HLHH")))
  
  }
}

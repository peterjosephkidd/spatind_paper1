#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               4. Calculate Spatial Indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(icesVocab)
library(readxl)
library(ggplot2) 

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"

# Load data (numbers might change e.g. 31stks)
stksurveys <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")
ssb.vs.tb <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-AllSurveyData-manual.xlsx"), sheet = "Stocks")

refpts     <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-69stks-AY2022-stkdescrptn.xlsx"))
sa_data    <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-69stks-AY2022-SA-data.xlsx"))

load(paste0(load.path, "Data/DR_Stocks/FishBaseMaturity/FishBase-L50.rds"))
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Divs/ices_divs.rds")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")

# Load functions
source(paste0(load.path, "Functions/spatinds_funs.R")) # for computing spatial indicators

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                                  Run Loop
#>                    
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>                    

load(paste0(save.path, "SurveyData/stks.rds")) # saved stks from data_2_DownloadDATRAS

stksurveys_full <- stksurveys %>%
  filter(InDatras == 1) %>%
  select(-c(Ship, Country, YrsExclude, Ages, inRcntStkAnX, inMatCalc, MatYrs, Usage, Notes, `Full Name`)) %>%
  na.omit()

# Categorise indicators
loc <- c("CoG (x)", "CoG (y)")
ran <- c("Inertia", "EOO", "ELA")
occ <- c("POPR", "POPH")
agg <- c("Gini Index", "D95", "SA", "EA", "SPI")

# SSB or Total Biomass
stksurveys_full <- left_join(stksurveys_full, ssb.vs.tb[c("StockKeyLabel", "Type")], by = "StockKeyLabel")

spatinds <- data.frame()
wrnlog <- list()

for(i in c(1:length(stks))){
  stk <- stks[i]
  #stk <- "lez.27.4a6a"
  #i <- which(stks == stk)
  
  if (stk == "lez.27.4a6a") {
    message(paste0("\n", stk, " is a combined stock of two species of megrim"))
    species_aphia <- c(findAphia("Lepidorhombus boscii", latin = T), findAphia("Lepidorhombus whiffiagonis", latin = T))
    message(paste0(length(species_aphia), " Valid_Aphia codes successfully retrieved\n"))
  } else {
    species_aphia <- findAphia(unique(stksurveys_full[stksurveys_full$StockKeyLabel == stk,]$SpeciesScientificName), latin = TRUE)
  }
  
  writeLines(paste0(rep("#", 100), collapse = ""))
  message(paste0(i, ": ", stk))
  
  srvys <- stksurveys_full[stksurveys_full$StockKeyLabel == stk,]
  srvys.list <- unique(srvys$SurveyAcronymn)
  
  if (all(srvys$Type == "Total Biomass")) {
    mtr <- FALSE
    L50levels <- "mean" # this is just so data can be loaded once, 
                        # we dont use L50 mean info anywhere when mtr = FALSE
    
  } else if (all(srvys$Type == "SSB")) {
    mtr <- TRUE
    L50levels <- c("mean", "lowerCI", "upperCI")
    
  } else { 
    warning("Biomass measure not consitently specified. Defualting mtr = TRUE")
    mtr <- TRUE
    L50levels <- c("mean", "lowerCI", "upperCI")}
  
  for (j in 1:length(srvys.list)) {
    
    srv <- srvys.list[j]
    #srv <- "NIGFS"
    #j <- which(srvys.list == srv)
    
    for (lvl in 1:length(L50levels)) {
    
      message(paste0(i, ".", j, ": ", stk, ", ", srv))
      
      files <- list.files(paste0(load.path, "Data/DR_Stocks/SurveyData/", stk, "/matures/"), pattern = paste0("^", srv, "\\.Yr.*L50\\.", L50levels[lvl]), full.names = T)
      do.call(list, lapply(files, load, envir = .GlobalEnv))
      indices <- srvys[srvys$SurveyAcronymn == srv,]
      
      for(ind in 1:nrow(indices)){
        srvindx <- indices[ind,]
        index <- srvindx$SurveyIndex
        
        message(paste0(i, ".", j, ".", ind, ": ", stk, ", ", srv, ", ", index, " (maturity = ", mtr, ", L50 = ", L50levels[lvl], ")"))
        
        yrs <- srvindx$YearStart:srvindx$YearEnd
        qrs <- srvindx$Quarter
        stk_divs <- unlist(strsplit(srvindx$Divisions, ", "))
        
        if(paste(stk_divs, collapse = ", ") == "NEA"){stk_divs = unique(ices_rect$Area_27)[!is.na(unique(ices_rect$Area_27))]}
        
        if(index == "BTS-Isis") {
          hlhh <- filter(hlhh, Ship == "64SS")
        }
        
        check <- hlhh %>%
          ungroup() %>%
          filter(Area_27 %in% stk_divs,
                 Year %in% c(yrs),
                 Quarter %in% c(qrs),
                 HaulVal != "I") 
        
        n.data <- check %>%
          group_by(Year, Quarter) %>%
          count() %>%
          mutate(Quarter = as.character(Quarter),
                 StockKeyLabel = stk,
                 SurveyName = srv, 
                 SurveyIndex = index,
                 AreaList = paste0(stk_divs, collapse = ", "))
        
        if (nrow(check) > 0) {
          
          # Centre of Gravity (CoG) and Inertia
          writeLines("CoG & Inertia")
          cginert <- coginis(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr,
                   cog = T, inertia = T, iso = F, density = T)
          
          # Extent of Occurrence (EOO)
          writeLines("EOO")
          
          eoo <- chullarea(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)[1:3]
          
          # Ellipse Area (ELA)
          writeLines("ELA")
          
          ela <- ellarea(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
          
          # Proportion of Presence
          # Rectangle (POPR)
          writeLines("POPR")
          
          popr <- pa_rect(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
          
          # Haul (POPH)
          writeLines("POPH")
          
          poph <- pa_haul(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
          
          # Gini index
          writeLines("Lorenz")
          
          lorenz <- lorenz_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
          writeLines("Gini")
          
          gni <- Gini(lorenz, matures = mtr)
          
          # D95
          writeLines("D95")
          
          D95 <- d95(lorenz)
          
          # Spreading Area (SA) & Equivalent Area (EA)
          writeLines("SA & EA")
          
          sa_data <- spreadingarea_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
          
          sa <- sa_data %>%
            group_by(Year) %>%
            {if (mtr == FALSE)
                  summarise(., "Spreading Area"  = spreadingarea_calc(TotalNo_Dur),
                            "Equivalent Area" = equivalentarea(TotalNo_Dur))
            else (summarise(., "Spreading Area"  = spreadingarea_calc(TotalNoMature_Dur),
                            "Equivalent Area" = equivalentarea(TotalNoMature_Dur)))
              } %>%
            mutate(Quarter = paste(as.character(sort(unique(sa_data$Quarter))), collapse = ", ")) %>% 
            relocate(Year, Quarter) 
          
          # Spread of Participation Index
          writeLines("SPI")
          
          SPI <- spi(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)[c(1,2,4)]
          
          message("All spatial indicators calculated\n")
          # Combine ouptuts
          df_list <- list(cginert, eoo, ela, popr, poph, gni, D95, sa, SPI) 
          
          sidf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) %>%
                  select(-c(nrects, nrects_p, no_haul.ids, pr_hauls)) %>% # remove some cols
                  rename(EOO = convex_hull_area,
                         POPR = PosAreaR,
                         POPH = PosAreaH,
                         ELA = `Ellipse Area`,
                         SPI = SPI.dur,
                         SA = `Spreading Area`,
                         EA = `Equivalent Area`) %>%
            mutate(StockKeyLabel = stk,
                   SurveyName    = srv,
                   SurveyIndex   = index,
                   ValidAphia    = paste0(species_aphia, collapse = ", "),
                   AreaList      = paste0(stk_divs, collapse = ", ")) 
          
          # Add numer of data points in each yr/qr combo
          sidf <- full_join(sidf, n.data, by = c("Year", "Quarter", "StockKeyLabel", "SurveyName", "SurveyIndex", "AreaList")) %>%
            relocate(StockKeyLabel, SurveyName, SurveyIndex, ValidAphia, Year, Quarter, n)
          sidf <- cbind (sidf, mtr, L50lvl = L50levels[lvl])
          
          spatinds <- rbind(spatinds, sidf)
          
        } else {
          msg <- paste0("No data - filling row with NAs\n", stk, ": ", srv, "; ", index, "; Yrs ", paste0(range(yrs), collapse = "-"), ", Qrs ", paste0(qrs, collapse = ", "))
          wrnlog <- rbind(wrnlog, msg)
          
          message(msg)
          
          sidf <- data.frame("StockKeyLabel" = stk, "SurveyName" = srv, "SurveyIndex" = index, 
           "ValidAphia" = species_aphia, "Year" = NA, "Quarter" = qrs, "n" = NA,
           "CoG (x)" = NA, `CoG (y)` = NA, "Inertia" = NA, "EOO" = NA, "ELA" = NA,
           "POPR" = NA, "POPH" = NA, "Gini Index" = NA, "D95" = NA, 
           "SA" = NA, "EA" = NA, "SPI" = NA, "AreaList" = paste0(stk_divs, collapse = ", "), 
           "mtr" = NA, "L50lvl" = NA, check.names = F)
          spatinds <- rbind(spatinds, sidf)
          print("###################")
        }
        writeLines(paste0(rep("-", 100), collapse = ""))
        
      }
    }
  }
}


wrnlog
View(spatinds)

# Summary of data
spatinds %>%
  group_by(StockKeyLabel, SurveyName, SurveyIndex, Quarter, ValidAphia, mtr, L50lvl) %>%
  summarise(YrRange = paste0(range(Year), collapse = "-"),
            N.Data = sum(n, na.rm = T)) %>%
  arrange(Quarter, SurveyName, StockKeyLabel, L50lvl) %>%
  print(n = nrow(.))

suppressWarnings(dir.create(paste0(save.path, "Outputs/SpatInds"), recursive = TRUE))
save(spatinds, file = paste0(save.path, "Outputs/SpatInds/Spatial.indicators.rds"))

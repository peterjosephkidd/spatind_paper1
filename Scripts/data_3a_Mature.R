#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               3.a. Filter survey data to mature individuals
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script is used to identify mature individuals in the survey data for 
#> each of the target species. A binary column is added to signal mature/immature.
#> The total number of matures is then calculated which is used later within 
#> some spatial indicators. The idea is that we want to be calculating indicators
#> for the mature component of the species because we are comparing indicators
#> to spawning stock biomass (SSB). To do this, we use estimates of L50 to 
#> identify mature individuals. These estimates are not precise. To account for
#> uncertainty, the process of identifying mature individuals and calculating 
#> total number of matures is performed three times using different estimates of 
#> L50. (1) mean L50, (2) lower confidence interval of L50, (3) and an upper 
#> confidence interval of L50. Later, we will see if the indicators differ depending
#> on the L50 value used to identify mature individuals. We also take some other
#> life history parameters (e.g. Linf)
#>

#There are compatability problems with `FishBase`, so install an earlier
#version. These have been fixed now. see
#<https://james-thorson-noaa.github.io/FishLife/>

#remotes::install_github("ropensci/rfishbase",ref="d150f2e0f5")
#remotes::install_github("james-thorson/FishLife")
#devtools::install_github("henning-winker/SPMpriors")

library(writexl)
library(readxl)
library(icesDatras)
library(icesVocab)
library(dplyr)
library(remotes)
library(rfishbase)
library(SPMpriors)
library(FishLife)
library(fishtree)

rm(list = ls())

#save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

load.path <- paste0(getwd(), "/Data/Initial/")
save.path <- paste0(getwd(), "/Data/Generated/")

stksurveys <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")
refpts     <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Stocks")

source(paste0(getwd(),"/Functions/dataprep_funs.R"))

# Taxonomic database
#taxizedb::db_download_ncbi(verbose = TRUE, overwrite = FALSE)
#taxizedb::db_path("ncbi")

# Get life history information from FishBase
Lmat <- data.frame()

for (i in 1:nrow(refpts)) {
  
  stk <- refpts[i,]
  genus <- strsplit(stk$SpeciesScientificName, " ")[[1]][1]
  species <- strsplit(stk$SpeciesScientificName, " ")[[1]][2]
  
  if(is.na(species)){
    taxon_data <- fishbase %>% 
      filter(Genus == genus) %>%
      select(Family, Order, Class) %>%
      distinct() %>%
      mutate(Genus = genus,
             Species = NA)
    } else {
      taxon_data <- fishbase %>% 
        filter(Genus == genus, Species == species) %>%
        select(Family, Order, Class) %>%
        mutate(Genus = genus,
               Species = species)
    }
  
  # Use tryCatch to handle errors
  tryCatch ({
    message(paste0(taxon_data$Genus, " ", taxon_data$Species))
    par <- flmvn_traits(genus, species, Plot = FALSE)
    L50 <- par$traits[3,]
    Linf <- par$traits[1,2]
    Lmat_Linf <- L50[[4]]/Linf
    output <- cbind(select(stk, StockKeyLabel, SpeciesCommonName, SpeciesScientificName), L50, Linf, Lmat_Linf, taxon_data)
    Lmat <- rbind(Lmat, output)
  }, error = function(e) {
    # Handle the error gracefully, e.g., print a message
    cat("Error occurred for:", stk$SpeciesCommonName, "\nError message:", conditionMessage(e), "\n")
  })
  next
  rm(output, par, stk, L50, genus, species)
}

head(Lmat)

# Do scientific names match with those from FishBase?
all(Lmat$SpeciesScientificName == paste0(Lmat$Genus, " ", Lmat$Species))

# Did we get the expected number of outputs? No
nrow(Lmat)
nrow(refpts)
# Which species did we not retrieve information for? Nephrops and megrim
refpts[!refpts$StockKeyLabel %in% Lmat$StockKeyLabel,]
# One nephrops (which we do not assess and ignore) and one megrim stock

# lez.27.4a6a consists of two species 
# Lepidorhombus whiffiagonis and L. boscii. 
# We deal with this later after we identify matures in survey data for the rest of the stocks

################################################################################
# Identify Matures in Survey Data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
#
# This code takes a long while and is highly inefficient 
# For each survey used for each stock, 3 copies of the survey data are created. One for each L50 condition. 
# e.g. if 2 stocks have 4 surveys each then there will 2 x 4 x 3 copies of HH, HL, CA, and HLHH (each!)  
#
# Stocks with surveys that are not in survey_names (i.e. DATRAS) will not have a local file created
# and are not assessed any further. This reduces total number of stocks from 69 to 39
################################################################################

stks <- Lmat$StockKeyLabel
#stks <- "ank.27.78abd"

method2 <- TRUE # method used in data_2_DownlaodDATRAS.R and data_3_CleanExchange.R
L50levels <- data.frame("mean" = 7, "lowerCI" = 9, "upperCI" = 10) # index locations to get L50 values out of Lmat
survey_names <- list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Cleaned/all.stocks/"))
survey_names <- unique(stringr::str_remove(survey_names, "\\.Yr.*"))
error_messages <- list()


for (i in 1:length(stks)) {
  
  stk <- stks[i]
  message(paste0("\n",i,": ", stk))
  stkLmat <- Lmat[Lmat$StockKeyLabel == stk,]
  species <- stkLmat$SpeciesCommonName
  species_aphia <-  findAphia(stkLmat$SpeciesScientificName, latin = TRUE)
  srvys <- unique(stksurveys[stksurveys$StockKeyLabel == stk,]$SurveyAcronymn)
  srvys_avail <- srvys[srvys %in% survey_names]
  
  if (any(srvys %in% survey_names)) {
    suppressWarnings(dir.create(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk), recursive = T))
  }
    

  for (j in 1:length(srvys_avail)) {
    for (lvl in 1:length(L50levels)) {
      
      tryCatch({
        L50 <- stkLmat[L50levels[[lvl]]] # mean, lower, upper
        srv <- srvys_avail[j]
        message(paste0(i,".",j,": ", srv, " (L50 = ", names(L50levels[lvl]), ")"))
        suppressWarnings(rm(hlhh, ca, hl, hh))
      
        if (method2 == TRUE){
          files <- list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Cleaned/all.stocks/"), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
        } else{
          files <- list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Cleaned/", stk), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
        }
      
        do.call(list, lapply(files, load, envir = .GlobalEnv))
        
        hlhh <- filter_to_matures(hlhh, species, species_aphia, L50, is.hlhh = TRUE)
        ca   <- filter_to_matures(ca,   species, species_aphia, L50, is.hlhh = FALSE)
        
        save(hh,   file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, hh,   r = paste0("HH.L50.",   names(L50levels[lvl])) )))
        save(hl,   file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, hl,   r = paste0("HL.L50.",   names(L50levels[lvl])) )))
        save(ca,   file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, ca,   r = paste0("CA.L50.",   names(L50levels[lvl])) )))
        save(hlhh, file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, hlhh, r = paste0("HLHH.L50.", names(L50levels[lvl])) )))
        message ("- Files saved")
        
      }, error = function(e){
        error_messages <<- c(error_messages, conditionMessage(e))
        message (paste0("Error: ", stk, ", ", srv, " data not available."))
      }, finally = {
        message ("- Complete\n")
      })
      next
    }
  }
}

################################################################################
#> lez.27.4a6a - manual intervention -------------------------------------------
#> 
#> we identify mature megrim based upon the L50 values for both L.whif and
#> L.bosci species of megrim. The datasets for each species 
#> (e.g. ca.bosci and ca.whif) are combined into a single dataset.
################################################################################
error_messages <- list()
species_aphia.whif <-  findAphia("Lepidorhombus whiffiagonis", latin = TRUE)
species_aphia.bosc <-  findAphia("Lepidorhombus boscii", latin = TRUE)

par.whif <- flmvn_traits(Genus = "Lepidorhombus", Species = "whiffiagonis", Plot = FALSE)
par.bosc <- flmvn_traits(Genus = "Lepidorhombus", Species = "boscii", Plot = FALSE)
L50.whif <- par.whif$traits[3,]
L50.bosc <- par.bosc$traits[3,]

stk <- "lez.27.4a6a"
i <- which(stks == stk)
srvys <- unique(stksurveys[stksurveys$StockKeyLabel == stk,]$SurveyAcronymn)
srvys_avail <- srvys[srvys %in% survey_names]

if (any(srvys %in% survey_names)) {
  suppressWarnings(dir.create(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk), recursive = T))
}

for (j in 1:length(srvys_avail)) {
  srv <- srvys_avail[j]
  suppressWarnings(rm(hlhh, ca, hl, hh))
  
  if (method2 == TRUE){
    files <- list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Cleaned/all.stocks/"), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
  } else{
    files <- list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Cleaned/", stk), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
  }
  
  for (lvl in 1:length(L50levels)) {
      
    tryCatch({
      message(paste0(i,".",j,": ", srv, " (L50 = ", names(L50levels[lvl]), ")"))
      
      #message(paste0("\nL50: "), names(L50levels[lvl]), "\n")
      
      do.call(list, lapply(files, load, envir = .GlobalEnv))
  
      hlhh.whif <- filter_to_matures(hlhh, "Megrim (combined)", species_aphia.whif, L50.whif[L50levels[[lvl]]-3], is.hlhh = TRUE)
      ca.whif   <- filter_to_matures(ca,   "Megrim (combined)", species_aphia.whif, L50.whif[L50levels[[lvl]]-3], is.hlhh = FALSE)
      
      hlhh.bosc <- filter_to_matures(hlhh, "Megrim (combined)", species_aphia.bosc, L50.bosc[L50levels[[lvl]]-3], is.hlhh = TRUE)
      ca.bosc   <- filter_to_matures(ca,   "Megrim (combined)", species_aphia.bosc, L50.bosc[L50levels[[lvl]]-3], is.hlhh = FALSE)
      
      hlhh <- merge(hlhh.whif, hlhh.bosc, by = names(hlhh.whif)[1:38], suffixes = c(".whifiagonis", ".boscii"))
      ca <-   merge(ca.whif, ca.bosc,     by = names(ca.whif)[1:38],   suffixes = c(".whifiagonis", ".boscii"))
      
      hlhh <- hlhh %>% 
        mutate(TrgtSpcsMature = ifelse(!is.na(TrgtSpcsMature.whifiagonis) & TrgtSpcsMature.whifiagonis == 1 | !is.na(TrgtSpcsMature.boscii) & TrgtSpcsMature.boscii == 1, 1, 
                                ifelse(!is.na(TrgtSpcsMature.whifiagonis) & TrgtSpcsMature.whifiagonis == 0 | !is.na(TrgtSpcsMature.boscii) & TrgtSpcsMature.boscii == 0, 0, NA)),
               TotalNoMature =  ifelse(is.na(TotalNoMature.whifiagonis) & is.na(TotalNoMature.boscii), NA, rowSums(.[c("TotalNoMature.whifiagonis", "TotalNoMature.boscii")], na.rm = TRUE)))
      
      ca <- ca %>%
        mutate(TrgtSpcsMature = ifelse(!is.na(TrgtSpcsMature.whifiagonis) & TrgtSpcsMature.whifiagonis == 1 | !is.na(TrgtSpcsMature.boscii) & TrgtSpcsMature.boscii == 1, 1, 
                                ifelse(!is.na(TrgtSpcsMature.whifiagonis) & TrgtSpcsMature.whifiagonis == 0 | !is.na(TrgtSpcsMature.boscii) & TrgtSpcsMature.boscii == 0, 0, NA)))
    
      save(hh,   file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, hh,   r = paste0("HH.L50.",   names(L50levels[lvl])) )))
      save(hl,   file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, hl,   r = paste0("HL.L50.",   names(L50levels[lvl])) )))
      save(ca,   file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, ca,   r = paste0("CA.L50.",   names(L50levels[lvl])) )))
      save(hlhh, file = paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/", stk, "/", namefile(stk, hlhh, r = paste0("HLHH.L50.", names(L50levels[lvl])) )))
      
      message("- Files saved")
    }, error = function(e){
      error_messages <<- c(error_messages, conditionMessage(e))
      message(paste0("Error: ", stk, ", ", srv, " data not available."))
    }, finally = {
      message("- Complete\n")
    })
    next
  }
}

load(paste0(getwd(), "/Data/Generated//DR_Stocks/SurveyData/Matures/", stk, "/SWC-IBTS.Yr1985-2010.Q1.Q4.HLHH.L50.upperCI--lez.27.4a6a.rds"))
load(paste0(getwd(), "/Data/Generated//DR_Stocks/SurveyData/Matures/", stk, "/SWC-IBTS.Yr1985-2010.Q1.Q4.HLHH.L50.mean--lez.27.4a6a.rds"))
load(paste0(getwd(), "/Data/Generated//DR_Stocks/SurveyData/Matures/", stk, "/SWC-IBTS.Yr1985-2010.Q1.Q4.HLHH.L50.lowerCI--lez.27.4a6a.rds"))
L50.bosc
L50.whif
unique(hlhh$TrgtSpcsL50.boscii)
unique(hlhh$TrgtSpcsL50.whifiagonis)

# Output tables for report >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

# Save L50 data
Lmat <- rbind(Lmat, cbind("StockKeyLabel" = "lez.27.4a6a", 
                          "SpeciesCommonName" = "Megrim (combined)", 
                          "SpeciesScientificName" =  "Lepidorhombus whiffiagonis & L.boscii", 
                          Lmat[Lmat$StockKeyLabel == "meg.27.8c9a", 4:17],
                          "Species" = "whiffiagonis & boscii (combined)"))

suppressWarnings(dir.create(paste0(getwd(), "/Data/Generated/DR_Stocks/FishBaseMaturity"), recursive = T))
save(Lmat, file = paste0(paste0(getwd(), "/Data/Generated/DR_Stocks/FishBaseMaturity/FishBase-L50.rds")))
          
# Final stocks 
stk_names <- list.files(paste0(getwd(), "/Data/Generated/DR_Stocks/SurveyData/Matures/"))
suppressWarnings(dir.create(paste0(getwd(), "/Data/Generated/DR_Stocks/StockNames"), recursive = T))
stk_names
save(stk_names, file = paste0(paste0(getwd(), "/Data/Generated/DR_Stocks/StockNames/stk_names_3a.rds")))


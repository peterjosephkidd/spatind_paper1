#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               3.a. Filter survey data to mature individuals
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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

load.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/SurveyData/"

refpts     <- read_xlsx(paste0(load.path, "icesSA_data/icesData-69stks-AY2022-stkdescrptn.xlsx"))
stksurveys <- read_xlsx(paste0(load.path, "icesSA_data/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")

source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/dataprep_funs.R")

# Taxonomic database
db_download_ncbi(verbose = TRUE, overwrite = FALSE)
db_path("ncbi")

# Get L50 from FishBase
Lmat <- data.frame()

for (i in 1:nrow(refpts)) {
  stk <- refpts[i,]
  genus <- strsplit(stk$SpeciesScientificName, " ")[[1]][1]
  species <- strsplit(stk$SpeciesScientificName, " ")[[1]][2]
  
  if(is.na(Species)){
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
  rm(output, par, stk, L50, Genus, Species)
}

head(Lmat)
nrow(Lmat)
nrow(refpts)
refpts[!refpts$StockKeyLabel %in% Lmat$StockKeyLabel,]

# lez.27.4a6a consists of two species 
# Lepidorhombus whiffiagonis and L. boscii. 
# We deal with this below



# Method 1: Individual Stocks >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
load(paste0(save.path, "/stks.rds")) # saved stks from data_2_DownloadDATRAS
#stks <- Lmat$StockKeyLabel
#stks <- "ank.27.78abd"
method2 <- TRUE # method used in data_2_DownlaodDATRAS.R and data_3_CleanExchange.R
L50levels <- data.frame("mean" = 7, "lowerCI" = 9, "upperCI" = 10) # index locations
error_messages <- list()

for (i in 1:length(stks)) {
  stk <- stks[i]
  #stk <- "mac.27.nea"
  #i <- which(stks == stk)
  #sapply(list.files(paste0(save.path, stk, "/matures"), full.names = TRUE), file.remove) # clear directories
  message(paste0("\n",i,": ", stk))
  stkLmat <- Lmat[Lmat$StockKeyLabel == stk,]
  species <- stkLmat$SpeciesCommonName
  species_aphia <-  findAphia(stkLmat$SpeciesScientificName, latin = TRUE)
  srvys <- unique(stksurveys[stksurveys$StockKeyLabel == stk,]$SurveyAcronymn)
  suppressWarnings(dir.create(paste0(save.path, stk, "/matures"), recursive = T))
    
    for (j in 1:length(srvys)) {
      for (lvl in 1:length(L50levels)) {
      L50 <- stkLmat[L50levels[[lvl]]] # mean, lower, upper
      srv <- srvys[j]
      message(paste0(i,".",j,": ", srv, " (L50 = ", names(L50levels[lvl]), ")"))
      suppressWarnings(rm(hlhh, ca, hl, hh))
      
      if (method2 == TRUE){
        files <- list.files(paste0(load.path, "SurveyData/all.stocks/clean/"), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
      } else{
        files <- list.files(paste0(load.path, "SurveyData/", stk, "/clean/"), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
      }
      
      tryCatch ({
        do.call(list, lapply(files, load, envir = .GlobalEnv))
        hlhh <- filter_to_matures(hlhh, species, species_aphia, L50, is.hlhh = TRUE)
        ca   <- filter_to_matures(ca,   species, species_aphia, L50, is.hlhh = FALSE)
        save(hh,   file = paste0(save.path, stk, "/matures/", namefile(stk, hh,   r = paste0("HH.L50.",   names(L50levels[lvl])) )))
        save(hl,   file = paste0(save.path, stk, "/matures/", namefile(stk, hl,   r = paste0("HL.L50.",   names(L50levels[lvl])) )))
        save(ca,   file = paste0(save.path, stk, "/matures/", namefile(stk, ca,   r = paste0("CA.L50.",   names(L50levels[lvl])) )))
        save(hlhh, file = paste0(save.path, stk, "/matures/", namefile(stk, hlhh, r = paste0("HLHH.L50.", names(L50levels[lvl])) )))
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

# lez.27.4a6a - manual intervention
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
suppressWarnings(dir.create(paste0(save.path, stk, "/matures"), recursive = T))
#sapply(list.files(paste0(save.path, stk, "/matures"), full.names = TRUE), file.remove) # clear directory

for (j in 1:length(srvys)) {
  
  srv <- srvys[j]
  suppressWarnings(rm(hlhh, ca, hl, hh))
  
  if (method2 == TRUE){
    files <- list.files(paste0(load.path, "SurveyData/all.stocks/clean/"), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
  } else{
    files <- list.files(paste0(load.path, "SurveyData/", stk, "/clean/"), pattern = paste0("^", srv, "\\.Yr", "*"), full.names = T)
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
  
    save(hh,   file = paste0(save.path, stk, "/matures/", namefile(stk, hh,   r = paste0("HH.L50.",   names(L50levels[lvl])) )))
    save(hl,   file = paste0(save.path, stk, "/matures/", namefile(stk, hl,   r = paste0("HL.L50.",   names(L50levels[lvl])) )))
    save(ca,   file = paste0(save.path, stk, "/matures/", namefile(stk, ca,   r = paste0("CA.L50.",   names(L50levels[lvl])) )))
    save(hlhh, file = paste0(save.path, stk, "/matures/", namefile(stk, hlhh, r = paste0("HLHH.L50.", names(L50levels[lvl])) )))
    
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

load(paste0(save.path, stk, "/matures/SWC-IBTS.Yr1985-2010.Q1.Q4.HLHH.L50.upperCI--lez.27.4a6a.rds"))
load(paste0(save.path, stk, "/matures/SWC-IBTS.Yr1985-2010.Q1.Q4.HLHH.L50.mean--lez.27.4a6a.rds"))
load(paste0(save.path, stk, "/matures/SWC-IBTS.Yr1985-2010.Q1.Q4.HLHH.L50.lowerCI--lez.27.4a6a.rds"))
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

suppressWarnings(dir.create(paste0(load.path, "/FishBaseMaturity"), recursive = T))
save(Lmat, file = paste0(load.path, "/FishBaseMaturity/FishBase-L50.rds"))
          








#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>       THIS IS SOMETHING TO PUT INTO THE DATA_3_CLEANEXCHNG.R SCRIPT
#>        
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#> I noticed that some of the summations of HLNoAtLength does not equal TotalNo
#> Sometimes it is greater than TotalNo
#> Sometimes by a large amount e.g. 3000 but usually by 1-10
#> This is going to require some manual intervention to look at fix for
#> every survey data I download
#> It appears to me the error is in the TotalNo, not in my code. 
#> For most datapoints HLNoAtLngt is equal to TotalNo
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
any(hlhh$`SumHlNoAtLngt - TotalNo` > 0)

fails <- hlhh[hlhh$`SumHlNoAtLngt - TotalNo` > 0,]
nrow(fails)
nrow(hlhh[hlhh$`SumHlNoAtLngt - TotalNo` == 0,])
nrow(hlhh[hlhh$`SumHlNoAtLngt - TotalNo` < 0,])

fails$YearPlot <- as.character(fails$Year)
  
ggplot(data = fails) +
  geom_histogram(aes(x = `SumHlNoAtLngt - TotalNo`, fill = YearPlot), bins = 200) +
  geom_vline(xintercept = 10)

topfails <- hlhh[hlhh$`SumHlNoAtLngt - TotalNo` > 10,] 

ggplot(data = topfails) +
  geom_histogram(aes(x = `SumHlNoAtLngt - TotalNo`, fill = YearPlot), bins = 200)

  tst <- hlhh %>% 
    #filter(Valid_Aphia == va) %>%
    select(Valid_Aphia, haul.id, Year, Quarter, HaulNo, TotalNo, HLNoAtLngt, 
           LngtClass, TrgtSpcsL50, TrgtSpcsMature, TotalNoMature, PropMature, 
           SumHLNoAtLngt, `TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`,
           `SumHlNoAtLngt - TotalNo`) %>%
    arrange(haul.id)
  
  fails <- tst[tst$`SumHLNoAtLngt <= TotalNo` == "Fail",]
  
  # In which years and quarters
  table(fails$Quarter, fails$Year)
  # Which species
  failspecies <- unique(topfails$Valid_Aphia)
  # Any target species?
  va %in% failspecies
  # Which year and quarters for target species
  table(fails[fails$Valid_Aphia == va,]$Quarter, fails[fails$Valid_Aphia == va,]$Year)
  # Have a look
  TrgtSpcsFails <- fails[fails$Valid_Aphia == va,]
  # Biggest difference
  max(TrgtSpcsFails$`SumHlNoAtLngt - TotalNo`, na.rm = T)
  # have a look
  TrgtSpcsFails  %>%
    select(Valid_Aphia, haul.id, Year, Quarter, HaulNo, TotalNo, HLNoAtLngt, 
           LngtClass, TrgtSpcsL50, TrgtSpcsMature, TotalNoMature, PropMature, 
           SumHLNoAtLngt, 
           #`TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`,
           `SumHlNoAtLngt - TotalNo`) %>%
    arrange(haul.id) %>%
    print(n = 150)
  # Are there case where TotalNo within a single haul.id is not consistent 
  checkTotNo <- TrgtSpcsFails %>%
    ungroup() %>%
    select(haul.id, TotalNo) %>%
    distinct()
  any(duplicated(checkTotNo$haul.id))
  # What about for all species
  checkTotNo_all <- fails %>%
    ungroup() %>%
    select(haul.id, TotalNo) %>%
    distinct()
  any(duplicated(checkTotNo$haul.id))
  duphauls <- checkTotNo_all[duplicated(checkTotNo_all$haul.id),]
  
  fails %>%
    filter(haul.id == duphauls$haul.id[3]) %>%
    arrange(haul.id, Valid_Aphia) %>%
    print(n = nrow(.))
  

  fails[fails$Year == "NA",]
  max(fails$SumHLNoAtLngt - fails$TotalNo, na.rm = T)
  
  fails %>%
   arrange(-`SumHlNoAtLngt - TotalNo`) %>%
    select(- c(`TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`))
  
  topfails <- fails %>%
    select(Valid_Aphia, haul.id) %>%
    distinct()
  
  failspecies <- unique(topfails$Valid_Aphia)
  
  
  
  
  hlhh %>%
    filter(haul.id == "2001:1:FR:35HT:GOV:F0023:23",
           Valid_Aphia == 105865) %>%
    select(Valid_Aphia, haul.id, Year, Quarter, HaulNo, TotalNo, HLNoAtLngt, 
           LngtClass, TrgtSpcsL50, TrgtSpcsMature, TotalNoMature, PropMature, 
           SumHLNoAtLngt, 
           #`TotalNo = SumHLNoAtLngt`, `SumHLNoAtLngt <= TotalNo`,
           `SumHlNoAtLngt - TotalNo`)
  
  

  
  tst <- hlhh %>%
    filter(Valid_Aphia ==  va,
           haul.id == "2000:1:DE:06NI:GOV:116:34") 
  sum(tst$HLNoAtLngt)
  tst %>%
    #group_by(haul.id) %>%
    mutate(TotalNoMature = sum(HLNoAtLngt[TrgtSpcsMature == 1]),
           PropMature = TotalNoMature/TotalNo)

  



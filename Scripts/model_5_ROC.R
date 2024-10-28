#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               5. Assess Spatial Indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script takes the indicator calculations from model_4_SpatIndCalc.R
#> and compares them to SSB stock assessment outputs using Receiver Operating
#> Characteristic (ROC curves).

library(dplyr)
library(icesVocab)
library(readxl)
library(ggplot2) 

rm(list = ls())

# Load Data
sa_data  <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-69stks-AY2022-SA-data.xlsx")) # Stock Assessment Data 
load(paste0(getwd(), "/Output/Data/SpatInds/spatinds_4.rds")) # Spatial Indicator Data
load(paste0(getwd(), "/Data/Generated/DR_Stocks/StockNames/stk_names_3a.rds")) # stk_names from data_3a_Mature.R
source(paste0(getwd(), "/Functions/ROC_funs.R")) # ROC Functions

# Check we are not missing any stocks
identical(stk_names, unique(spatinds$StockKeyLabel))
all(stk_names %in% sa_data$StockKeyLabel)

# Combine SSB, MSY Btrigger, and Spatinds data
ssb.ref <- sa_data %>% 
  select(StockKeyLabel, Year, SSB, MSYBtrigger) %>%
  filter(StockKeyLabel %in% stk_names) %>%
  mutate(`SSB/MSYBtrigger` = SSB/MSYBtrigger) %>%
  full_join(., spatinds, by = c("StockKeyLabel", "Year")) %>%
  mutate(SurveyNameIndex = paste0(SurveyName, ", ", SurveyIndex)) %>%
  filter(SurveyNameIndex != "NA, NA")

all(unique(spatinds$StockKeyLabel) %in% unique(ssb.ref$StockKeyLabel))

# Parameters for ROC
state <- "SSB/MSYBtrigger"
inds <- c("CoG (x)", "CoG (y)", "Inertia", "EOO", "ELA", "POPR", "POPH", "Gini Index", "D95", "SA", "EA", "SPI")
roc_longlist <- list()
iter <- 1

# Loop
for (i in 1:length(stk_names)) {
  
  stk <- stk_names[i]
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
        
        # ROC Analysis
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

rocAll_long <- rocAll_long %>% 
  mutate(across(where(is.list), as.numeric),
         across(c(`SSB/MSYBtrigger`, `Spatial Indicator Value`), as.numeric))

str(rocAll_long)

# Check for NAs in AUC
any(is.na(rocAll_long$AUC))
rocAll_long[is.na(rocAll_long$AUC),]

#> We see NaN in FPR. FPR = FP/(FP+TN). If stock status is only positive, then you 
#> cannot compute FPR and thus TSS and AUC. We may see NaN in TPR if stock status is only
#> negative and TPR, TSS, AUC, cannot be computed.
#> 
#> In other rows we see that FP, TP, TN, and FN are filled with NA. This is because
#> the indicator is not calculated for these year
#> 
#> We will get rid of rows where the FPR or TPR = NaN, and rows where the 
#> indicator value = NA

rocAll_long <- rocAll_long %>% 
  filter(!is.nan(TPR),
         !is.nan(FPR),
         !is.na(TP),
         !is.na(FP),
         !is.na(TN),
         !is.na(TN))

any(is.na(rocAll_long$AUC))

# Save
suppressWarnings(dir.create(paste0(getwd(), "/Output/Data/ROC"), recursive = T))
save(rocAll_long, file = paste0(getwd(), "/Output/Data/ROC/ROCdata.rds"))

#> Not all surveys are informative. A long timeseries with contrast in stock
#> status is ideal for telling us whether an indicator can reliably classify 
#> stock status. We filter to surveys with at least 15 years of survey data
#> and that there is contrast of a ratio of max 1:4
## Filter to good surveys

# How much contrast in SSB/MSYB trigger in each time series:
ROContrast <- rocAll_long %>%
  select(StockKeyLabel, Year, Quarter, SurveyNameIndex, status, Value) %>%
  na.omit() %>%
  select(-Value) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter) %>%
  summarise(N.years = length(Year),
            N.poor  = length(which(status == FALSE)),
            N.good  = length(which(status == TRUE)),
            Ratio.P2G   = N.poor/N.good) %>%
  print(n = nrow(.))

hist(ROContrast$Ratio.P2G[ROContrast$Ratio.P2G < 15], breaks = 100)

# Which stocks/surveys should be removed?
## Remove if:
### Less than 15 years in time series
### More than 4:1 ratio of good:bad (or vice versa)
removals <- ROContrast %>%
  mutate(contrast = if_else(Ratio.P2G == 0 | is.infinite(Ratio.P2G ), 9, if_else(Ratio.P2G < 0.25 | Ratio.P2G > 4, 4 , 0)),
         years    = if_else(N.years < 15, 1, 0),
         rem      = contrast + years) %>%
  arrange(-rem, N.years, Ratio.P2G) %>%
  print(n = nrow(.))

# Remove these stocks from data
rocRem_long <- merge(rocAll_long, removals) %>%
  filter(!rem > 0)

save(rocRem_long, file = paste0(getwd(), "/Output/Data/ROC/rocRem_long.rds"))

# Number of stocks remaining (25 stocks)
stk_names_rem <- unique(rocRem_long$StockKeyLabel)
stk_names_rem
save(stk_names_rem, file = paste0(getwd(), "/Output/Data/ROC/rocRem_long_stocks.rds"))

# Surveys (52)
srvys_rem <- rocRem_long %>% 
  select(StockKeyLabel, SurveyName, Quarter, SurveyNameIndex) %>% 
  distinct() %>%
  arrange(StockKeyLabel)
srvys_rem
save(srvys_rem, file = paste0(getwd(), "/Output/Data/ROC/rocRem_long_surveys.rds"))

# Quick plot
#load(paste0(getwd(), "/Output/Data/ROC/ROCdata.rds"))

stk <- sample(stk_names, 1) # or specify
plot_df <- filter(rocRem_long, 
                  StockKeyLabel == stk, 
                  L50lvl == "mean", 
                  Year > 1500) %>%
  select(-c(Year, `SSB/MSYBtrigger`, status, TP, FP, TN, FN, FPR, TPR, TSS, Value)) %>%
  distinct() %>%
  mutate(SurveyNameQr = paste0(SurveyName, "-Q", Quarter))

ggplot(data = plot_df) +
  geom_col(aes(y=AUC, x=SurveyNameQr, fill=SurveyNameQr)) +
  facet_wrap("Indicator") +
  coord_cartesian(ylim = c(0,1)) +
  guides(colour   = guide_legend(order = 1)) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  ggtitle(stk) +
  theme(axis.text.x = element_blank())

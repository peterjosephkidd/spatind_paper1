#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                            1. Select Stocks
#>                              
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script reproduces the file .../Data/Initial/DR_Stocks/StockInfo/icesData-69stks-AY2022-SA-data.xlsx
#> Which is already provided in the folder
#> There is therefore no need to run this script except only to see how
#> stocks were sleected at the beginning of this project

library(icesSAG)
library(icesSD)
library(stringr)
library(writexl)
library(dplyr)
library(ggplot2)

rm(list = ls())

# 1. Stock Descriptions ####
stks22 <- getSD(year = 2022) # N=272

# Data-rich stocks 
stks22.dr <- stks22 %>%
  mutate(DataCategory = as.numeric(DataCategory)) %>%
  filter(DataCategory < 2,
         SpeciesCommonName != "Norway lobster") # N=95

# Remove stocks without assessment keys
stks22.dr <- stks22.dr[!is.na(stks22.dr$AssessmentKey),] # N=84
any(is.na(stks22.dr$AssessmentKey))

# Explore 
length(unique(stks22.dr$SpeciesCommonName)) 
stks22.dr %>% 
  group_by(SpeciesCommonName) %>%
  summarise(Freq = length(SpeciesCommonName)) %>%
  arrange(-Freq)

length(unique(stks22.dr$StockKeyLabel))     # n.stocks
length(unique(stks22.dr$AssessmentKey))     # n.asskeys

table(stks22.dr$ExpertGroup, stks22.dr$FisheriesGuild)
table(stks22.dr$FisheriesGuild)
table(stks22.dr$ExpertGroup)

# 2. Reference Points ####
refpts <- icesSAG::getFishStockReferencePoints(unique(stks22.dr$AssessmentKey))
refpts <- do.call(bind_rows, refpts) # bind_rows better here due to different dimensions among lists
head(refpts)

# Stocks with MSY Btrigger estimated 
refpts.msyb <- filter(refpts, !is.na(MSYBtrigger)) # N=69

# Link to Advice
add_link = F
if(isTRUE(add_link)){
  advice <- icesSAG::getListStocks(2022) %>% 
    filter(AssessmentKey %in% unique(refpts.msyb$AssessmentKey)) %>% 
    select(AssessmentKey, LinkToAdvice)
  
  refpts.msyb <- full_join(refpts.msyb, advice, by = c("AssessmentKey"))
  } else {
    refpts.msyb <- refpts.msyb
}

head(refpts.msyb)

# 3. Stock Assessment Estimates ####
sumtbl <- getSummaryTable(unique(refpts.msyb$AssessmentKey))
sumtbl <- do.call(rbind.data.frame, sumtbl) # bind_rows doesn't work here due to columns not being of consistent class
sumtbl <- rename(sumtbl, StockKeyLabel = fishstock)
head(sumtbl)

# 4. Final dataframe ####
# Combine summary table and reference points ###
finaldata <- full_join(sumtbl, refpts.msyb, 
                       by = "StockKeyLabel", 
                       suffix = c("", ".refpts"))
nrow(sumtbl) == nrow(finaldata)
head(finaldata)

# check AY matches then remove one of the AY cols ###
if(all(finaldata$AssessmentYear == finaldata$AssessmentYear.refpts)){
  finaldata <- select(finaldata, -AssessmentYear.refpts)
}

# Add Stock Descriptions ###
finaldata <- right_join(stks22, finaldata, by = na.omit(janitor::compare_df_cols(stks22, finaldata))$column_name)

head(finaldata)
length(unique(finaldata$StockKeyLabel))

# Check for NAs
any(is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]))
unique(finaldata[is.na(finaldata[, c("FisheriesGuild", "TrophicGuild", "SizeGuild")]),]$StockKeyLabel) # Atlantic Wolffish (AK 3208) # check https://sid.ices.dk/ViewStock.aspx?key=3208

# Save data ###
write_xlsx(finaldata, paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-", 
                           length(unique(finaldata$StockKeyLabel)), 
                           "stks-AY", 
                           unique(finaldata$AssessmentYear),
                           "-SA-data.xlsx"))

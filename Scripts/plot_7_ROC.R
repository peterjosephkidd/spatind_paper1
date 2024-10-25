#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               5. Plot ROC curve outputs
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 0. Init ####
library(dplyr)
library(ggplot2) 
library(readxl)
library(writexl)
library(rfishbase)
library(SPMpriors)
library(FishLife)


rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/"

load(file = paste0(load.path, "Outputs/ROC/ROCdata.rda"))

metadata <-     read_xlsx(paste0(load.path, "icesSA_data/icesData-69stks-AY2022-stkdescrptn.xlsx"))
srv.coverage <- read_xlsx(paste0(load.path, "icesSA_data/icesData-stksurveys-survcoverage.xlsx"))
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/results_funs.R")
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/stockleaves.rds")

# _________________________________________________________________________ ####

# 1. Prep Data ####
## 1.1 Add grouping variables ####
all(rocAll_long$StockKeyLabel %in% metadata$StockKeyLabel)
rocAll_long <- left_join(rocAll_long, metadata[c("StockKeyLabel", "SpeciesCommonName", "SpeciesScientificName", "ExpertGroup", "TrophicGuild", "FisheriesGuild", "SizeGuild")], by = "StockKeyLabel")
rocAll_long <- filter(rocAll_long, !is.na(StockKeyLabel))

# Remove location indicators
rocAll_long <- filter(rocAll_long, !`Spatial Indicator` %in% c("CoG (x)", "CoG (y)"))

## 1.2 Indicator order ####
indorder <- c(#"CoG (x)", "CoG (y)",      # Location
              "Inertia", "EOO", "ELA",   # Dispersion
              "POPR", "POPH",            # Occupancy
              "Gini Index", "D95", "SA", # Aggregation 
              "EA", "SPI")

## 1.3 Growth rate ####
growth <- c()
species.list <- na.omit(unique(rocAll_long$SpeciesScientificName))

for (i in 1:length(species.list)) {
  
  SpeciesScientificName <- species.list[i]
  
  if (SpeciesScientificName == "Lepidorhombus") {
    warning("\nlez.27.4a6a is a combined stock of Megrim", immediate. = T)
  } else {
  
  message(paste0("\n", SpeciesScientificName))
  Genus   <- strsplit(SpeciesScientificName, " ")[[1]][1]
  Species <- strsplit(SpeciesScientificName, " ")[[1]][2]
  
  par <- SPMpriors::flmvn_traits(Genus, Species, Plot = FALSE)
  K <- par$traits[2,]
  output <- cbind(SpeciesScientificName, K)
  growth <- rbind(growth, output)
  
  }
}

growth <- (rbind(growth, growth[growth$SpeciesScientificName == "Lepidorhombus whiffiagonis",]))
growth[nrow(growth),]$SpeciesScientificName <- "Lepidorhombus" # using whif for combined megrim stocks, as boscii is rare
growth <- rename(growth, GrowthRateK = mu.sp)

rocAll_long <- full_join(rocAll_long, growth[c("SpeciesScientificName", "GrowthRateK")], by = "SpeciesScientificName")

## 1.4 Add survey coverage ####
srv.coverage$SurveyName <- srv.coverage$SurveyAcronymn
srv.coverage$Quarter <- as.character(srv.coverage$Quarter)

unique(rocAll_long$StockKeyLabel)[!unique(rocAll_long$StockKeyLabel) %in% unique(srv.coverage$StockKeyLabel)]

rocAll_long <- left_join(rocAll_long, srv.coverage[c("StockKeyLabel", "SurveyIndex", "SurveyName", "Quarter", "AvgSurveyCoverage", "SpeciesCommonName", "SpeciesScientificName", "ExpertGroup")],
                         by = join_by(StockKeyLabel, SurveyIndex, SurveyName, Quarter, SpeciesCommonName, SpeciesScientificName, ExpertGroup))

## 1.5 Indicator categories ####
#loc <- c("CoG (x)", "CoG (y)")                   # Location
ran <- c("Inertia", "EOO", "ELA")                # Dispersion
occ <- c( "POPR", "POPH")                        # Occupancy
agg <- c("Gini Index", "D95", "SA", "EA", "SPI") # Aggregation

rocAll_long <- rocAll_long %>%
  mutate(ind_category = case_when(
    #`Spatial Indicator` %in% loc ~ "Location",
    `Spatial Indicator` %in% ran ~ "Dispersion",
    `Spatial Indicator` %in% occ ~ "Occupancy",
    `Spatial Indicator` %in% agg ~ "Aggregation",
    TRUE ~ NA_character_
  ))

rocAll_long$ind_category <- factor(
  rocAll_long$ind_category, levels = c(#"Location", 
    "Occupancy", "Dispersion", "Aggregation"))

## 1.6 Colours for oringinal bar plot ####
# Colours (doesn't work well for this amount of stocks - skip)
colr.groups <- rocAll_long %>%
  select(StockKeyLabel, FisheriesGuild, GrowthRateK) %>%
  distinct() %>%
  arrange(FisheriesGuild, GrowthRateK) %>%
  group_by(FisheriesGuild) %>%
  mutate(rank = order(GrowthRateK))

Ben    <- paletteer::paletteer_c("ggthemes::Classic Area Green", 30, direction = -1)
Crust  <- paletteer::paletteer_c("grDevices::Red-Purple", 30)
Dem    <- paletteer::paletteer_c("grDevices::Mint", 30)
Elasmo <- paletteer::paletteer_c("grDevices::SunsetDark", 30)
Pel    <- paletteer::paletteer_c("ggthemes::Red-Gold", 30, direction = -1)

colr.samples2 <- colr.groups %>%
  group_by(FisheriesGuild) %>%
  count() %>%
  mutate(colr.list = if_else(FisheriesGuild == "Benthic",      list(Ben   [as.integer(seq(1, 30, length.out = n))]),
                     if_else(FisheriesGuild == "Crustacean",   list(Crust [as.integer(seq(1, 30, length.out = n+1))]),
                     if_else(FisheriesGuild == "Demersal",     list(Dem   [as.integer(seq(1, 30, length.out = n))]),
                     if_else(FisheriesGuild == "Elasmobranch", list(Elasmo[as.integer(seq(1, 30, length.out = n+1))]),
                     if_else(FisheriesGuild == "Pelagic",      list(Pel   [as.integer(seq(1, 30, length.out = n))]), NA))))))

colr.samples <- left_join(colr.groups, colr.samples2, by = "FisheriesGuild") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(colr = unlist(colr.list)[rank]) %>%
  select(StockKeyLabel, colr)

rocAll_long <- left_join(rocAll_long, colr.samples, by = "StockKeyLabel")



## 1.7 Order of bars ####
stkorder <- rocAll_long %>% 
  select(StockKeyLabel, FisheriesGuild, GrowthRateK) %>%
  distinct() %>%
  na.omit() %>%
  arrange(FisheriesGuild, GrowthRateK)
stkorder$StockKeyLabel

## 1.8 Unlist some vars ####
for (i in 1:ncol(rocAll_long)) {
  if (is.list(rocAll_long[i])) {
    colnames(rocAll_long[i])
    rocAll_long[i] <- unlist(rocAll_long[i])
  }
}


# _________________________________________________________________________ ####

# 2. Good surveys ####
## Filter to good surveys

# How much contrast in SSB/MSYB trigger in each time series:
ROContrast <- rocAll_long %>%
  select(StockKeyLabel, Year, Quarter, SurveyNameIndex, status, `Spatial Indicator Value`) %>%
  na.omit() %>%
  select(-`Spatial Indicator Value`) %>%
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
rocAll_rem <- merge(rocAll_long, removals) %>%
  filter(!rem > 0)
save(rocAll_rem, file = paste0(save.path, "ROC/rocAll_rem.rds"))
       
## 2.1 Final selection ####
# Number of stocks remaining (25 stocks)
stks_rem <- rocAll_rem %>% 
  select(StockKeyLabel, FisheriesGuild) %>% 
  distinct() %>% 
  arrange(FisheriesGuild, StockKeyLabel) 
stks_rem
save(stks_rem, file = paste0(save.path, "ROC/rocAll_rem_stocks.rds"))

table(stks$FisheriesGuild)
nrow(stks)

rm(stks_rem)

# Species (15 species)
spcs_rem <- rocAll_rem %>% 
  select(StockKeyLabel, FisheriesGuild, SpeciesCommonName) %>% 
  distinct() %>%
  arrange(FisheriesGuild, StockKeyLabel, SpeciesCommonName) 
spcs_rem
save(spcs_rem, file = paste0(save.path, "ROC/rocAll_rem_species.rds"))

table(spcs$SpeciesCommonName, spcs$FisheriesGuild)
length(unique(spcs$SpeciesCommonName))

rm(spcs_rem)

# Surveys
srvys_rem <- rocAll_rem %>% 
  select(StockKeyLabel, FisheriesGuild, SpeciesCommonName, SurveyName, Quarter, SurveyNameIndex) %>% 
  distinct() %>%
  arrange(FisheriesGuild, StockKeyLabel, SpeciesCommonName)
srvys_rem
save(srvys_rem, file = paste0(save.path, "ROC/rocAll_rem_surveys.rds"))

# _________________________________________________________________________ ####

# 3. AUC summaries ####
# All surveys (used in random forest)
auc_summary <- rocAll_long %>%
  group_by(StockKeyLabel, SurveyName, SurveyIndex, SurveyNameIndex, Quarter) %>%
  select(StockKeyLabel, SurveyNameIndex, SpeciesCommonName, SpeciesScientificName,
         Quarter, ind_category, `Spatial Indicator`, 
         L50lvl,  AUC, colr, GrowthRateK, FisheriesGuild, AvgSurveyCoverage, Year) %>%
  mutate(Years = paste0(min(Year[Year != min(Year)]), "-", max(Year)),
         YearLength = max(Year) - min(Year[Year != min(Year)])) %>%
  ungroup() %>%
  select(-Year) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`, L50lvl) %>%
  na.omit() %>%
  mutate(xaxis = paste0(StockKeyLabel, ": ", SurveyName, ", Q", Quarter)) %>%
  print(n = 20)

auc_summary <- left_join(auc_summary, ROContrast[c(1:3,7)], by = c("StockKeyLabel", "SurveyNameIndex", "Quarter"))

save(auc_summary, file = paste0(save.path, "ROC/auc_summary_all_surveys.rds"))  
write_xlsx(auc_summary, paste0(save.path, "ROC/auc_summary_all_surveys.xlsx"))

# Good surveys (we use this)
auc_summary <-  rocAll_rem %>%
  group_by(StockKeyLabel, SurveyName, SurveyIndex, SurveyNameIndex, Quarter) %>%
  select(StockKeyLabel, SurveyNameIndex, SpeciesCommonName, SpeciesScientificName,
         Quarter, ind_category, `Spatial Indicator`, 
         L50lvl,  AUC, colr, GrowthRateK, FisheriesGuild, AvgSurveyCoverage,  Year) %>%
  mutate(Years = paste0(min(Year[Year != min(Year)]), "-", max(Year)),
         YearLength = max(Year) - min(Year[Year != min(Year)])) %>%
  ungroup() %>%
  select(-Year) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`, L50lvl) %>%
  #na.omit() %>%
  mutate(xaxis = paste0(StockKeyLabel, ": ", SurveyName, ", Q", Quarter)) %>%
  print(n = 20)

auc_summary <- left_join(auc_summary, ROContrast[c(1:3,7)], by = c("StockKeyLabel", "SurveyNameIndex", "Quarter"))

save(auc_summary, file = paste0(save.path, "ROC/auc_summary_good_surveys.xlsx.rds"))  
write_xlsx(auc_summary, paste0(save.path, "ROC/auc_summary_good_surveys.xlsx"))

# Order stocks by fisheries guild, k, and survey coverage
stkorder.auc <- auc_summary %>% 
  ungroup() %>%
  select(StockKeyLabel, xaxis, FisheriesGuild, GrowthRateK, colr, AvgSurveyCoverage) %>%
  distinct() %>%
  na.omit() %>%
  arrange(FisheriesGuild, GrowthRateK, StockKeyLabel, AvgSurveyCoverage) %>%
  print(n = nrow(.))

# factorize columns for plotting
auc_summary$xaxis <- factor(auc_summary$xaxis,  levels = stkorder.auc$xaxis)
auc_summary$colr <-  factor(auc_summary$colr,   levels = unique(stkorder.auc$colr))
auc_summary$StockKeyLabel <- factor(auc_summary$StockKeyLabel, levels = unique(stkorder.auc$StockKeyLabel))

# Total number of surveys remaining
Nsurvs <- length(unique(as.character(auc_summary$xaxis[!is.na(auc_summary$AUC)]))) # where AUC is available. 

# Proportions of AUC good/average/bad using mean L50
auc_prop_mean <- auc_summary %>%
  filter(L50lvl == "mean") %>%
  ungroup() %>%
  select(`Spatial Indicator`, AUC, L50lvl) %>%
  group_by(`Spatial Indicator`) %>%
  mutate(propgood = round(length(AUC[AUC>=0.75])/Nsurvs*100,1),
         propavg  = round(length(AUC[AUC>0.5 & AUC <0.75])/Nsurvs*100,1),
         propbad  = round(length(AUC[AUC<=0.5])/Nsurvs*100,1)) %>%
  select(-AUC) %>%
  distinct()

# Proportion of AUCs good/bad across L50 values 
#> We are looking for consistency across L50 values. 
#> If an indicator is good no matter which L50 value was used, it is not sensitive to L50
#> If the performance of an indicator changes with L50, then it is sensitive to L50
#> Percetnages wont ad up to 100% in each tile.
#> The missing percentage is the proportion where performance was not consistent across L50
auc_prop <- auc_summary %>% 
  group_by(StockKeyLabel, SurveyNameIndex, `Spatial Indicator`, Quarter, xaxis) %>%
  mutate(highAUC = if_else(all(AUC >= 0.75), 1, 0),
         avgAUC  = if_else(all(AUC >0.25 & AUC <0.75), 1, 0),
         lowAUC  = if_else(all(AUC <= 0.25), 1, 0)) %>%
  filter(L50lvl == "mean") %>%
  ungroup() %>%
  group_by(`Spatial Indicator`) %>%
  summarise(propgood = round(sum(highAUC)/Nsurvs*100,1),
            propavg  = round(sum(avgAUC) /Nsurvs*100,1),
            propbad  = round(sum(lowAUC) /Nsurvs*100,1))

# _________________________________________________________________________ ####

# 4. [OLD] Original Bar Plot ####
#> Original bar charts produced for presentation
## P1: Bar Chart ####
auc.plot <- ggplot() +
  geom_col(data = auc_summary[auc_summary$L50lvl == "mean",], aes(x = xaxis, y = AUC, fill = colr), position = position_dodge(width = 1), width = 1) +
  geom_line(data = auc_summary[auc_summary$L50lvl != "mean",],  aes(x = xaxis, y = AUC, group = xaxis)) +
  geom_point(data = auc_summary[auc_summary$L50lvl != "mean",], aes(x = xaxis, y = AUC, shape = factor(L50lvl, level = c("lowerCI", "upperCI"), labels = c("Lower CI", "Upper CI")), colour = factor(L50lvl, level = c("lowerCI", "upperCI"), labels = c("Lower CI", "Upper CI")))) +
  
  scale_fill_identity("Stock ID", guide = "legend", labels = levels(auc_summary$StockKeyLabel)) +
  geom_hline(yintercept = 0.25,  colour = "grey20", lty = 1) +
  geom_hline(yintercept = 0.75, colour = "grey20", lty = 2) +
  geom_label(data = auc_prop, aes(x = length(unique(auc_prop$xaxis)) + 7, y = 1,     label = c(paste0(auc_prop$propgood, "%"))), fill = "forestgreen", alpha = 0.6, size = 3.3) +
  geom_label(data = auc_prop, aes(x = length(unique(auc_prop$xaxis)) + 7, y = 0.625, label = c(paste0(auc_prop$propavg, "%"))),  fill = "gold3", alpha = 0.7, size = 3.3) +
  geom_label(data = auc_prop, aes(x = length(unique(auc_prop$xaxis)) + 7, y = 0.1,   label = c(paste0(auc_prop$propbad, "%"))),  fill = "red4", alpha = 0.8, size = 3.3) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right") +
  xlab("Survey Name, Survey Index") +
  labs(shape = "L50 Confidence Intervals (CI)", colour = "L50 Confidence Intervals (CI)") +
  guides(color = guide_legend(override.aes = list(size = 3)))
auc.plot
ggsave(paste0(save.path, "ROC/", "AUC-summaryplot-allsurveys.png"), auc.plot,     height = 12, width = 20)

## P2: Proportions ####
colrs <- c("brown4", "gold3", "forestgreen")

auc_prop2 <- auc_prop %>% 
  tidyr::pivot_longer(cols = c("propgood", "propavg", "propbad"),
                      names_to = "PropCat",
                      values_to = "Prop") %>%
  mutate(colr = if_else(PropCat == "propbad", colrs[1], 
                        if_else(PropCat == "propavg", colrs[2], colrs[3])),
         colr = factor(colr, levels = c(colrs[1], colrs[2], colrs[3])))

auc.prop <- ggplot() +
  geom_col(data = auc_prop2, aes(x = factor(`Spatial Indicator`, level = indorder), y = Prop, fill = colr), width = 0.95) +
  scale_fill_identity("", guide = "legend", labels = c("AUC <= 0.25", "0.5 < AUC < 0.75 ", "AUC >= 0.75")) +
  ylab("Percetage of Survey Indices") +
  xlab("Spatial Indicator") +
  scale_x_discrete(expand = c(0, 0)) +  # Remove padding space on x-axis
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) + # Remove padding space on y-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))

auc.prop

ggsave(paste0(save.path, "ROC/", "AUC-propplot-allsurveys.png"), auc.prop,     height = 4, width = 5)


# Explore anomalies >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk <- unique(rocAll_long$StockKeyLabel)[6]
srvindx <- unique(rocAll_long[rocAll_long$StockKeyLabel == stk,]$SurveyNameIndex)[3]
qr <- 1
ind <- "Inertia"

rocAll_long %>%
  filter(StockKeyLabel == stk,
         SurveyNameIndex == srvindx,
         Quarter == qr,
         `Spatial Indicator` == ind) %>%
  na.omit() %>%
  print(n = nrow(.))

  
# _________________________________________________________________________ ####

# 5. Tables ####
## T1: All AUC & TSS Values ####################################################

# Find highest TSS and corresponding spatial indicator value
t.auctss <- #rocAll_long %>% for all surveys
  rocAll_rem %>%
  group_by(Quarter, StockKeyLabel, SurveyNameIndex, `Spatial Indicator`, L50lvl) %>%
  summarise(OptIndVal = max(`Spatial Indicator Value`),
            OptTSS = max(TSS), 
            AUC = unique(AUC),
            Years  = paste0(min(Year[Year!=min(Year)]), "-", max(Year)), 
            AvgSurveyCoverage = unique(AvgSurveyCoverage),
            K = unique(GrowthRateK),
            Survey = unique(SurveyName)) %>%
  relocate(StockKeyLabel, Survey, Years, Quarter) %>%
  tidyr::pivot_wider(names_from = L50lvl, values_from = c(AUC, OptTSS, OptIndVal)) %>%
  as.data.frame() %>%
  relocate(StockKeyLabel, Survey, Quarter, Years, AvgSurveyCoverage, K, `Spatial Indicator`,
           AUC_mean, OptTSS_mean, OptIndVal_mean,
           AUC_lowerCI, OptTSS_lowerCI, OptIndVal_lowerCI,
           AUC_upperCI, OptTSS_upperCI, OptIndVal_upperCI) %>%
  arrange(StockKeyLabel, `Spatial Indicator`, Survey) %>%
  ungroup() %>%
  select(-SurveyNameIndex) %>%
  mutate(OptIndVal_mean    = as.numeric(OptIndVal_mean),
         OptIndVal_lowerCI = as.numeric(OptIndVal_lowerCI),
         OptIndVal_upperCI = as.numeric(OptIndVal_upperCI)) %>%
  mutate_if(is.numeric, ~round(., 3))
t.auctss

all(unique(t.auctss$StockKeyLabel) %in% unique(rocAll_rem$StockKeyLabel))

# Save
save(t.auctss, file = paste0(save.path, "ROC/T1-AUC-TSS.rds"))  
write_xlsx(t.auctss, paste0(save.path, "ROC/T1-AUC-TSS.xlsx"))

# LaTex Table 
tab_aucL50 <- t.auctss %>%
  rename(`Stock ID` = StockKeyLabel, Qr = Quarter) %>%
  select(-c(K, AvgSurveyCoverage)) 

tab_aucL50$`Spatial Indicator` <- gsub("Gini Index", "G", tab_aucL50$`Spatial Indicator`)
tab_aucL50$`Spatial Indicator` <- gsub("Inertia", "I", tab_aucL50$`Spatial Indicator`)

load(paste0(save.path, "SummaryTables/T1.stksum_short.rds"))
stks <- T1stksum.short$`Stock ID`
tm(T1stksum.short)

# This creates an individual tex file for each stock which can be uploaded to overleaf
# Or you can copy the outputs from the console
for (i in stks) {
  writeLines("#################################################################")
  message(i)
  writeLines("\n")
  
  df <- filter(tab_aucL50, `Stock ID` == i)
  
  df_to_latex(df,
              bold_header = TRUE,
              font_size = "Huge",
              longtable = FALSE,
              resize = TRUE,
              position = "!htb",
              nest_cols = list(c(6, 7, 8), c(9, 10, 11), c(12, 13, 14)),
              nest_cols_rename = list(c("AUC", "TSS", "Thresh"), c("AUC", "TSS", "Thresh"), c("AUC", "TSS", "Thresh")),
              nest_header = c("Mean", "Lower CI", "Upper CI"),
              wrap_text = c(1,5), 
              text_width = c(6,5),
              label = "sup: auc_l50", 
              caption = paste0("ROC performance metrics for ", i, " across L50 sensitivity tests."),
              file_path = paste0(save.path, "SummaryTables/sup_aucL50_", i, ".tex"))
  writeLines("\n")
}

# Copy this into tex file to place supp tables in same order as stocks in Table 1 in methods
writeLines(paste0(paste0("\\input{suppmaterial/tables/sup_aucL50_", stks, ".tex}"), collapse = "\n"))

## T2: AUC bins ################################################################
t.auc_bins <- auc_summary %>% 
  #filter(#StockKeyLabel %in% c("had.27.46a20", "mac.27.nea"),
  #      Fish0eriesGuild == "Elasmobranch") %>%
  group_by(StockKeyLabel, SurveyNameIndex, L50lvl, `Spatial Indicator`, Quarter, xaxis) %>%
  mutate(AUClvl = if_else(all(AUC >= 0.8), "Best", 
                  if_else(all(AUC >= 0.6 & AUC < 0.8), "Good", 
                  if_else(all(AUC > 0.4 & AUC < 0.6), "Random", 
                  if_else(all(AUC > 0.2 & AUC <= 0.4), "Bad", 
                  if_else(all(AUC <= 0.2), "Worst", "Inconsistent")))))) %>%
  ungroup() %>%
  select(-AUC) %>%
  distinct() %>%
  group_by(`ind_category`, `Spatial Indicator`, L50lvl) %>%
  summarise(`1.0 $>=$ AUC $>=$ 0.8` =    (length(AUClvl[AUClvl=="Best"])),
            `0.8 $>$ AUC $>=$ 0.6`  =    (length(AUClvl[AUClvl=="Good"])),
            `0.6 $>$ AUC $>$ 0.4`  =    (length(AUClvl[AUClvl=="Random"])),
            `0.4 $>=$ AUC $>$ 0.2`   =    (length(AUClvl[AUClvl=="Bad"])),
            `0.2 $>=$ AUC $>=$ 0` =    (length(AUClvl[AUClvl=="Worst"])), .groups = "keep")

t.auc_bins$`Spatial Indicator` <- gsub("Gini Index", "G", t.auc_bins$`Spatial Indicator`)
t.auc_bins$`Spatial Indicator` <- gsub("Inertia", "I", t.auc_bins$`Spatial Indicator`)
t.auc_bins$`Spatial Indicator` <- factor(t.auc_bins$`Spatial Indicator`, levels = c("I", "EOO", "ELA", "POPR", "POPH", "G", "D95", "SA", "EA", "SPI"))
t.auc_bins$L50lvl <- factor(t.auc_bins$L50lvl, levels = c("mean", "lowerCI", "upperCI"), labels = c("Mean", "Lower CI", "Upper CI"))

t.auc_bins <- t.auc_bins %>%
  arrange(`Spatial Indicator`, L50lvl) %>%
  rename(`Indicator Category` = ind_category,
         `L50 Test` = L50lvl)

t.auc_bins$`Indicator Category` <- as.character(t.auc_bins$`Indicator Category`)
t.auc_bins$`Spatial Indicator` <- as.character(t.auc_bins$`Spatial Indicator`)
t.auc_bins$`L50 Test` <- as.character(t.auc_bins$`L50 Test`)

df_to_latex(t.auc_bins, 
            bold_header = TRUE,
            font_size = "footnotesize",
            resize = FALSE,
            wrap_text  = c(1,2), 
            text_width = c(5,5),
            nest_cols = list(c(4:8)),
            align = c("l","l","l","c","c","c","c","c"),
            nest_header = "AUC Category",
            nest_cols_rename = list(c("++", "+", "", "-", "- -")),
            label = "tab: auc_hist", 
            caption = "Frequency counts of AUCs within performance categories for each spatial indicator under each L50 sensitivity test. AUC Category refers to the performance category that the mean AUC falls within; ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6)." ,
            file_path = paste0(save.path, "SummaryTables/sup_frequency_auc.tex"))

View(filter(t.auc_bins, L50lvl == "mean"))

t.auc_bins_long <- tidyr::pivot_longer(t.auc_bins, cols = starts_with("N."), names_to = "Performance", values_to = "count")

t.auc_bins_long$Performance <- factor(t.auc_bins_long$Performance, 
                                      levels = rev(c("N.best", "N.good", "N.rand", "N.bad", "N.worst")),
                                      labels = rev(c("AUC >= 0.8", "0.6 <= AUC < 0.8", "0.4 < AUC < 0.6", "0.2 < AUC <= 0.4", "AUC <= 0.2")))

## T3 Summary Stats ############################################################
### Bindata
# Create columns to put AUC values in bins
bindata <- create_auc_bindata(auc_summary, bins = seq(0,1, by = 0.1))

### Stats
# Calculate mean and variance in AUCs for indicator for each L50 method
ind_stats <- auc_stats(bindata, avg_across_L50 = FALSE)
ind_stats$`Spatial Indicator` <- gsub("Gini Index", "G", ind_stats$`Spatial Indicator`)
ind_stats$`Spatial Indicator` <- gsub("Inertia", "I", ind_stats$`Spatial Indicator`)
ind_stats$`Spatial Indicator` <- factor(ind_stats$`Spatial Indicator`, levels = c("I", "EOO", "ELA", "POPR", "POPH", "G", "D95", "SA", "EA", "SPI"))
ind_stats$L50lvl <- factor(ind_stats$L50lvl, levels = c("mean", "lowerCI", "upperCI"), labels = c("Mean", "Lower CI", "Upper CI"))
ind_stats[5:12] <- round(ind_stats[5:12], 3)
ind_stats$cond <- ifelse(is.na(ind_stats$cond), "", ind_stats$cond)

ind_stats <- ind_stats %>%
  arrange(`Spatial Indicator`, L50lvl) %>%
  rename(`L50 Test` = L50lvl, `Indicator Category` = ind_category, 
         `Mean AUC` = mu, SD = stndrd_dev, SE = se, 
         `Lower CI (95\\%)` = lower_ci, `Upper CI (95\\%)` = upper_ci, 
         `Median AUC` = median_auc, `AUC Category` = cond) %>%
  select(-CI95_margin)

print(ind_stats, n=30)

save(ind_stats, file = paste0(save.path, "/SummaryTables/sup_overall_indicator_stats.rds"))

# LaTex table
ind_stats$`Spatial Indicator` <- as.character(ind_stats$`Spatial Indicator`)
ind_stats$`L50 Test` <- as.character(ind_stats$`L50 Test`)
ind_stats$`Indicator Category` <- as.character(ind_stats$`Indicator Category`)

df_to_latex(ind_stats, 
            bold_header = TRUE,
            font_size = "small",
            resize = TRUE,
            wrap_text  = c(1,3,5,9,10,11,12), 
            text_width = c(5,5,5,5,5 ,5 ,5),
            label = "tab: ind_stats", 
            caption = "Summary statistics of AUC outputs for each spatial indicator under each L50 sensitivity test. n = the number of contributing AUC values to the mean AUC; SS = sum of squares; SD = standard deviation; SE = standard error; CI = confidence interval. AUC Category refers to the performance category that the mean AUC falls within; ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6)." ,
            file_path = paste0(save.path, "SummaryTables/sup_overall_indicator_stats.tex"))

## T4 AUC Category Counts ######################################################
auc_summary2 <- auc_summary
auc_summary2$StockKeyLabel <- factor(auc_summary2$StockKeyLabel, levels = unique(T1stksum.short$`Stock ID`))

auc_catcount <- auc_summary2 %>%
  filter(L50lvl == "mean", ind_category == "Dispersion") %>%
  mutate(NGG = ifelse(AUC >= 0.8, 1, 0),
         NG = ifelse(AUC >= 0.6 & AUC < 0.8, 1, 0),
         NR = ifelse(AUC < 0.6 & AUC > 0.4, 1, 0),
         NB = ifelse(AUC <= 0.4 & AUC > 0.2, 1, 0),
         NBB = ifelse(AUC <= 0.2, 1, 0)) %>%
  group_by(SpeciesCommonName,
           StockKeyLabel,
           #SurveyName, 
           #Quarter
           ) %>%
  summarise(`++` = sum(NGG),
            `+` = sum(NG),
            ` ` = sum(NR),
            `-` = sum(NB),
            `- -` = sum(NBB)) %>%
  arrange(StockKeyLabel, 
          #SurveyName
          ) %>%
  ungroup() %>%
  bind_rows(
    summarise(., 
              SpeciesCommonName = "", 
              StockKeyLabel = "", 
              #SurveyName = "", 
              #Quarter = "",
              `++` = sum(`++`), 
              `+` = sum(`+`), 
              ` ` = sum(` `), 
              `-` = sum(`-`), 
              `- -` = sum(`- -`))) %>%
  rename(
    Species = SpeciesCommonName,
    `Stock ID` = StockKeyLabel,
    #Survey = SurveyName
  ) %>%
  print(., n = nrow(.))

# Cases that do not show negative relationships
auc_catcount %>%
  filter(`-` == 0, `- -` == 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that do not show positive relationships
auc_catcount %>%
  filter(`+` == 0, `++` == 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that only show positive 
auc_catcount %>%
  filter(` ` == 0, `-` == 0, `- -` == 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that show strong positive 
auc_catcount %>%
  filter(`++` > 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that did not achieve strong positve 
auc_catcount %>%
  filter(!`++` > 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases where there were strong OR moderate positive 
auc_catcount %>%
  filter(`++` > 0 | `+` > 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that only show random 
auc_catcount %>%
  filter(`+` == 0,`++` == 0, `-` == 0, `- -` == 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that do not show random 
auc_catcount %>%
  filter(!` ` > 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that show strong negative 
auc_catcount %>%
  filter(`- -` > 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

# Cases that show strong negative or weak neg
auc_catcount %>%
  filter(`- -` > 0 | `-` > 0) %>%
  print(., n = nrow(.)) %>%
  nrow(.)

save(auc_catcount, file = paste0(save.path, "ROC/auc_catcount_good_surveys.rds"))  

# LaTex table
df_to_latex(auc_catcount, 
            bold_header = TRUE,
            font_size = "footnotesize",
            resize = TRUE,
            wrap_text = c(1,2), 
            text_width = c(12,12),
            nest_cols = list(c(5:9)),
            nest_header = "AUC Category",
            nest_cols_rename = list(c("++", "+", "", "-", "- -")),
            align = c("l","l","l","l", "c","c","c","c","c"),
            label = "tab: auc_catcount", 
            caption = "The counts of indicators within each AUC category for each combination of stock and survey AUC Category refers to the performance category that the mean AUC falls within; ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6).",
            file_path = paste0(save.path, "SummaryTables/tab_catcount.tex")
)


df_to_latex(t.auc_bins, 
            bold_header = TRUE,
            font_size = "footnotesize",
            resize = FALSE,
            wrap_text  = c(1,2), 
            text_width = c(5,5),
            nest_cols = list(c(4:8)),
            align = c("l","l","l","c","c","c","c","c"),
            nest_header = "AUC Category",
            nest_cols_rename = list(c("++", "+", "", "-", "- -")),
            label = "tab: auc_hist", 
            caption = "Frequency counts of AUCs within performance categories for each spatial indicator under each L50 sensitivity test. AUC Category refers to the performance category that the mean AUC falls within; ”++” excellent performance (AUC $>=$ 0.8); ”+” good performance (0.8 $>$ AUC $>=$ 0.6); ”-” worse than random (0.4 $>=$ AUC $>$ 0.2); ”– -” worst performance (AUC $<=$ 0.2). No symbol indicates that classification skill was no better than a random classifier (0.4 $<$ AUC $<$ 0.6)." ,
            file_path = paste0(save.path, "SummaryTables/sup_frequency_auc.tex"))

# _________________________________________________________________________ ####

# Density & Heatmaps ####
# 1. Histogram ####
### Frequency plots
# Plot histogram of AUC bins for L50 Mean test
ind_stats <- auc_stats(bindata, avg_across_L50 = FALSE)
bin_freq <- create_auc_binfreq(bindata)

binfreq <- binfreq %>%
  mutate(ind_category = case_when(
    #`Spatial Indicator` %in% loc ~ "Location",
    `Spatial Indicator` %in% ran ~ "Dispersion",
    `Spatial Indicator` %in% occ ~ "Occupancy",
    `Spatial Indicator` %in% agg ~ "Aggregation",
    TRUE ~ NA_character_
  ))

histplot <- plot_auc_hist(bin_freq, ind_stats, L50_test = "Mean")
histplot <- histplot + 
  labs(title="")

ggsave(paste0(save.path, "ROC/", "AUC-histplot-mean.png"), histplot, height = 12, width = 20)

## To colour hist bars by indicator category
bindata %>% 
  group_by(`Spatial Indicator`) %>% 
  filter(L50lvl == "mean") %>% 
  summarise(AUC_mean = mean(AUC), AUC_median = median(AUC), AUC_SD = sd(AUC))

ind_stats <- ind_stats %>%
  tidyr::pivot_longer(cols = c(n, mu, SS, stndrd_dev, se, CI95_margin, lower_ci, upper_ci, median_auc),
                      names_to = "SummaryStat",
                      values_to = "Value")
L50_test <- "Mean"

histplot <-  ggplot() +
    geom_col(data = binfreq[binfreq$L50lvl == L50_test,], 
             aes(x = AUC_bin/10, y = Freq,
                 fill = factor(ind_category, levels = c("Dispersion", "Occupancy", "Aggregation"))),
             colour = "black", 
             just = 1,
             width = 0.1, 
             alpha = 0.4) +
    scale_fill_brewer(palette = "Set2") +
    #scale_fill_manual(values = c("Dispersion" = "vermillion", "Occupancy" = "lightblue", "Aggregation" = "green4")) +
    
    geom_vline(data = ind_stats[ind_stats$SummaryStat %in% c("mu", "median_auc") & ind_stats$L50lvl == "mean",], 
               aes(xintercept = Value, linetype = factor(SummaryStat, labels = c("Median", "Mean"))), linewidth = 1, colour = c("grey10")) +
    labs(x = "AUC Bin", y = "Frequency") +
    facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scales = "free_x") +
    scale_x_continuous(breaks = seq(0,1, by=.1), expand = c(0,0.05)) +
    scale_y_continuous(breaks = seq(0,20, by = 2), expand = c(0,0)) +
    theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          # Panels
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          panel.background   = element_blank(),
          panel.border       = element_rect(colour = "black", fill = NA),
          strip.background   = element_rect(colour = "black"),
          strip.text         = element_text(size = 20),
          # Legend
          legend.position = "right",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
    labs(linetype = "Central Tendency",
         fill = "Indicator Category") +
    coord_cartesian(ylim = c(0,17))
histplot
ggsave(paste0(save.path, "ROC/", "AUC-histplot-mean.png"), histplot, height = 10, width = 14)

# Heatmaps ####
# Plot heatmap of AUCs with performance categories overlaid ~~ ONLY FOR L50 = Mean ~~
ind_hmap_data      <- create_heatmap_data(bindata[bindata$L50lvl == "mean",], group_vars = c("Spatial Indicator"))
ind_hmap_data_long <- pivot_long_heatmap(ind_hmap_data, col_index = "")
plot_heatmap(ind_hmap_data_long, x_order = indorder)

# 2. Fisheries Guild Level ####
# Calculate mean and variance in AUCs for indicator for each L50 method
fshgld_stats <- auc_stats(bindata, group_vars = c("FisheriesGuild"))
plot_auc_barchart(fshgld_stats[fshgld_stats$L50lvl == "mean",], indorder, facet_var = c("FisheriesGuild"))

fshgld_hmap_data      <- create_heatmap_data(bindata[bindata$L50lvl == "mean",], group_vars = c("Spatial Indicator", "FisheriesGuild"))
fshgld_hmap_data_long <- pivot_long_heatmap(fshgld_hmap_data, col_index = FisheriesGuild)
plot_heatmap(fshgld_hmap_data_long, x_order = indorder)

# 3. Species Level ####
# Calculate mean and variance in AUCs for indicator for each L50 method
spcs_stats <- auc_stats(bindata, group_vars = c("FisheriesGuild", "SpeciesCommonName"))
spcs_order <-  unique(arrange(spcs_stats, FisheriesGuild, SpeciesCommonName)$SpeciesCommonName)
plot_auc_barchart(spcs_stats[spcs_stats$L50lvl == "mean",], facet_var = "SpeciesCommonName", facet_order = spcs_order)

spcs_hmap_data      <- create_heatmap_data(bindata[bindata$L50lvl == "mean",], group_vars = c("Spatial Indicator", "FisheriesGuild", "SpeciesCommonName"))
spcs_hmap_data_long <- pivot_long_heatmap(spcs_hmap_data, col_index = paste0(FisheriesGuild, ": ", SpeciesCommonName))
plot_heatmap(spcs_hmap_data_long, x_order = indorder) 

# 4. Stock Level ####
# Calculate mean and variance in AUCs for indicator for each L50 method
stks_stats <- auc_stats(bindata, group_vars = c("FisheriesGuild", "SpeciesCommonName", "StockKeyLabel"))
stks_order <-  unique(arrange(stks_stats, FisheriesGuild, SpeciesCommonName, StockKeyLabel)$StockKeyLabel)
plot_auc_barchart(stks_stats[stks_stats$L50lvl == "mean",], facet_var = "StockKeyLabel", facet_order = stks_order)

stks_hmap_data      <- create_heatmap_data(bindata[bindata$L50lvl == "mean",], group_vars = c("Spatial Indicator", "FisheriesGuild", "SpeciesCommonName", "StockKeyLabel"))
stks_hmap_data_long <- pivot_long_heatmap(stks_hmap_data, col_index = paste0(FisheriesGuild, ": ", SpeciesCommonName, " (", StockKeyLabel, ")"))
plot_heatmap(stks_hmap_data_long, x_order = indorder)

# 5. Survey Level ####
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/stockleaves.rds")
# Calculate mean and variance in AUCs for indicator for each L50 method
surv_stats <- auc_stats(bindata, group_vars = c("FisheriesGuild", "SpeciesCommonName", "StockKeyLabel", "SurveyName", "Quarter"))
surv_order <-  unique(arrange(surv_stats, FisheriesGuild, SpeciesCommonName, StockKeyLabel)$StockKeyLabel)
surv_order <- rev(stk_leaves2$StockKeyLabel)

surv_hmap_data      <- create_heatmap_data(bindata[bindata$L50lvl == "mean",], group_vars = c("Spatial Indicator", "FisheriesGuild", "SpeciesCommonName", "StockKeyLabel", "SurveyName", "Quarter"))
surv_hmap_data_long <- pivot_long_heatmap(surv_hmap_data, col_index = paste0(FisheriesGuild, ": ", SpeciesCommonName, " (", StockKeyLabel, ") ", SurveyName, " Q", Quarter))

surv_hmap_data_long2 <- surv_hmap_data_long %>%
  rowwise() %>%
  mutate(col_index = strsplit(col_index, ": ")[[1]][2])

surv_heatmap <- plot_heatmap(surv_hmap_data_long2, x_order = indorder) + 
  ylab("Guild, Species, Stock ID, Survey") +
  #labs(subtitle = paste0(marks, collapse = "\n")) +
  #geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
  geom_vline(xintercept = c(3.5, 5.5), linetype = "solid") +
  geom_hline(yintercept = c(6.5, 12.5, 30.5), linewidth = 1) +
  geom_hline(yintercept = c(3.5, 5.5, 14.5, 15.5, 17.5, 18.5, 21.5, 23.5, 26.5, 28.5, 31.5, 32.5, 33.5, 35.5, 37.5, 38.5, 41.5, 45.5, 48.5, 49.5)) +
  #geom_hline(yintercept = seq(1.5, 50, 1), linetype = "dotted") +
  theme(plot.background = element_rect(fill = "white"),
        panel.border    = element_blank(),
        
        plot.title = element_text(hjust = 1.5),
        legend.position = "bottom",         
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0)) +
  ggtitle("") +
  ylab("") +
  xlab("") +
  guides(fill = "none")

surv_heatmap
ggsave(paste0(save.path, "ROC/", "AUC-heatmap-surveylevel-meanL50.png"), surv_heatmap, height = 12, width = 8)

### Split heatmap by fisheries guild
surv_hmap_data_long2_L50 <- surv_hmap_data_long_L50 %>%
  rowwise() %>%
  mutate(col_index = strsplit(col_index, ": ")[[1]][2])

surv_hmap_data_long2 <- surv_hmap_data_long %>%
  rowwise() %>%
  mutate(col_index = strsplit(col_index, ": ")[[1]][2])

# Benthic
surv_heatmap_benth_L50 <- plot_heatmap(surv_hmap_data_long2_L50, x_order = indorder, filter_var = "FisheriesGuild", filter_val = "Benthic") +
  geom_hline(yintercept = seq(3.5, 100, 3), linetype = "dotted") +
  geom_hline(yintercept = c(3.5, 6.5, 9.5, 15.5, 21.5, 24.5, 33.5, 45.5, 48.5, 57.5, 60.5)) +
  geom_vline(xintercept = c(2.5, 5.5, 7.5)) +
  ggtitle("Heatmap of spatial indictaor performance for benthic stocks.\n") +
  ylab("Groups:\nStock, Survey, L50 test") +
  labs(subtitle = paste0(marks, collapse = "\n")) +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = -6),
        legend.position = "bottom",        
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0))  

surv_heatmap_benth_L50
ggsave(paste0(save.path, "ROC/", "AUC-heatmap-surveylevel-L50-benthic.png"), surv_heatmap_benth_L50, height = 13, width = 9)

# Benthic mean L50
surv_heatmap_benth <- plot_heatmap(surv_hmap_data_long2, x_order = indorder, filter_var = "FisheriesGuild", filter_val = "Benthic") +
  geom_hline(yintercept = seq(1.5, 100, 1), linetype = "dotted") +
  geom_hline(yintercept = c(1.5, 2.5, 3.5, 5.5, 7.5, 8.5, 11.5, 15.5, 16.5, 19.5, 20.5, 21.5, 22.5)) +
  geom_vline(xintercept = c(2.5, 5.5, 7.5)) +
  ggtitle("Heatmap of spatial indictaor performance for benthic stocks.\n") +
  ylab("Groups:\nStock, Survey, L50 test") +
  labs(subtitle = paste0(marks, collapse = "\n")) +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = -6),
        legend.position = "bottom",        
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0))  
surv_heatmap_benth
ggsave(paste0(save.path, "ROC/", "AUC-heatmap-surveylevel-benthic.png"), surv_heatmap_benth, height = 13, width = 9)


# Demersal mean L50
surv_heatmap_dem <- plot_heatmap(surv_hmap_data_long2, x_order = indorder, filter_var = "FisheriesGuild", filter_val = "Demersal") +
  geom_hline(yintercept = seq(3.5, 100, 3), linetype = "dotted") +
  geom_hline(yintercept = c(6.5, 9.5, 15.5, 18.5, 27.5, 33.5, 42.5, 48.5,
                            57.5, 60.5)) +
  ggtitle("Heatmap of spatial indictaor performance for demersal stocks.\n") +
  ylab("Groups:\nStock, Survey, L50 test") +
  labs(subtitle = paste0(marks, collapse = "\n")) +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = -3),
        legend.position = "bottom",         
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0))  
surv_heatmap_dem
ggsave(paste0(save.path, "ROC/", "AUC-heatmap-surveylevel-L50-demersal.png"), surv_heatmap_dem, height = 11, width = 9)

# Elasmobranch (Spurdog) mean L50
surv_heatmap_elasmo <- plot_heatmap(surv_hmap_data_long2, x_order = indorder, filter_var = "FisheriesGuild", filter_val = "Elasmobranch") +
  geom_hline(yintercept = seq(1.5, 10, 1), linetype = "dotted") +
  ggtitle("Heatmap of spatial indictaor performance for elasmobranch stocks.\n") +
  ylab("Groups:\nStock, Survey, L50 test") +
  labs(subtitle = paste0(marks, collapse = "\n")) +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = -3),
        legend.position = "bottom",         
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0))  
surv_heatmap_elasmo
ggsave(paste0(save.path, "ROC/", "AUC-heatmap-surveylevel-L50-elasmobranch.png"), surv_heatmap_elasmo, height = 4, width = 9)

# Pelagic mean L50
surv_heatmap_pel <- plot_heatmap(surv_hmap_data_long2, x_order = indorder, filter_var = "FisheriesGuild", filter_val = "Pelagic") +
  geom_hline(yintercept = seq(3.5, 40, 3), linetype = "dotted") +
  geom_hline(yintercept = c(9.5, 15.5)) +
  ggtitle("Heatmap of spatial indictaor performance for pelagic stocks.\n") +
  ylab("Groups:\nStock, Survey, L50 test") +
  labs(subtitle = paste0(marks, collapse = "\n")) +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 6),
        legend.position = "bottom",         
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0))  
surv_heatmap_pel
ggsave(paste0(save.path, "ROC/", "AUC-heatmap-surveylevel-L50-pelagic.png"), surv_heatmap_pel, height = 6, width = 9)

# _________________________________________________________________________ ####

# ROC curve ####
library(rpart)

tst <- rocAll_rem %>% filter(StockKeyLabel == "cod.27.22-24",
                      SurveyName == "BITS",
                      `Spatial Indicator` == "D95",
                      Quarter == 1,
                      SurveyNameIndex == "BITS, BITS-Q1",
                      L50lvl == "mean")

rocAll_rem$roc.id <- paste0(rocAll_rem$StockKeyLabel, 
                            rocAll_rem$SurveyName, 
                            rocAll_rem$SurveyIndex,
                            rocAll_rem$Quarter, 
                            rocAll_rem$SurveyNameIndex, 
                            rocAll_rem$L50lvl,
                            rocAll_rem$`Spatial Indicator`)

ggplot(data= filter(rocAll_rem, 
                    L50lvl == "mean", 
                    `Spatial Indicator` == "POPH",
                    SpeciesCommonName == "Plaice",
                    StockKeyLabel == "ple.27.21-23",
                    SurveyNameIndex == "NS-IBTS, BITS NS-IBTS Q1")) +
  geom_path(aes(x = FPR, y = TPR, colour = roc.id, group = roc.id), alpha = 1) +
  geom_point(aes(x = FPR, y = TPR, colour = roc.id, group = roc.id), alpha = 1) +
  geom_abline(intercept = c(0,0), colour = "blue") +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scale = "free_x")


tst <- filter(rocAll_rem, 
       L50lvl == "mean", 
       `Spatial Indicator` == "POPH",
       SpeciesCommonName == "Plaice",
       StockKeyLabel == "ple.27.21-23",
       SurveyNameIndex == "NS-IBTS, BITS NS-IBTS Q1")
tst$`Spatial Indicator Value` <- as.numeric(tst$`Spatial Indicator Value`)
t.roc <- pROC::roc(tst, response = status, predictor = `Spatial Indicator Value`)
pROC::ggroc(t.roc) + coord_flip()

ROC_plot <-ggplot(data= filter(rocAll_rem, 
                    L50lvl == "mean")) +
  geom_path(aes(x = FPR, y = TPR, colour = AUC, group = roc.id), alpha = 1) +
  scale_colour_continuous(type = "viridis") +
  geom_abline(intercept = c(0,0), linetype = 2, colour = "black") +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scale = "free_x") +
  labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", 
       title = paste0("ROC Curves for each spatial indicator")) +
  theme(axis.text.x = element_text(hjust=1),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right")
ROC_plot
ggsave(paste0(save.path, "ROC/", "ROC-curves.png"), ROC_plot, height = 8, width = 12)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> 
#>                      REDUNDANT CODE TO BE DELETED
#>                      
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Check previous floating point issue
tst <- bindata %>%
  select(`Spatial Indicator`, L50lvl, AUC, AUC_bin) %>%
  filter(`Spatial Indicator` == "D95", L50lvl == "mean") %>%
  arrange(AUC)
tst[9,]$AUC == 0.4
rm(tst)

# Factor L50lvl
bindata$L50lvl <- factor(bindata$L50lvl, levels = c("lowerCI", "upperCI", "mean"), labels = c("Lower CI", "Upper CI", "Mean"))

# Get measures of skewness and central tendency
summary_stats <- bindata %>%
  group_by(L50lvl, `Spatial Indicator`) %>%
  summarise(prop_in_highest_bin = sum(AUC == max(AUC)) / n(),  # Proportion of observations in the highest AUC bin
            meanAUC =   mean(AUC),                                # Mean AUC value
            medianAUC = median(AUC),                              # Median AUC value
            sdAUC = sd(AUC),                                  # Standard deviation of AUC values
            skewnessAUC  = e1071::skewness(AUC),                   # Skewness of AUC distribution
            kurtosisAUC = e1071::kurtosis(AUC)) %>%  # Kurtosis of AUC distribution,
  
  arrange(L50lvl, meanAUC) %>%
  tidyr::pivot_longer(cols = c(prop_in_highest_bin, meanAUC, medianAUC, sdAUC, skewnessAUC, kurtosisAUC),
               names_to = "SummaryStat",
               values_to = "Value") %>%
print(., n=nrow(.)) 

# Refine
bindata2 <- bindata %>%
  group_by(`Spatial Indicator`, L50lvl) %>%
  select(AUC_bin) %>%
  arrange(AUC_bin) %>%
  group_by(`Spatial Indicator`, L50lvl, AUC_bin) %>%
  summarise(n = length(AUC_bin)) %>%
  mutate(cs = cumsum(n)/sum(n)) %>% # cumulatiev frequncy
  tidyr::complete(AUC_bin = 0:min(AUC_bin), fill = list(n = 0, cs = 0)) %>% 
  tidyr::complete(AUC_bin = max(AUC_bin):10, fill = list(n = 1, cs = 1)) %>%
  na.omit() %>%
  print(., n = nrow(.))
head(bindata2)

# Any missing bins?
tst <- bindata2 %>%
  group_by(`Spatial Indicator`, L50lvl) %>%
  mutate(r = list(AUC_bin)) %>%
  mutate(m = as.numeric(paste0(seq(0,10,1)[!seq(0,10,1) %in% sort(unlist(r))], collapse = ","))) %>%
  select(m) %>%
  distinct() %>%
  na.omit() %>%
  rename(AUC_bin = m) %>%
  mutate(n = as.integer(0), cs = as.double(NA)) %>%
  print(., n = nrow(.))


# Add missing AUC bins and use prior cs value to fill NAs
bindata3 <- rbind(bindata2, tst) %>%
  arrange(`Spatial Indicator`, L50lvl, AUC_bin) %>%
  tidyr::fill(cs, .direction = "down") %>%
  print(., n = nrow(.))

# Add bin ranges
bindata3$AUC_binrange <- factor(bindata3$AUC_bin, levels = unique(sort(bindata3$AUC_bin)), labels  = c("0", "0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"))
head(bindata3)

# Add sematic groups
#bindata3 <- bindata3 %>%
#  mutate(AUC_cat = case_when(
#    AUC_binrange %in% c("0-0.1",   "0.1-0.2") ~ "Worst",
#    AUC_binrange %in% c("0.2-0.3", "0.3-0.4") ~ "Bad",
#   AUC_binrange %in% c("0.4-0.5", "0.5-0.6") ~ "Random",
#    AUC_binrange %in% c("0.6-0.7", "0.7-0.8") ~ "Good",
#    AUC_binrange %in% c("0.8-0.9", "0.9-1.0") ~ "Best",
#    TRUE ~ NA_character_ # Return NA for unmatched values
#  ))
head(bindata3)


binmean <- bindata %>%
  filter(L50lvl == "Mean") %>%
  arrange(AUC) %>%
  select(`Spatial Indicator`, AUC, AUC_bin, AUC_cat) %>%
  ungroup() %>%
  group_by(AUC_bin, `Spatial Indicator`) %>%
  reframe(n = length(AUC_bin)) %>%
  tidyr::complete(AUC_bin, fill = list(n = 0)) %>% 
  print(., n = nrow(.))


# Plot frequency 
freqplot <- ggplot(na.omit(bindata3), aes(x = AUC_binrange, y = n)) +
  #geom_bar(stat = "count", aes(group = L50lvl, fill = L50lvl)) +
  geom_point(aes(color = L50lvl)) +
  geom_line(aes(group = L50lvl, color = L50lvl)) +
  geom_area(aes(fill = L50lvl, group = L50lvl), alpha = 0.1, position = "identity") +
  labs(x = "AUC Bins", y = "Frequency", title = "Frequency of AUC Bins") +
  scale_color_manual(values = c("grey70", "grey40", "cyan4"), name = "L50") +
  scale_fill_manual(values = c("NA", "NA", "cyan4"), name = "L50") +
  #scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_x_discrete(labels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0")) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scale = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) 
freqplot

# Plot cumulative sum
cumfreqplot <- ggplot(data = bindata3) +
  geom_line(aes(x = AUC_bin/10, y = cs, colour = `L50lvl`, linewidth = L50lvl, alpha = L50lvl), linetype = 1) +
  labs(x = "AUC", y = "Cumulative Proportion", title = "Cumulative Frequency Plot") +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = 1, linewidth = 0.5) +  # Add identity line
  geom_vline(data = summary_stats[summary_stats$SummaryStat %in% c("meanAUC", "medianAUC"),], aes(xintercept = Value, colour = L50lvl, alpha = L50lvl, linetype = SummaryStat)) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scales = "free_x") +
  scale_linetype_manual(values = c(2,1), name = "AUC Central Tendenccy", labels = c("Mean", "Median")) +
  scale_linewidth_manual(values = c(0.9, 0.9, 1),              name = "L50") +
  scale_alpha_manual    (values = c(0.7, 0.7, 0.8),              name = "L50") +
  scale_color_manual    (values = c("cyan3", "red2", "black"), name = "L50") +
  scale_x_continuous(breaks = seq(0,1, by=.1)) +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right")

cumfreqplot
head(bindata3[bindata3$L50lvl == "Mean" & bindata3$`Spatial Indicator` == "D95",], 12)

# Histogram
rect_df <- data.frame(
  xmin = c(0, 0.2, 0.4, 0.6, 0.8),
  xmax = c(0.2, 0.4, 0.6, 0.8, 1),
  ymin = rep(-Inf, 5),
  ymax = rep(Inf, 5),
  color = c("red", "lightpink", "lightyellow", "lightgreen", "green3")
)

histplot <- ggplot() +
  #geom_bar(stat = "count", aes(group = L50lvl, fill = L50lvl)) +
  #geom_rect(data = rect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color), alpha = 0.1) +
  #scale_fill_identity() +
  geom_col(data = bindata3[bindata3$L50lvl == "Mean",], aes(x = AUC_bin/10, y = n, fill = factor(AUC_cat, levels = c("Worst", "Bad", "Random", "Good", "Best")))) +
  scale_fill_manual(values = c("Worst" = "grey20", "Bad" = "grey40", "Random" = "grey60", "Good" = "grey80", "Best" = "cyan4")) +
  
  geom_vline(data = summary_stats[summary_stats$SummaryStat %in% c("meanAUC", "medianAUC") & summary_stats$L50lvl == "Mean",], aes(xintercept = Value, linetype = factor(SummaryStat, labels = c("Mean", "Median")))) +
  labs(x = "AUC Bins", y = "Frequency", title = "Histogram of AUC Scores (Mean L50)") +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scales = "free_x") +
  scale_x_continuous(breaks = seq(0,1, by=.1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,20, by = 2), expand = c(0,0)) +
  theme(axis.text.x = element_text(hjust=1),
    # Panels
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black"),
    # Legend
    legend.position = "right") +
  labs(fill = "AUC Performance Category",
       linetype = "Summary Statistics") +
  coord_cartesian(ylim = c(0,17))
histplot


bindata %>%
  filter(L50lvl == "Mean",
        `Spatial Indicator` == "D95") %>%
  arrange(AUC) %>%
  select(`Spatial Indicator`, AUC, AUC_bin) %>%
  ungroup() %>%
  group_by(AUC_bin, `Spatial Indicator`) %>%
  reframe(n = length(AUC_bin)) %>%
  tidyr::complete(AUC_bin, fill = list(n = 0)) %>% 
  print(., n = nrow(.))

  ggplot() + 
  geom_col(data = tst, aes(AUC_bin, n)) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scales = "free_x") 
  


hist(bindata[bindata$L50lvl == "Mean" & bindata$`Spatial Indicator` == "D95",]$AUC)

ggplot(data = auc_summary, aes(x = AUC, fill = L50lvl)) +
  geom_density(alpha = 0.5) +
  geom_freqpoly(aes(y = after_stat(density), 
                   colour = L50lvl), 
                binwidth = 0.1, linewidth = 1) +
  labs(x = "AUC", y = "Density", title = "Density of AUC Values") +
  theme_minimal() +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = c(indorder))), scale = "free_x") 
  xlim(c(0,1))

bindata %>% 
  select(AUC, AUC_bin, AUC_cat) %>%
  filter(L50lvl == "Mean",
         `Spatial Indicator` == "D95") %>%
  arrange(AUC)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#                                 EXPLORE                                   ####
#>                                
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

## POPH ####
data.poph <- auc_summary %>% filter(`Spatial Indicator` == "POPH")
prop.poph <- auc_prop %>% filter(`Spatial Indicator` == "POPH")

## Which stocks did it perform well for? (For all L50s)
poph.sum <- data.poph %>% 
  group_by(StockKeyLabel) %>%
  mutate(N.sur  = length(unique(SurveyNameIndex))) %>%
  filter(AUC >= 0.75) %>%
  group_by(StockKeyLabel, SurveyNameIndex) %>%
         mutate(L50 = length(unique(L50lvl))) %>%
  filter(L50 >= 3) %>%
  select(-c(L50, L50lvl, AUC)) %>%
  distinct() %>%
  group_by(StockKeyLabel) %>%
  mutate(SurGood = length(unique(SurveyNameIndex)),
         PropSurGd = paste0(SurGood, "/", N.sur)) %>%
  arrange(FisheriesGuild, StockKeyLabel, AvgSurveyCoverage) %>%
  print(n = nrow(.))  

# How many time series was it good for? (27)
goodTS <- nrow(poph.sum)

# How many time series are there? (52)
nTS <- nrow(data.poph %>%
  ungroup() %>%
  select(StockKeyLabel, SurveyNameIndex, Quarter) %>%
  distinct())

# Prop
goodTS/nTS*100

poph.sum %>%
  select(StockKeyLabel, FisheriesGuild, PropSurGd) %>%
  distinct()




# How many time series was it good for?




rowSums(t[2:5])
colSums(t[2:5])
sum(colSums(t[2:5])) # total obvs = 52 survyes * 12 indictaors 

auc_prop


plot.poph <- ggplot() +
  geom_col(data = data.poph[data.poph$L50lvl == "mean",], aes(x = xaxis, y = AUC, fill = FisheriesGuild), position = position_dodge(width = 1), width = 1) +
  geom_line(data = data.poph[data.poph$L50lvl != "mean",],  aes(x = xaxis, y = AUC, group = xaxis)) +
  geom_point(data = data.poph[data.poph$L50lvl != "mean",], aes(x = xaxis, y = AUC, shape = factor(L50lvl, level = c("lowerCI", "upperCI"), labels = c("Lower CI", "Upper CI")), colour = factor(L50lvl, level = c("lowerCI", "upperCI"), labels = c("Lower CI", "Upper CI")))) +
  facet_wrap(vars(StockKeyLabel), scale = "free_x") +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right")

  scale_fill_identity("Stock ID", guide = "legend", labels = levels(data.poph$StockKeyLabel)) +
  geom_hline(yintercept = 0.5,  colour = "grey20", lty = 1) +
  geom_hline(yintercept = 0.75, colour = "grey20", lty = 2) +
  geom_label(data = prop.poph, aes(x = length(unique(prop.poph$xaxis)) + 7, y = 1,     label = c(paste0(prop.poph$propgood, "%"))), fill = "forestgreen", alpha = 0.6, size = 3.3) +
  geom_label(data = prop.poph, aes(x = length(unique(prop.poph$xaxis)) + 7, y = 0.625, label = c(paste0(prop.poph$propavg, "%"))),  fill = "gold3", alpha = 0.7, size = 3.3) +
  geom_label(data = prop.poph, aes(x = length(unique(prop.poph$xaxis)) + 7, y = 0.1,   label = c(paste0(prop.poph$propbad, "%"))),  fill = "red4", alpha = 0.8, size = 3.3) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right") +
  xlab("Survey Name, Survey Index") +
  labs(shape = "L50 Confidence Intervals (CI)", colour = "L50 Confidence Intervals (CI)") +
  guides(color = guide_legend(override.aes = list(size = 3)))

rocAll_long %>%
  filter(`Spatial Indicator` == "POPH")

lm:


rbind(head(data.poph), tail(data.poph))




  
# Some indicators may perform well but this may be due to lack of contrast/few years of data in the timeseries 

ROContrast <- rocAll_long %>%
  select(StockKeyLabel, Year, Quarter, SurveyNameIndex, status, `Spatial Indicator Value`) %>%
  na.omit() %>%
  select(-`Spatial Indicator Value`) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter) %>%
  summarise(N.years = length(Year),
            N.poor  = length(which(status == FALSE)),
            N.good  = length(which(status == TRUE)),
            Ratio.P2G   = N.poor/N.good) %>%
  print(n = nrow(.))

# What if we removed these uniformative suveys? >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
hist(ROContrast$Ratio.P2G[ROContrast$Ratio.P2G < 15], breaks = 100)

removals <- ROContrast %>%
  mutate(contrast = if_else(Ratio.P2G == 0 | is.infinite(Ratio.P2G ), 9, if_else(Ratio.P2G < 0.25 | Ratio.P2G > 4, 4 , 0)),
         years    = if_else(N.years < 15, 1, 0),
         rem      = contrast + years) %>%
  arrange(-rem, N.years, Ratio.P2G) %>%
  print(n = nrow(.))

rocAll_rem <- merge(rocAll_long, removals) %>%
  filter(!rem > 0)

auc_summary_rem <- rocAll_rem %>%
  select(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`, AUC) %>%
  distinct() %>%
  group_by(StockKeyLabel, SurveyNameIndex, Quarter, `Spatial Indicator`) %>%
  na.omit() %>%
  summarise(AUC) %>%
  mutate(xaxis = paste0(StockKeyLabel, ":", SurveyNameIndex, ", ", Quarter))

Nsurvs <- length(unique(as.character(auc_summary_rem$xaxis[!is.na(auc_summary_rem$AUC)]))) # where AUC is available. 

auc_prop_rem <- auc_summary_rem %>%
  ungroup() %>%
  select(`Spatial Indicator`, AUC) %>%
  group_by(`Spatial Indicator`) %>%
  mutate(propgood = round(length(AUC[AUC>=0.75])/Nsurvs*100,1),
         propavg = round(length(AUC[AUC>0.5 & AUC <0.75])/Nsurvs*100,1),
         propbad = round(length(AUC[AUC<=0.5])/Nsurvs*100,1)) %>%
  select(-AUC) %>%
  distinct()

ggplot() +
  geom_col(data = auc_summary_rem, aes(x = xaxis, y = AUC, fill = StockKeyLabel), position = position_dodge(width = 2), width = 0.8) +
  geom_hline(yintercept = 0.5,  colour = "grey20", lty = 1) +
  geom_hline(yintercept = 0.75, colour = "grey20", lty = 2) +
  geom_text(data = auc_prop_rem, aes(x = length(unique(auc_prop_rem$xaxis)) + 10, y = 1,     label = c(paste0(auc_prop_rem$propgood, "%"))), size = 3.3) +
  geom_text(data = auc_prop_rem, aes(x = length(unique(auc_prop_rem$xaxis)) + 10, y = 0.625, label = c(paste0(auc_prop_rem$propavg, "%"))),  size = 3.3) +
  geom_text(data = auc_prop_rem, aes(x = length(unique(auc_prop_rem$xaxis)) + 10, y = 0.25,  label = c(paste0(auc_prop_rem$propbad, "%"))),  size = 3.3) +
  facet_wrap(vars(factor(`Spatial Indicator`, levels = indorder))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        # Panels
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA),
        strip.background   = element_rect(colour = "black"),
        # Legend
        legend.position = "right") +
  xlab("Survey Name, Survey Index")

colrs <- c("brown4", "gold3", "forestgreen")
auc_prop2_rem <- auc_prop_rem %>% 
  tidyr::pivot_longer(cols = c("propgood", "propavg", "propbad"),
                      names_to = "PropCat",
                      values_to = "Prop") %>%
  mutate(colr = if_else(PropCat == "propbad", colrs[1], 
                        if_else(PropCat == "propavg", colrs[2], colrs[3])),
         colr = factor(colr, levels = c(colrs[1], colrs[2], colrs[3])))

ggplot() +
  geom_col(data = auc_prop2_rem, aes(x = factor(`Spatial Indicator`, level = indorder), y = Prop, fill = colr)) +
  scale_fill_identity("", guide = "legend", labels = c("AUC <= 0.5", "0.5 < AUC < 0.75 ", "AUC >= 0.75")) +
  ylab("Percetage of Survey Indices")



m <- lm(AUC_mean ~ K, data = t.auctss)
summary(m)

cor.test(t.auctss$AUC_mean, t.auctss$)


n <- nrow(t.auctss)
df <- n -2
alpha <- 0.5
crit <- qt(1-alpha/2, df)
t_AUC_K <- cor_AUC_K * sqrt((n - 2) / (1 - cor_AUC_K^2))
cor_AUC_K_significant <- abs(t_AUC_K) > crit
print(paste("Correlation between AUC_mean and K is significant:", cor_AUC_K_significant))



stksurveys <- read_xlsx(paste0(load.path, "icesSA_data/icesData-31stks-AY2022-stksurveys-optim.xlsx"), sheet = "Surveys")
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/spatind-1/boot/initial/data/ices_shp/ICES Rect/ices_rect.rds")



tst <- stksurveys %>%
  group_by(StockKeyLabel) %>%
  mutate(divs =  strsplit(Divisions, ", ")) %>%
  select(StockKeyLabel, Divisions, divs) %>%
  distinct() %>%
  mutate(divmiss = if_else(any(!divs %in% ices_rect$Area_27), divs[!divs %in% ices_rect$Area_27], NA))

areas <- sort(unique(ices_rect$Area_27))
divs <- unique(unlist(strsplit(stksurveys$Divisions, ", ")))
missdivs <- divs[!divs %in% areas]
stringr::str_remove(missdivs, "NEA")

View(stksurveys[which(strsplit(stksurveys$Divisions, ", ") %in% missdivs),])

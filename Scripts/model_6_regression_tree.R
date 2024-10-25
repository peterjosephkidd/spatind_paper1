#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                      Random Forest/Regression tree
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(dplyr)
library(ggplot2) 
library(randomForest)
library(party)
library(partykit)
library(plotrix)
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(vip)
library(rattle)

rm(list = ls())

# Reptree ####
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)


if(!('reprtree' %in% installed.packages())){
  install_github('munoztd0/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))


# Load auc data ####
load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/"
source("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Functions/results_funs.R")

### AUC Data
load(paste0(load.path, "ROC/auc_summary_good_surveys.xlsx.rds"))  
### Survey Coverage data
survey_coverage <- read_xlsx("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/icesData-stksurveys-survcoverage.xlsx")
### Expected survyes for cross checking
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/expected-surveys.rds")
### Info on ecorgeions, WGs, size guilds etc for each stok
stock_info <- read_xlsx("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/icesSA_data/icesData-AllSurveyData-manual.xlsx", sheet = "Stocks")
### L50 data
load("~/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/FishBaseMaturity/FishBase-L50.rds")

# Tidy data ####
# The number of rectnagles in each stock region
stock_area <- survey_coverage %>% 
  select(StockKeyLabel, StkRects) %>% 
  distinct()

# Some grop variables for each stock
stock_vars <- stock_info %>% 
  select(StockKeyLabel, EcoRegion, TrophicGuild, SizeGuild)

# The ecoregions that each stock encompasses
ecoregions <- stock_info %>%
  select(StockKeyLabel, EcoRegion) %>%
  mutate(EcoRegion = strsplit(EcoRegion, ", ")) %>%
  tidyr::unnest(., EcoRegion) %>%
  mutate(Present = 1,
         EcoRegion = gsub(" ", "", EcoRegion))

ecoregions_wide <- tidyr::pivot_wider(ecoregions, names_from = EcoRegion, values_from = Present, values_fill = 0)

# Add mean L50 value
Lmat <- rename(Lmat, L50MeanVal = mu.stk)

# Add Numerical Stock ID
stockno <- auc_summary %>%
  ungroup() %>%
  select(StockKeyLabel) %>%
  distinct() %>%
  mutate(StockNo = order(StockKeyLabel)) %>%
  print(., n  = nrow(.))

# Merge extra variables
auc_summary <- left_join(auc_summary, stockno, by = "StockKeyLabel")
auc_summary <- left_join(auc_summary, stock_area[c("StockKeyLabel", "StkRects")], by = "StockKeyLabel")
auc_summary <- left_join(auc_summary, stock_vars, by = "StockKeyLabel")
auc_summary <- left_join(auc_summary, ecoregions_wide, by = "StockKeyLabel")
auc_summary <- left_join(auc_summary, Lmat[c("StockKeyLabel", "L50MeanVal", "Linf", "Lmat_Linf", "Family", "Order", "Class", "Genus", "Species")], by = "StockKeyLabel")

# Factorise categorical variables
auc_summary <- auc_summary %>%
  rename(SpatialIndicator = `Spatial Indicator`) %>%
  mutate_at(vars(SurveyName, StockKeyLabel, SpeciesCommonName, SpeciesScientificName, Quarter, ind_category, SpatialIndicator, L50lvl, FisheriesGuild, Family, Order, Class, Genus, Species), factor) %>%
  group_by(StockKeyLabel) %>%
  mutate(StockNo = order(unique(StockKeyLabel)))
head(auc_summary)

#___________________________________________________________________________####
# Predictors ####
set.seed(2512)

names(auc_summary)
# Define predictor variables

pred_set1 <-  gsub(" ", "", c(### Survey Predictors 
               "SurveyName",
               "Quarter",
               "AvgSurveyCoverage", 
               "YearLength",
               "Ratio.P2G",
               ### Stock Predictors
               "StockKeyLabel",
               "Bay of Biscay and the Iberian Coast Ecoregion",
               "Celtic Seas Ecoregion",
               "Greater North Sea Ecoregion",
               "Oceanic Northeast Atlantic Ecoregion",
               "Greenland Sea Ecoregion",
               "Iceland Sea Ecoregion",
               "Faroes Ecoregion",
               "Arctic Ocean Ecoregion",
               "Norwegian Sea Ecoregion",
               "Baltic Sea Ecoregion", 
               "Azores Ecoregion",
               "Barents Sea Ecoregion",
               "StkRects",
               ### Species Predictors
               "Family",
               "Order",
               "Class",
               "Genus",
               "Species",
               "FisheriesGuild",
               "SizeGuild",
               "TrophicGuild",
               "GrowthRateK",
               "L50MeanVal",
               "L50lvl",
               "Linf",
               "Lmat_Linf",
               ### Indicator predictors
               "SpatialIndicator",
               "ind_category"))

pred_set2 <-  gsub(" ", "", c(### Survey Predictors 
  #"SurveyName",
  "Quarter",
  #"AvgSurveyCoverage", 
  "YearLength",
  #"Ratio.P2G",
  ### Stock Predictors
  #"StockKeyLabel",
  #"Bay of Biscay and the Iberian Coast Ecoregion",
  #"Celtic Seas Ecoregion",
  #"Greater North Sea Ecoregion",
  #"Oceanic Northeast Atlantic Ecoregion",
  #"Greenland Sea Ecoregion",
  #"Iceland Sea Ecoregion",
  #"Faroes Ecoregion",
  #"Arctic Ocean Ecoregion",
  #"Norwegian Sea Ecoregion",
  #"Baltic Sea Ecoregion", 
  #"Azores Ecoregion",
  #"Barents Sea Ecoregion",
  "StkRects",
  ### Species Predictors
  "Family",
  "Order",
  "Class",
  "Genus",
  "Species",
  "FisheriesGuild",
  "SizeGuild",
  "TrophicGuild",
  "GrowthRateK",
  "L50MeanVal",
  "L50lvl",
  "Linf",
  "Lmat_Linf",
  ### Indicator predictors
  "SpatialIndicator",
  "ind_category"))

pred_set3 <-  gsub(" ", "", c(### Survey Predictors 
  "SurveyName",
  "Quarter",
  "AvgSurveyCoverage", 
  "YearLength",
  #"Ratio.P2G",
  ### Stock Predictors
  #"StockKeyLabel", 
  "Bay of Biscay and the Iberian Coast Ecoregion",
  #"Celtic Seas Ecoregion",
  #"Greater North Sea Ecoregion",
  #"Oceanic Northeast Atlantic Ecoregion",
  #"Greenland Sea Ecoregion",
  #"Iceland Sea Ecoregion",
  #"Faroes Ecoregion",
  #"Arctic Ocean Ecoregion",
  #"Norwegian Sea Ecoregion",
  #"Baltic Sea Ecoregion", 
  #"Azores Ecoregion",
  #"Barents Sea Ecoregion",
  "StkRects",
  ### Species Predictors
  #"SpeciesCommonName",
  "FisheriesGuild",
  "SizeGuild",
  "TrophicGuild",
  "GrowthRateK",
  "L50MeanVal",
  ### Indicator predictors
  "SpatialIndicator",
  "ind_category"
  #"L50lvl"
  ))





# Feature selection ####
# Is L50 an important feature?
preds <- pred_set1

data <- auc_summary %>%
  ungroup() %>%
  select(AUC, all_of(preds))

preds <- pred_set1[pred_set1!="L50MeanVal"]

subsets <- c(1:length(preds))

rfe_ctrl <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv", 
                       repeats = 5,
                       number = 10,
                       verbose = F)

feature_sel <- rfe(x = data[,-1], 
                   y = as.matrix(data[,1]),
                 sizes = subsets,
                 rfeControl = rfe_ctrl)

save(feature_sel, file = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/feature_selection.rds")
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/feature_selection.rds")

feature_sel
predictors(feature_sel)
plot(feature_sel, type=c("g", "o"))

# L50 is not an important feature
# Filter to L50 mean data and remove L50lvl as a predictor
# Re-run feature selection to see what is now important
L50mean_data <- data %>%
  filter(L50lvl == "mean") %>%
  select(-c("L50lvl", "Ratio.P2G", "AvgSurveyCoverage", "YearLength"))

preds <- pred_set1[!pred_set1 %in% c("L50lvl", "Ratio.P2G", "AvgSurveyCoverage", "YearLength")]

feature_sel2 <- rfe(x = L50mean_data[,-1], 
                   y = as.matrix(L50mean_data[,1]),
                   sizes = subsets,
                   rfeControl = rfe_ctrl)

save(feature_sel2, file = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/feature_selection_meanL50.rds")
load("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/feature_selection_meanL50.rds")

feature_sel2
predictors(feature_sel2)
plot(feature_sel2, type=c("g", "o"))


# Regression Tree ####
# Fit a regression tree using the selected features as predictors of AUC
features <- predictors(feature_sel2)
#features <- pred_set2[!pred_set2 %in% c("YearLength", "L50lvl")]
formula <- paste("AUC ~ ", paste(features, collapse = " + "))
reg_tree <- rpart(formula, data = L50mean_data)

printcp(reg_tree)
plotcp(reg_tree)
vip(reg_tree, num_features = 40, bar = FALSE, geom = "col")
rpart.plot(reg_tree, cex = 0.5, type = 5)

# Pruning ####
# Prune the regression tree
rpartCtrl <- rpart.control(cp = 0.026)

pruned_tree <- rpart(formula, 
                     data = L50mean_data,
                     control = rpartCtrl)
printcp(pruned_tree)

rpart.plot(pruned_tree, cex = 0.5, type = 5)

# Feature Importance ####
vip(pruned_tree, num_features = 40, bar = FALSE, geom = "col")

importance <- data.frame("vip" = pruned_tree$variable.importance)
importance$feature <- rownames(importance)
rownames(importance) <- NULL
head(importance)

importance$feature <- factor(importance$feature, 
                             levels = arrange(importance, vip)$feature, 
                             labels = rev(c("Stock ID",
                                        "Species", 
                                        "Genus",
                                        "Survey", 
                                        "Indicator Category",
                                        "Survey Quarter")))

vip_plot <- ggplot(data = importance, aes(x = vip, y = feature, fill = type)) +
  geom_col(fill = "cyan4", colour = "black") + 
  scale_x_continuous(expand = c(0.01,0)) +
  xlab("Importance") +
  ylab("Feature") +
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
vip_plot

ggsave("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/variable_importance.png", 
       vip_plot, height = 2.5, width = 3.5)

# Tree plots ####
rpart.plot(pruned_tree, 
           cex = 1, 
           type = 1,
           extra = 101,
           clip.facs = T,
           under.percent = 1,
           uniform = T,
           nn = T,
           under = T,
           split.box.col = "cyan4",
           split.border.col = "black",
           split.cex = 0.6,
           branch.type = 0,
           branch.fill = "grey")

fancyRpartPlot(pruned_tree,
               cex = 0.5,
               clip.facs = T,
               split.shadow.col = 0,
               shadow.col = "white")

prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=101, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=2,
    cex = 0.5,
    nn = T,
    uniform = T,
    clip.facs = T,
    compress = T,
    ycompress = T,
    Margin = 0,
    space = 0,
    gap = 0,
    #box.col = "beige",
    box.palette = c("white", "lightblue", "lightblue4", "cyan4", "blue4"),
    split.box.col = "lightgrey",
    #fallen.leaves = T,
    split.prefix = "is ",
    split.suffix = "?") #display 5 decimal places in output


## Leaves ####
# Look at the groupings

l8 <- L50mean_data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         !Species %in% c("morhua", "acanthias", "merluccius", "scombrus", "piscatorius", "platessa", "solea", "merlangus")) %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 8)

l7 <- L50mean_data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         Species %in% c("morhua", "acanthias", "merluccius", "scombrus", "piscatorius", "platessa", "solea", "merlangus"),
         ind_category != "Dispersion", 
         !SurveyName %in% c("EVHOE", "SWC-IBTS", "SP-NORTH", "SNS")) %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 7)

l6 <- L50mean_data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         Species %in% c("morhua", "acanthias", "merluccius", "scombrus", "piscatorius", "platessa", "solea", "merlangus"),
         ind_category != "Dispersion", 
         SurveyName %in% c("EVHOE", "SWC-IBTS", "SP-NORTH", "SNS"),
         !StockKeyLabel %in% c("mac.27.nea", "ple.27.420")) %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 6)

l5 <- L50mean_data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         Species %in% c("morhua", "acanthias", "merluccius", "scombrus", "piscatorius", "platessa", "solea", "merlangus"),
         ind_category != "Dispersion", 
         SurveyName %in% c("EVHOE", "SWC-IBTS", "SP-NORTH", "SNS"),
         StockKeyLabel %in% c("mac.27.nea", "ple.27.420")) %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 5)

l4 <- L50mean_data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         Species %in% c("morhua", "acanthias", "merluccius", "scombrus", "piscatorius", "platessa", "solea", "merlangus"),
         ind_category == "Dispersion", 
         !StockKeyLabel %in% c("cod.27.22-24", "ple.27.7a", "ple.27.7d", "sol.27.4", "whg.27.6a", "whg.27.7b-ce-k")) %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 4)

l3 <- L50mean_data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         Species %in% c("morhua", "acanthias", "merluccius", "scombrus", "piscatorius", "platessa", "solea", "merlangus"),
         ind_category == "Dispersion", 
         StockKeyLabel %in% c("cod.27.22-24", "ple.27.7a", "ple.27.7d", "sol.27.4", "whg.27.6a", "whg.27.7b-ce-k")) %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 3)

l2 <- L50mean_data %>%
  filter(StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SurveyName != "NS-IBTS") %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 2)
  
l1 <- L50mean_data %>%
  filter(StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                              "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SurveyName == "NS-IBTS") %>%
  select(StockKeyLabel, Species, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = 1)

stk_leaves <- rbind(l1,l2,l3,l4,l5,l6,l7,l8)
stk_leaves2 <- stk_leaves %>%
  group_by(StockKeyLabel, SurveyName, Quarter) %>%
  arrange(-LeafNo) %>%
  summarise(LeafNo = paste0(LeafNo, collapse = " & ")) %>%
  arrange(LeafNo) %>%
  print(., n = nrow(.))

save(stk_leaves2, file = "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/stockleaves.rds")

## Heatmap ####

hmap_auc <- auc_summary %>%
  rename(`Spatial Indicator` = SpatialIndicator)
hmap_auc <- auc_summary
bindata <- create_auc_bindata(hmap_auc, bins = seq(0,1, by = 0.1))

marks <- c("Bins:",
           "++  (AUC >= 0.8)",
           "+    (0.6 <= AUC < 0.8)",
           "-     (0.2 < AUC <= 0.6)",
           "--    (AUC <= 0.2)")

# Calculate mean and variance in AUCs for indicator for each L50 method
surv_stats <- auc_stats(bindata, group_vars = c("FisheriesGuild", "SpeciesCommonName", "StockKeyLabel", "SurveyName", "Quarter"))
# calculate mean AUC across L50 tests, and variance in the means -- i.e. mean of means
surv_stats_avgL50 <- auc_stats(bindata, group_vars = c("FisheriesGuild", "SpeciesCommonName", "StockKeyLabel", "SurveyName", "Quarter"), avg_across_L50 = TRUE)

#surv_order <-  unique(arrange(surv_stats_avgL50, FisheriesGuild, SpeciesCommonName, StockKeyLabel)$StockKeyLabel)


indorder <- c(#"CoG (x)", "CoG (y)",      # Location
  "Inertia", "EOO", "ELA",   # Dispersion
  "POPR", "POPH",            # Occupancy
  "Gini Index", "D95", "SA", # Aggregation 
  "EA", "SPI")

# Heatmaps
surv_hmap_data      <- create_heatmap_data(bindata[bindata$L50lvl == "mean",], group_vars = c("Spatial Indicator", "FisheriesGuild", "SpeciesCommonName", "StockKeyLabel", "SurveyName", "Quarter"))
surv_hmap_data <- left_join(surv_hmap_data, stk_leaves2, by = c("StockKeyLabel", "SurveyName", "Quarter"))

surv_hmap_data_long <- pivot_long_heatmap(surv_hmap_data, col_index = paste0(LeafNo, ": ", SpeciesCommonName, " (", StockKeyLabel, ") ", SurveyName, " Q", Quarter))

surv_heatmap <- plot_heatmap(surv_hmap_data_long, x_order = indorder) + 
  ylab("Guild, Species, Stock ID, Survey") +
  #labs(subtitle = paste0(marks, collapse = "\n")) +
  #geom_hline(yintercept = c(6.5, 50.5, 64.5, 75.5)) +
  #geom_hline(yintercept = c(20.5, 25.5, 38.5, 48.5, 57.5, 76.5, 79.5, 83.5, 86.5)) +
  geom_vline(xintercept = c(3.5, 5.5), linetype = "solid") +
  #geom_hline(yintercept = c(6.5, 12.5, 30.5), linewidth = 1) +
  #geom_hline(yintercept = c(3.5, 5.5, 14.5, 15.5, 17.5, 18.5, 21.5, 23.5, 26.5, 28.5, 31.5, 32.5, 33.5, 35.5, 37.5, 38.5, 41.5, 45.5, 48.5, 49.5)) +
  #geom_hline(yintercept = seq(1.5, 50, 1), linetype = "dotted") +
  theme(plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 1.5),
        legend.position = "bottom",         
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 0)) +
  ggtitle("") +
  ylab("") +
  xlab("") +
  guides(fill = "none")

surv_heatmap

ggsave("C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/ROC/AUC-heatmap-surveylevel-leaf-numbers.png",
       surv_heatmap, height = 12, width = 8)


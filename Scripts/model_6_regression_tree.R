#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>                      Random Forest/Regression tree
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> This script uses AUC data from model_5_ROC.R. We are interested in what 
#> predicts AUC. First, we look at whether L50 condition is a good predictor of 
#> AUCs. We include the data/AUCs from the three L50 conditions. 
#> We do this by performing recursive feature elimination (RFE). We then 
#> perform RFE again (without L50 condition as a predictor), to predict the AUCs
#> from just the outputs from when L50 mean was used to identify matures in 
#> survey data. Using the key features identified by the RFE, we then 
#> perform regression tree analysis. 

library(dplyr) 
library(ggplot2)
library(caret) 
library(readxl) 
library(rpart)
library(vip)
library(rpart.plot)
library(rattle)


library(randomForest)
library(party)
library(partykit)
library(plotrix)

rm(list = ls())

# Load data and functions
load(paste0(getwd(), "/Output/Data/ROC/auc_summary.rds"))         # AUC Data
load(paste0(getwd(), "/Output/Data/ROC/rocRem_long_surveys.rds")) # final survey list
load(paste0(getwd(), "/Output/Data/ROC/rocRem_long_stocks.rds"))  # final stock list
stock_info <- read_xlsx(paste0(getwd(), "/Data/Initial/DR_Stocks/StockInfo/icesData-AllSurveyData-manual.xlsx"), sheet = "Stocks")
load(paste0(getwd(), "/Data/Generated/DR_Stocks/FishBaseMaturity/FishBase-L50.rds")) # Life history data
source(paste0(getwd(), "/Functions/results_funs.R"))              # user functions 

suppressWarnings(dir.create(paste0(getwd(), "/Output/Data/RFE"), recursive = T))
suppressWarnings(dir.create(paste0(getwd(), "/Output/Data/RegTree"), recursive = T))

# Tidy data ####
# Some grop variables for each stock
stock_vars <- stock_info %>% 
  filter(StockKeyLabel %in% stk_names_rem) %>%
  select(StockKeyLabel, EcoRegion, TrophicGuild, SizeGuild)

# One hot encode ecoregion
ecoregions <- stock_info %>%
  filter(StockKeyLabel %in% stk_names_rem) %>%
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
auc_summary <- left_join(auc_summary, stock_vars, by = "StockKeyLabel")
auc_summary <- left_join(auc_summary, ecoregions_wide, by = "StockKeyLabel")
auc_summary <- left_join(auc_summary, Lmat[c("StockKeyLabel", "L50MeanVal", "Linf", "Lmat_Linf", "Family", "Order", "Class", "Genus", "Species")], by = "StockKeyLabel")

# Factorise categorical variables
auc_summary <- auc_summary %>%
  mutate_at(vars(SurveyName, StockKeyLabel, SpeciesCommonName, SpeciesScientificName, Quarter, ind_category, Indicator, L50lvl, FisheriesGuild, Family, Order, Class, Genus, Species), factor) %>%
  group_by(StockKeyLabel) %>%
  mutate(StockNo = order(unique(StockKeyLabel)))
head(auc_summary)

#___________________________________________________________________________####
# Predictors ####
set.seed(2512)

# Define predictor variables (predicting AUC)
names(auc_summary)
pred_set  <-  gsub(" ", "", c(### Survey Predictors 
               "SurveyName",
               "Quarter",
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
               ### Species Predictors
               "Family",
               "Order",
               "Class",
               "Genus",
               "SpeciesScientificName",
               "FisheriesGuild",
               "SizeGuild",
               "TrophicGuild",
               "GrowthRateK",
               "L50MeanVal",
               "L50lvl",
               "Linf",
               "Lmat_Linf",
               ### Indicator predictors
               "Indicator",
               "ind_category"))

# Recursive Feature Elimination ################################################
# Are there differences in AUC between the three L50 conditions?
preds <- pred_set[pred_set!="L50MeanVal"]

data <- auc_summary %>%
  ungroup() %>%
  filter(ind_category != "Location") %>%
  select(AUC, all_of(preds))

subsets <- c(1:length(preds))

rfe_ctrl <- caret::rfeControl(functions = rfFuncs,
                       method = "repeatedcv", 
                       repeats = 5,
                       number = 10,
                       verbose = F)

feature_sel <- rfe(x          = data[,-1], 
                   y          = as.matrix(data[,1]),
                   sizes      = subsets,
                   rfeControl = rfe_ctrl)

save(feature_sel, file = paste0(getwd(), "/Output/Data/RFE/feature_sel_L50.rds"))

#load(paste0(getwd(), "/Output/Data/RFE/feature_sel_L50.rds"))

feature_sel
predictors(feature_sel)
plot(feature_sel, type=c("g", "o"))

# L50 is not an important feature ##############################################
# Filter to L50 mean data and remove L50lvl as a predictor
# Re-run feature selection to see what is now important
preds <- pred_set[pred_set!="L50lvl"]

data <- auc_summary %>%
        ungroup() %>%
        filter(L50lvl == "mean",
               ind_category != "Location") %>%
        select(AUC, all_of(preds))

subsets <- c(1:length(preds))

rfe_ctrl <- caret::rfeControl(functions = rfFuncs,
                              method = "repeatedcv", 
                              repeats = 5,
                              number = 10,
                              verbose = F)

feature_sel2 <- rfe(x = data[,-1], 
                   y = as.matrix(data[,1]),
                   sizes = subsets,
                   rfeControl = rfe_ctrl)

save(feature_sel2, file = paste0(getwd(), "/Output/Data/RFE/feature_sel_meanL50.rds"))
load(paste0(getwd(), "/Output/Data/RFE/feature_sel_meanL50.rds"))

feature_sel2
feature_sel2$call
predictors(feature_sel2)
plot(feature_sel2, type=c("g", "o"))


# Regression Tree ##############################################################
# Fit a regression tree using the selected features as predictors of AUC
features <- predictors(feature_sel2)
#features <- pred_set2[!pred_set2 %in% c("YearLength", "L50lvl")]
formula <- paste("AUC ~ ", paste(features, collapse = " + "))
reg_tree <- rpart(formula, data = data)

printcp(reg_tree)
plotcp(reg_tree)
vip(reg_tree, num_features = 40, bar = FALSE, geom = "col")
rpart.plot(reg_tree, cex = 0.5, type = 5)

save(reg_tree, file = paste0(getwd(), "/Output/Data/RegTree/RegTree.rds"))
load(paste0(getwd(), "/Output/Data/RegTree/RegTree.rds"))

## Pruning #####################################################################
# Prune the regression tree
best <- reg_tree$cptable[which(reg_tree$cptable[,4] == min(reg_tree$cptable[,4])),]
ose  <- reg_tree$cptable[reg_tree$cptable[,4] <= best[4]+best[5],]
cp   <- ose[1]

rpartCtrl <- rpart.control(cp = cp)

pruned_tree <- rpart(formula, 
                     data = data,
                     control = rpartCtrl)

base_tree <- rpart(formula, 
                  data = data,
                  control = rpart.control(cp = reg_tree$cptable[1]))

# Model performance
printcp(pruned_tree) # record root node error 
root_node_error <- 23.52/520

# Model performance on absolute scale 
cbind(pruned_tree$cptable[,1:2],pruned_tree$cptable[,3:5]*root_node_error)

plotcp(pruned_tree)

# Variable Importance
vip(pruned_tree, num_features = 40, bar = FALSE, geom = "col")

# Decision Tree Diagram
rpart.plot(pruned_tree, cex = 0.8, type = 1,
           extra = 101, box.palette= "BuGn")

# Detailed node information
as.data.frame(pruned_tree$frame)

# Rules
rules <- path.rpart(pruned_tree, node = 1:pruned_tree$frame$n)

rule_tbl <- data.frame()

for (Split in 1:length(rules) ){
  spliti <- rules[Split]
  for (Criteria in length(spliti[[1]])) {
    critj <- spliti[[1]][Criteria]
    SplitNo <- Split-1
    CriteriaNo <- Criteria-1
    res <- cbind(SplitNo,CriteriaNo,critj)
    rule_tbl <- rbind(rule_tbl, res)
  }
}

rule_tbl$StockKeyLabel <- ifelse(
  grepl("StockKeyLabel=", rule_tbl$critj),
  sub("StockKeyLabel=", "", rule_tbl$critj),
  NA
)
rule_tbl$SurveyName <- ifelse(
  grepl("SurveyName=", rule_tbl$critj),
  sub("SurveyName=", "", rule_tbl$critj),
  NA
)
rule_tbl$ind_category <- ifelse(
  grepl("ind_category=", rule_tbl$critj),
  sub("ind_category=", "", rule_tbl$critj),
  NA
)
rule_tbl$SpeciesScientificName <- ifelse(
  grepl("SpeciesScientificName=", rule_tbl$critj),
  sub("SpeciesScientificName=", "", rule_tbl$critj),
  NA
)

rule_tbl

rule_tbl_l <- tidyr::pivot_longer(rule_tbl, c(4:7), names_to = "SplitVar", values_to = "SplitCriteria")
rule_tbl_l <- select(rule_tbl_l, -c(critj, CriteriaNo)) %>% na.omit()

save(rule_tbl_l, file = paste0(getwd(), "/Output/Data/RegTree/RegRules.rds"))

asRules(pruned_tree) # note Rule numbers: 5,27,7,53,25,9,24,8,52
end_nodes <- c(5,27,7,53,25,9,24,8,52)

# Save Tree
save(pruned_tree, file = paste0(getwd(), "/Output/Data/RegTree/RegTreePruned.rds"))

## Leaves ######################################################################
# Identify the groupings
# These are manually taken by looking at the regression tree
# Starting from right to left
# These are used to order plots later

preds <- pred_set[pred_set!="L50lvl"]

data <- auc_summary %>%
  ungroup() %>%
  filter(L50lvl == "mean",
         ind_category != "Location") %>%
  select(AUC, all_of(preds))

l9 <- data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         !SpeciesScientificName %in% c("Gadus morhua", "Squalus acanthias", "Merluccius merluccius", "Scomber scombrus", "Lophius piscatorius", "Pleuronectes platessa", "Solea solea", "Merlangius merlangus")) %>%
         select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "I")

l8 <- data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SpeciesScientificName %in% c("Gadus morhua", "Squalus acanthias", "Merluccius merluccius", "Scomber scombrus", "Lophius piscatorius", "Pleuronectes platessa", "Solea solea", "Merlangius merlangus"),
         ind_category != "Dispersion", 
         !SurveyName %in% c("EVHOE", "SWC-IBTS", "SP-NORTH", "SNS")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "H")

l7 <- data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SpeciesScientificName %in% c("Gadus morhua", "Squalus acanthias", "Merluccius merluccius", "Scomber scombrus", "Lophius piscatorius", "Pleuronectes platessa", "Solea solea", "Merlangius merlangus"),
         ind_category != "Dispersion", 
         SurveyName %in% c("EVHOE", "SWC-IBTS", "SP-NORTH", "SNS"),
         !StockKeyLabel %in% c("mac.27.nea", "ple.27.420")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "G")

l6 <- data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SpeciesScientificName %in% c("Gadus morhua", "Squalus acanthias", "Merluccius merluccius", "Scomber scombrus", "Lophius piscatorius", "Pleuronectes platessa", "Solea solea", "Merlangius merlangus"),
         ind_category != "Dispersion", 
         SurveyName %in% c("EVHOE", "SWC-IBTS", "SP-NORTH", "SNS"),
         StockKeyLabel %in% c("mac.27.nea", "ple.27.420")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "F")

l5 <- data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SpeciesScientificName %in% c("Gadus morhua", "Squalus acanthias", "Merluccius merluccius", "Scomber scombrus", "Lophius piscatorius", "Pleuronectes platessa", "Solea solea", "Merlangius merlangus"),
         ind_category == "Dispersion", 
         !StockKeyLabel %in% c("cod.27.22-24", "ple.27.7a", "ple.27.7d", "sol.27.4", "whg.27.6a", "whg.27.7b-ce-k")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "E")

l4 <- data %>%
  filter(!StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SpeciesScientificName %in% c("Gadus morhua", "Squalus acanthias", "Merluccius merluccius", "Scomber scombrus", "Lophius piscatorius", "Pleuronectes platessa", "Solea solea", "Merlangius merlangus"),
         ind_category == "Dispersion", 
         StockKeyLabel %in% c("cod.27.22-24", "ple.27.7a", "ple.27.7d", "sol.27.4", "whg.27.6a", "whg.27.7b-ce-k")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "D")

l3 <- data %>%
  filter(StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                               "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SurveyName != "NS-IBTS") %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "C")

l2 <- data %>%
  filter(StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                              "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SurveyName == "NS-IBTS",
         !StockKeyLabel %in% c("her.27.3a47d", "ple.27.21-23")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "B")
  
l1 <- data %>%
  filter(StockKeyLabel %in% c("had.27.46a20","her.27.3a47d", "hke.27.3a46-8abd", "hom.27.2a4a5b6a7a-ce-k8",
                              "meg.27.7b-k8abd", "ple.27.21-23", "tur.27.4", "whg.27.47d"),
         SurveyName == "NS-IBTS",
         StockKeyLabel %in% c("her.27.3a47d", "ple.27.21-23")) %>%
  select(StockKeyLabel, SpeciesScientificName, SurveyName, Quarter) %>%
  distinct() %>% 
  mutate(LeafNo = "A")

stk_leaves <- rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9)
stk_leaves$LeafNo <- factor(stk_leaves$LeafNo, levels = rev(unique(stk_leaves$LeafNo)))
stk_leaves <- stk_leaves %>%
  group_by(StockKeyLabel, SurveyName, Quarter) %>%
  arrange(LeafNo) %>%
  summarise(LeafNo = paste0(LeafNo, collapse = " & ")) %>%
  arrange(LeafNo) %>%
  print(., n = nrow(.))

save(stk_leaves, file = paste0(getwd(), "/Output/Data/RegTree/stockleaves.rds"))

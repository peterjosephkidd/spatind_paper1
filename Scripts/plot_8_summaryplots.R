#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               8. Summary plots of stocks & surveys
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(icesVocab)
library(readxl)
library(writexl)
library(ggplot2) 
library(cowplot)
library(stringr)

rm(list = ls())

load.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/"
save.path <- "C:/Users/pk02/OneDrive - CEFAS/Projects/C8503B/PhD/SpatIndAssess(GIT)/SpatIndAssess/Data/DR_Stocks/Outputs/SummaryPlots/"

load(paste0(load.path, "Data/DR_Stocks/SurveyData/stks.rds")) # saved stks from data_2_DownloadDATRAS
stksurveys  <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-AllSurveyData-manual.xlsx"), sheet = "Surveys")
stkinfo     <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-69stks-AY2022-stkdescrptn.xlsx"))
ecoregions  <- read_xlsx(paste0(load.path, "Data/DR_Stocks/icesSA_data/icesData-69stks-AY2022-SA-data.xlsx"))


stksurveys_full <- stksurveys %>%
  filter(InDatras == 1) %>%
  select(-c(Ship, Country, YrsExclude, Ages, inRcntStkAnX, inMatCalc, MatYrs, Usage, Notes, `Full Name`)) %>%
  na.omit()

stkinfo_full <- stkinfo %>%
  filter(StockKeyLabel %in% stks)

ecoregions_full <- ecoregions %>%
  select(StockKeyLabel, EcoRegion) %>%
  distinct() %>%
  filter(StockKeyLabel %in% stks) %>%
  tidyr::separate_rows(EcoRegion, sep = ", ") %>%
  mutate(EcoRegion = str_remove(EcoRegion, " Ecoregion")) %>%
  arrange(EcoRegion) 

n.stks <- length(unique(stkinfo_full$StockKeyLabel))
n.srvy <- length(stksurveys_full$SurveyAcronymn)
n.rgns <- length(ecoregions_full$EcoRegion)

# Stocks
stk.table <- stkinfo_full %>%
  mutate(Species = paste0(SpeciesCommonName, " (", SpeciesScientificName, ")"),
         `Stock ID` = StockKeyLabel) %>%
  select(`Stock ID`, Species) %>%
  arrange(`Stock ID`)
n.spcs <- length(unique(stkinfo_full$SpeciesCommonName))

# Fisheries & Size Guilds >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
plot.guild <- ggplot() +
  geom_histogram(data = stkinfo_full, aes(x = SizeGuild), fill = "cyan4", stat = "count", ) +
  coord_flip() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Number of Stocks") +
  xlab("Size Guild") +
  labs(fill = "Fisheries Guild") +
  geom_text(data = data.frame(), aes(x = Inf-1, y = Inf, label = paste0("N = ", n.stks)), hjust = 2, vjust = 3, size = 3.3) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) 

# Surveys >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
plot.srvy <- ggplot() +
  geom_histogram(data = stksurveys_full, aes(x = factor(SurveyAcronymn, level = rev(sort(unique(SurveyAcronymn))))), fill = "cyan4", stat = "count") +
  coord_flip() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Frequency") +
  xlab("Survey Acronymn") +
  guides(fill = "none") +
  geom_text(data = data.frame(), aes(x = Inf-1, y = Inf, label = paste0("N = ", n.srvy)), hjust = 2, vjust = 3, size = 3.3) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) 

# Working Groups >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
plot.wg <- ggplot() +
  geom_histogram(data = stkinfo_full, aes(x = factor(ExpertGroup, level = rev(sort(unique(ExpertGroup))))), fill = "cyan4", stat = "count") +
  coord_flip() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Number of Stocks") +
  xlab("Working Groups") +
  labs(fill = "Fisheries Guild") +
  geom_text(data = data.frame(), aes(x = Inf-1, y = Inf, label = paste0("N = ", n.stks)), hjust = 2, vjust = 3, size = 3.3) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) 

# Ecoregions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
plot.rgns <- ggplot() +
  geom_histogram(data = ecoregions_full, aes(x = factor(EcoRegion, level = rev(sort(unique(EcoRegion))))), fill = "cyan4", stat = "count") +
  coord_flip() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  ylab("Frequency") +
  xlab("Ecoregion") +
  labs(fill = "Fisheries Guild") +
  geom_text(data = data.frame(), aes(x = Inf-1, y = Inf, label = paste0("N = ", n.rgns)), hjust = 2, vjust = 3, size = 3.3) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) 

plot.gwr <- plot_grid(plot.guild, plot.wg, plot.rgns,plot.srvy, labels = c('a.', 'b.', 'c.', 'd.'), nrow = 2)

# ROC matrix example >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
set.seed(360)

y <- c("Positive", "Negative")
x <- c("Negative", "Positve")
dt <- as.data.frame(cbind(y, x))

x <- abs(
  c(
    jitter(seq(1.5,1.95, length.out = 5), factor = 20),
    jitter(seq(0.3,0.5, length.out = 10), factor = 40),
    jitter(seq(0.6,0.8, length.out = 10), factor = 40),
    jitter(seq(0.7,1.4, length.out = 5), factor = 20)
  )
)

y <- abs(
  c(
    jitter(seq(0.9,1.2, length.out = 3), factor = 20),
    jitter(seq(0.1,0.5, length.out = 10), factor = 10),
    jitter(seq(0.6,0.8, length.out = 10), factor = 40),
    jitter(seq(0.7,1.4, length.out = 7), factor = 20)
  )
) 

dt2 <- as.data.frame(cbind(x,y))
dt2$Year <- seq(1990, to = 1990+nrow(dt2)-1)

#Observations
indseries <- ggplot(data = dt2) +
  geom_line(aes(Year,x), colour = "black") +
  geom_hline(yintercept = 1, lty = 2) +
  ylab("Biomass / MSY B trigger") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        aspect.ratio = 1) 
indseries
# Biomass
bioseries <- ggplot(data = dt2) +
  geom_line(aes(Year,y), colour = "black") +
  ylab("Indicator Values") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        #axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        aspect.ratio = 1) 
bioseries

fp <- dt2 %>% filter(y >= 1 & x < 1 ) %>% nrow()
tn <- dt2 %>% filter(y <  1 & x < 1 ) %>% nrow()
tp <- dt2 %>% filter(y >= 1 & x >= 1) %>% nrow()
fn <- dt2 %>% filter(y <  1 & x >= 1) %>% nrow()

ROCex <- ggplot(data = dt2) +
  geom_point(aes(x,y), colour = "darkgrey")+
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(breaks = 1, labels = expression("MSY B"[trigger])) +
  scale_y_continuous(breaks = 1, labels = "Threshold") +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(xintercept = 1) +
  geom_text(x = 0.45, y = 1.55, label = "FP", colour = "red2",   size = 4) +
  geom_text(x = 0.45, y = 1.40, label = fp, size = 3) +
  geom_text(x = 0.45, y = 0.45, label = "TN", colour = "green4", size = 4) +
  geom_text(x = 0.45, y = 0.30, label = tn, size = 3) +
  geom_text(x = 1.55, y = 0.45, label = "FN", colour = "red2",   size = 4) +
  geom_text(x = 1.55, y = 0.30, label = fn, size = 3) +
  geom_text(x = 1.55, y = 1.55, label = "TP", colour = "green4", size = 4) +
  geom_text(x = 1.55, y = 1.40, label = tp, size = 3) +
  
  ylab("Indicator Values (Predictions) \n\nNegative                   Positive") +
  xlab("Negative                   Positive \n\nBiomass Values (Observations)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.ticks.length = unit(.25, "cm"),
        axis.title = element_text(size = 10),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        aspect.ratio = 1) 
ROCex

# PLot as ROC curve
dt2 <- dt2 %>%
  mutate(
    state = ifelse(x >= 1, 1, 0)
  )

pROC::roc(dt2, state, y, plot = T)

# ROC Curve guide >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Perf ####
tpr1 <- c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1)
fpr1 <- c(0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 1)
d <- as.data.frame(cbind(tpr1, fpr1))

rocperf <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", linewidth = 0.6) +
  annotate("text", label = "AUC = 1", x = 0.3, y = 0.75, size = 2.75) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("TPR") + 
  xlab("FPR") 

## Random ####
tpr1 <- seq(0,1, length.out = 9)
fpr1 <- tpr1
d <- as.data.frame(cbind(tpr1, fpr1))

rocrand <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", linewidth = 0.6) +
  annotate("text", label = "AUC = 0.5", x = 0.3, y = 0.75, size = 2.75) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("FPR") 

## Imperf #### 
fpr1 <- c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1, 1)
tpr1 <- c(0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 1)

d <- as.data.frame(cbind(tpr1, fpr1))

rocimp <- ggplot() +
  geom_point(data = d, aes(x = fpr1, y = tpr1), colour = "blue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(data = d, aes(x = fpr1, y = tpr1), colour = "blue", linewidth = 0.6) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  annotate("text", label = "AUC = 0", x = 0.3, y = 0.75, size = 2.75) +
  theme(panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        # Axis
        axis.text = element_text(size = 8),
        #axis.text.x = element_text(angle = 90, vjust = 0.3), # rotate & shift right
        axis.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylab("") + 
  xlab("FPR") 

exampleroc <- plot_grid(rocperf, rocrand, rocimp, nrow = 1, 
                        labels = c("(a)", "(b)", "(c)"), 
                        #vjust = 10,
                        label_size = 10)

# Save >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ggsave(paste0(save.path, "guilds.png"),       plot.guild, width = 7, height = 5)
ggsave(paste0(save.path, "surveys.png"),      plot.srvy,  width = 7, height = 5)
ggsave(paste0(save.path, "expertgroups.png"), plot.wg,    width = 7, height = 5)
ggsave(paste0(save.path, "ecoregions.png"),   plot.rgns,  width = 7, height = 5)
ggsave(paste0(save.path, "ecoregions.png"),   plot.rgns,  width = 7, height = 5)
ggsave(paste0(save.path, "GldWkGrgns.png"),   plot.gwr,   width = 10, height = 7)
ggsave(paste0(save.path, "ROCmatrix.png"),    ROCex,      width = 4, height = 3)
ggsave(paste0(save.path, "indicator_series.png"),    indseries,      width = 4, height = 3)
ggsave(paste0(save.path, "biomass_series.png"),    bioseries,      width = 4, height = 3)
ggsave(paste0(save.path, "ROCurveGuide.png"), exampleroc, width = 10, height = 3)


write_xlsx(stk.table, path = paste0(save.path, "stocks.xlsx"))


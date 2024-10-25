#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>               8. Summary plots of spatial indicators
#> 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)
library(readxl)
library(writexl)
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

# Stocks
load(file = paste0(load.path, "Data/DR_Stocks/Outputs/ROC/expected-surveys.rds"))
stks <- unique(srvys$StockKeyLabel)
rm(srvys)

# Manipulate Stock and Survey Info
stksurveys_full <- stksurveys %>%
  filter(InDatras == 1) %>%
  select(-c(Ship, Country, YrsExclude, Ages, inRcntStkAnX, inMatCalc, MatYrs, Usage, Notes, `Full Name`)) %>%
  na.omit()

stksurveys_full <- left_join(stksurveys_full, ssb.vs.tb[c("StockKeyLabel", "Type")], by = "StockKeyLabel")

# Select stock, survey, and L50 level
i <- 20 # stock
j <- 1 # survey
lvl <- 1 # L50 lvl, 1=mean
ind <- 1 # survey index

stk <- stks[i]
if (stk == "lez.27.4a6a") {
  message(paste0("\n", stk, " is a combined stock of two species of megrim"))
  species_aphia <- c(icesVocab::findAphia("Lepidorhombus boscii", latin = T), findAphia("Lepidorhombus whiffiagonis", latin = T))
  message(paste0(length(species_aphia), " Valid_Aphia codes successfully retrieved\n"))
} else {
  species_aphia <- icesVocab::findAphia(unique(stksurveys_full[stksurveys_full$StockKeyLabel == stk,]$SpeciesScientificName), latin = TRUE)
}

L50levels <- c("mean", "lowerCI", "upperCI")

srvys <- stksurveys_full[stksurveys_full$StockKeyLabel == stk,]
srvys.list <- unique(srvys$SurveyAcronymn)
srv <- srvys.list[j]

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

message(paste0(i, ".", j, ": ", stk, ", ", srv))

# Load Survey Data
files <- list.files(paste0(load.path, "Data/DR_Stocks/SurveyData/", stk, "/matures/"), pattern = paste0("^", srv, "\\.Yr.*L50\\.", L50levels[lvl]), full.names = T)
do.call(list, lapply(files, load, envir = .GlobalEnv))
indices <- srvys[srvys$SurveyAcronymn == srv,]

srvindx <- indices[ind,]
index <- srvindx$SurveyIndex

message(paste0(i, ".", j, ".", ind, ": ", stk, ", ", srv, ", ", index, " (maturity = ", mtr, ", L50 = ", L50levels[lvl], ")"))

yrs <- srvindx$YearStart:srvindx$YearEnd
qrs <- srvindx$Quarter
stk_divs <- unlist(strsplit(srvindx$Divisions, ", "))
message(paste0(min(yrs), "-", max(yrs), " Q", qrs, " Divs: ", stk_divs))

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
nrow(check) > 0

# Range and Occupancy Indicators
yr <- 2007
occ_ran_plot <- mapdis(hlhh, yr, qrs, species_aphia, stk_divs, ices_rect, matures = TRUE, # data specifics
                   cog = F, inertia = F, EOO = T, ELA = T, # spatial indicators
                   density = T,                           # weight cog and inertia
                   km2lonlat = F,                                  # convert km to lonlat
                   title = "",                                     # plot title
                   xlim = c(-11, -2), ylim = c(51, 61)) +
  theme_classic() +
  labs(title = "", subtitle = "") +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black")
  )

ggsave(filename = paste0(save.path, "Outputs/SummaryPlots/POP-EOO-ELA.png"), occ_ran_plot, width = 7, height = 7)

# Gini index and D95
lorenz <- lorenz_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
gni <- Gini(lorenz, matures = mtr)
D95 <- d95(lorenz)

lorplot <- ggplot(data = lorenz[lorenz$Year %in% c(yr),], aes(x = rect_num_prop, y = cumsum_prop)) + 
  geom_area(aes(group = Year), fill = "cyan4", alpha = 0.5) +
  geom_line(aes(group = Year), colour = "black") +
  coord_cartesian(ylim= c(-0.02,1.02), xlim = c(0,1.02), expand = FALSE) +
  labs(x = "Culmuative Proportion of ICES Rectangles", 
       y = "Culmuative Proportion of Density") +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))+
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none") +
  annotate("segment", x = 0.95, y = 0, yend = 1, xend = 0.95, colour = "red2", size = 0.3) +
  annotate("label", x = 0.75, y = 0.375, label = "A", size = 3) +
  annotate("label", x = 0.875, y = 0.125, label = "B", size = 3) + 
  annotate("text", x = 0.2, y = 0.9, label = paste0("1-Gini = ", round(gni[gni$Year == yr,]$`Gini Index`,2)), size = 3) +
  annotate("text", x = 0.2, y = 0.8, label = paste0("D95 = ", round(D95[D95$Year == yr,]$D95,2)), size = 3) +
  annotate("rect", xmin = 0, ymin = 0, ymax = 0, xmax = 1, colour = "black") +
  annotate("rect", xmin = 1, ymin = 0, ymax = 1, xmax = 1, colour = "black") +
  annotate("segment", x = 0, y = 0, yend = 1, xend = 1, colour = "black", linetype = 2, size = 0.3) +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    #panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black")
  ) +
  labs(x = "T", y = "Q(T)/Q")
lorplot
ggsave(filename = paste0(save.path, "Outputs/SummaryPlots/lorenz_curve.png"), lorplot, width = 3.5, height = 3)


# Spreading Area
sa_data <- spreadingarea_data(hlhh, yrs, qrs, species_aphia, stk_divs, matures = mtr)
dev.new()
spreadingarea_calc(sa_data[sa_data$Year == yr,]$TotalNoMature_Dur, plot = T)

z <- lorenz[lorenz$Year == yr,]$TotalNoMature_Dur

z <- sa_data[sa_data$Year == yr,]$TotalNoMature_Dur

z <- rep(1, length(z))
# extract data
nb <-length(z)

# sort data in increasing order
#zi <- sort(z,index.return=T)
z <- sort(z)
w <- NA
if(is.na(w)){
  w <- rep(1, length(z))
} else{
  w <- w[order(sort(z))]
}

# computation of the spreading area 
Q <- sum(z*w)
QT <- c(0,cumsum(z*w))
QT_Q <- QT/Q
SA <- sum((QT[1:nb]+QT[2:(nb+1)])*w)/Q

# computation of (Q-Q(T))/Q as a function of T
fT <- c(0,cumsum(w))
fT <- fT[nb+1] - fT
fT <- rev(fT)
Tprop <- fT/max(fT)
QT <- QT[nb+1] - QT
QT <- rev(QT)

df <- as.data.frame(cbind(Year = yr, QT_Q, Tprop, fT))

# display
plot(fT, (QT)/Q, main="Curve (Q-Q(T))/Q", type="o", pch="+")
plot(fT, QT_Q, main="Curve (Q(T))/Q", type="o", pch="+")
plot(Tprop, QT_Q, main="Curve (Q(T))/Q", type="o", pch="+")

sa_plot <- ggplot(data = df, aes(x = fT, y = QT_Q)) + 
  geom_area(aes(group = Year), fill = "cyan4", alpha = 0.5) +
  geom_line(aes(group = Year), colour = "black") +
  coord_cartesian(ylim= c(-0.02,1.02), xlim = c(0,max(df$fT)+max(df$fT)*0.01), expand = FALSE) +
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8)) +
  scale_colour_gradientn(colours = rainbow(3), name = "Year") +
  guides(alpha = "none") +
  annotate("text", x = 15, y = 0.9, label = paste0("SA = ", round(SA,2)), size = 3) + 
  annotate("label", x = 58, y = 0.1, label = "B", size = 3) + 
  annotate("rect", xmin = 0, ymin = 0, ymax = 0, xmax = max(df$fT), colour = "black") +
  annotate("rect", xmin = max(df$fT), ymin = 0, ymax = 1, xmax = max(df$fT), colour = "black") +
  annotate("segment", x = 0, y = 0, yend = 1, xend = max(df$fT), colour = "black", linetype = 2, size = 0.3) +
  theme(
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    #panel.border       = element_rect(colour = "black", fill = NA),
    strip.background   = element_rect(colour = "black")
  ) +
  labs(x = "T", y = "Q(T)/Q")

ggsave(filename = paste0(save.path, "Outputs/SummaryPlots/sa_curve.png"), sa_plot, width = 3.5, height = 3)

joint_plot <- cowplot::plot_grid(lorplot, sa_plot, labels = c("a.", "b."), label_size = 10)
ggsave(filename = paste0(save.path, "Outputs/SummaryPlots/lorenz-sa-curves.png"), joint_plot, width = 7, height = 3)

spreadingarea_calc(sa_data[sa_data$Year == yr,]$TotalNoMature_Dur, plot = F)
equivalentarea(sa_data[sa_data$Year == yr,]$TotalNoMature_Dur)

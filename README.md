
# README

- **Data/**: Contains initial data needed to replicate analysis in
  cluding stock information, survey data, ICES Divisions
- **Functions/**: Contains user defined functions for calculating
  spatial indicators, ROC curves, and communciating results
- **Scripts/**: R scripts to run analysis
  - `data_1_SelectStocks.R`: Retrieve stock info and assessment outputs
    for data-rich stocks. (do not run)
  - `data_2_DownloadDATRAS.R`: Download DATRAS survey data. (do not run)
  - `data_3_CleanExchange.R`: Clean DATRAS survey data.
  - `data_3a_Mature.R`: Subset survey data to matures based on L50.
  - `model_4_SpatIndCalc.R`: Calculate spatial indicators using survey
    data.
  - `model_5_ROC.R`: Assess classifcation skill of spatial indicators
    using ROC curves.
  - `model_6_regression_tree.R`: Identify predictors of classifcation
    skill using RFE and regression trees.
  - `output_7_TabsAndPlots.R`: Plot primary and supplementary tables and
    plots.

Scripts 1 and 2 do not need to be run. They retrieve data rich stock
information (e.g. reference points and SSB) and download DATRAS survey
data which could change if ICES make retrospective corrections to data.
Outputs of these scripts have been provided in `Data/Initial/` so that
results can be accurately replicated.

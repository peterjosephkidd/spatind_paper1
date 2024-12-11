
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

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-rmarkdown2024" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2024. *<span
class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-datatable" class="csl-entry">

Barrett, Tyson, Matt Dowle, Arun Srinivasan, Jan Gorecki, Michael
Chirico, and Toby Hocking. 2024. *<span
class="nocase">data.table</span>: Extension of “<span
class="nocase">data.frame</span>”*.
<https://CRAN.R-project.org/package=data.table>.

</div>

<div id="ref-sp2013" class="csl-entry">

Bivand, Roger S., Edzer Pebesma, and Virgilio Gomez-Rubio. 2013.
*Applied Spatial Data Analysis with R, Second Edition*. Springer, NY.
<https://asdar-book.org/>.

</div>

<div id="ref-rgeos" class="csl-entry">

Bivand, Roger, and Colin Rundel. 2023. *<span
class="nocase">rgeos</span>: Interface to Geometry Engine - Open Source
(“GEOS”)*. <https://CRAN.R-project.org/package=rgeos>.

</div>

<div id="ref-rfishbase" class="csl-entry">

Boettiger, Carl, Duncan Temple Lang, and Peter Wainwright. 2012. “<span
class="nocase">rfishbase</span>: Exploring, Manipulating and Visualizing
FishBase Data from r.” *Journal of Fish Biology*, November.
<https://doi.org/10.1111/j.1095-8649.2012.03464.x>.

</div>

<div id="ref-ggnewscale" class="csl-entry">

Campitelli, Elio. 2024. *<span class="nocase">ggnewscale</span>:
Multiple Fill and Colour Scales in “<span
class="nocase">ggplot2</span>”*.
<https://CRAN.R-project.org/package=ggnewscale>.

</div>

<div id="ref-Chang_2019" class="csl-entry">

Chang, Jonathan, Daniel L Rabosky, Stephen A Smith, and Michael E
Alfaro. 2019. “An r Package and Online Resource for Macroevolutionary
Studies Using the Ray-Finned Fish Tree of Life.” *Methods in Ecology and
Evolution* 10 (7): 1118–24. <https://doi.org/10.1111/2041-210x.13182>.

</div>

<div id="ref-sfheaders" class="csl-entry">

Cooley, David. 2024. *<span class="nocase">sfheaders</span>: Converts
Between r Objects and Simple Feature Objects*.
<https://CRAN.R-project.org/package=sfheaders>.

</div>

<div id="ref-remotes" class="csl-entry">

Csárdi, Gábor, Jim Hester, Hadley Wickham, Winston Chang, Martin Morgan,
and Dan Tenenbaum. 2024. *<span class="nocase">remotes</span>: R Package
Installation from Remote Repositories, Including “GitHub”*.
<https://CRAN.R-project.org/package=remotes>.

</div>

<div id="ref-janitor" class="csl-entry">

Firke, Sam. 2023. *<span class="nocase">janitor</span>: Simple Tools for
Examining and Cleaning Dirty Data*.
<https://CRAN.R-project.org/package=janitor>.

</div>

<div id="ref-car" class="csl-entry">

Fox, John, and Sanford Weisberg. 2019. *An R Companion to Applied
Regression*. Third. Thousand Oaks CA: Sage.
<https://socialsciences.mcmaster.ca/jfox/Books/Companion/>.

</div>

<div id="ref-vip" class="csl-entry">

Greenwell, Brandon M., and Bradley C. Boehmke. 2020. “Variable
Importance Plots—an Introduction to the Vip Package.” *The R Journal* 12
(1): 343–66. <https://doi.org/10.32614/RJ-2020-013>.

</div>

<div id="ref-party2006c" class="csl-entry">

Hothorn, Torsten, Peter Buehlmann, Sandrine Dudoit, Annette Molinaro,
and Mark Van Der Laan. 2006. “Survival Ensembles.” *Biostatistics* 7
(3): 355–73.

</div>

<div id="ref-party2006a" class="csl-entry">

Hothorn, Torsten, Kurt Hornik, and Achim Zeileis. 2006a. “Unbiased
Recursive Partitioning: A Conditional Inference Framework.” *Journal of
Computational and Graphical Statistics* 15 (3): 651–74.
<https://doi.org/10.1198/106186006X133933>.

</div>

<div id="ref-partykit2006" class="csl-entry">

———. 2006b. “Unbiased Recursive Partitioning: A Conditional Inference
Framework.” *Journal of Computational and Graphical Statistics* 15 (3):
651–74. <https://doi.org/10.1198/106186006X133933>.

</div>

<div id="ref-partykit2015" class="csl-entry">

Hothorn, Torsten, and Achim Zeileis. 2015. “<span
class="nocase">partykit</span>: A Modular Toolkit for Recursive
Partytioning in R.” *Journal of Machine Learning Research* 16: 3905–9.
<https://jmlr.org/papers/v16/hothorn15a.html>.

</div>

<div id="ref-paletteer" class="csl-entry">

Hvitfeldt, Emil. 2021. *<span class="nocase">paletteer</span>:
Comprehensive Collection of Color Palettes*.
<https://github.com/EmilHvitfeldt/paletteer>.

</div>

<div id="ref-plotrix" class="csl-entry">

J, Lemon. 2006. “Plotrix: A Package in the Red Light District of r.”
*R-News* 6 (4): 8–12.

</div>

<div id="ref-SpatialEpi" class="csl-entry">

Kim, Albert Y., Jon Wakefield, and Mikael Moise. 2023. *SpatialEpi:
Methods and Data for Spatial Epidemiology*.
<https://CRAN.R-project.org/package=SpatialEpi>.

</div>

<div id="ref-caret" class="csl-entry">

Kuhn, and Max. 2008. “Building Predictive Models in r Using the Caret
Package.” *Journal of Statistical Software* 28 (5): 1–26.
<https://doi.org/10.18637/jss.v028.i05>.

</div>

<div id="ref-randomForest" class="csl-entry">

Liaw, Andy, and Matthew Wiener. 2002. “Classification and Regression by
randomForest.” *R News* 2 (3): 18–22.
<https://CRAN.R-project.org/doc/Rnews/>.

</div>

<div id="ref-rpartplot" class="csl-entry">

Milborrow, Stephen. 2024. *<span class="nocase">rpart.plot</span>: Plot
“<span class="nocase">rpart</span>” Models: An Enhanced Version of
“<span class="nocase">plot.rpart</span>”*.
<https://CRAN.R-project.org/package=rpart.plot>.

</div>

<div id="ref-icesDatras" class="csl-entry">

Millar, Colin, Cecilia Kvaavik, Adriana Villamor, Scott Large, and Arni
Magnusson. 2023. *<span class="nocase">icesDatras</span>: DATRAS Trawl
Survey Database Web Services*.
<https://CRAN.R-project.org/package=icesDatras>.

</div>

<div id="ref-icesSD" class="csl-entry">

Millar, Colin, Scott Large, and Arni Magnusson. 2022. *<span
class="nocase">icesSD</span>: Stock Database Web Services*.
<https://CRAN.R-project.org/package=icesSD>.

</div>

<div id="ref-icesSAG" class="csl-entry">

Millar, Colin, Scott Large, Arni Magnusson, and Carlos Pinto. 2023.
*<span class="nocase">icesSAG</span>: Stock Assessment Graphs Database
Web Services*. <https://CRAN.R-project.org/package=icesSAG>.

</div>

<div id="ref-icesVocab" class="csl-entry">

Millar, Colin, and Arni Magnusson. 2022. *<span
class="nocase">icesVocab</span>: ICES Vocabularies Database Web
Services*. <https://CRAN.R-project.org/package=icesVocab>.

</div>

<div id="ref-writexl" class="csl-entry">

Ooms, Jeroen. 2024. *<span class="nocase">writexl</span>: Export Data
Frames to Excel “<span class="nocase">xlsx</span>” Format*.
<https://CRAN.R-project.org/package=writexl>.

</div>

<div id="ref-sf2018" class="csl-entry">

Pebesma, Edzer. 2018. “<span class="nocase">Simple Features for R:
Standardized Support for Spatial Vector Data</span>.” *The R Journal* 10
(1): 439–46. <https://doi.org/10.32614/RJ-2018-009>.

</div>

<div id="ref-sp2005" class="csl-entry">

Pebesma, Edzer J., and Roger Bivand. 2005. “Classes and Methods for
Spatial Data in R.” *R News* 5 (2): 9–13.
<https://CRAN.R-project.org/doc/Rnews/>.

</div>

<div id="ref-sf2023" class="csl-entry">

Pebesma, Edzer, and Roger Bivand. 2023. *<span class="nocase">Spatial
Data Science: With applications in R</span>*. Chapman and Hall/CRC.
<https://doi.org/10.1201/9780429459016>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2022. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Rabosky_2018" class="csl-entry">

Rabosky, Daniel L, Jonathan Chang, Pascal O Title, Peter F Cowman,
Lauren Sallan, Matt Friedman, Kristin Kaschner, et al. 2018. “An Inverse
Latitudinal Gradient in Speciation Rate for Marine Fishes.” *Nature* 559
(7714): 392–95. <https://doi.org/10.1038/s41586-018-0273-1>.

</div>

<div id="ref-maps" class="csl-entry">

Richard A. Becker, Original S code by, Allan R. Wilks. R version by Ray
Brownrigg. Enhancements by Thomas P Minka, and Alex Deckmyn. 2023.
*<span class="nocase">maps</span>: Draw Geographical Maps*.
<https://CRAN.R-project.org/package=maps>.

</div>

<div id="ref-pROC" class="csl-entry">

Robin, Xavier, Natacha Turck, Alexandre Hainard, Natalia Tiberti,
Frédérique Lisacek, Jean-Charles Sanchez, and Markus Müller. 2011.
“<span class="nocase">pROC</span>: An Open-Source Package for r and s+
to Analyze and Compare ROC Curves.” *BMC Bioinformatics* 12: 77.

</div>

<div id="ref-party2008e" class="csl-entry">

Strobl, Carolin, Anne-Laure Boulesteix, Thomas Kneib, Thomas Augustin,
and Achim Zeileis. 2008. “Conditional Variable Importance for Random
Forests.” *BMC Bioinformatics* 9 (307).
<https://doi.org/10.1186/1471-2105-9-307>.

</div>

<div id="ref-party2007d" class="csl-entry">

Strobl, Carolin, Anne-Laure Boulesteix, Achim Zeileis, and Torsten
Hothorn. 2007. “Bias in Random Forest Variable Importance Measures:
Illustrations, Sources and a Solution.” *BMC Bioinformatics* 8 (25).
<https://doi.org/10.1186/1471-2105-8-25>.

</div>

<div id="ref-rpart" class="csl-entry">

Therneau, Terry, and Beth Atkinson. 2023. *<span
class="nocase">rpart</span>: Recursive Partitioning and Regression
Trees*. <https://CRAN.R-project.org/package=rpart>.

</div>

<div id="ref-FishLife" class="csl-entry">

Thorson, James. 2023. *FishLife: Predict Life History Parameters for Any
Fish*.

</div>

<div id="ref-MASS" class="csl-entry">

Venables, W. N., and B. D. Ripley. 2002. *Modern Applied Statistics with
s*. Fourth. New York: Springer. <https://www.stats.ox.ac.uk/pub/MASS4/>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-scales" class="csl-entry">

Wickham, Hadley, Thomas Lin Pedersen, and Dana Seidel. 2023. *<span
class="nocase">scales</span>: Scale Functions for Visualization*.
<https://CRAN.R-project.org/package=scales>.

</div>

<div id="ref-cowplot" class="csl-entry">

Wilke, Claus O. 2024. *<span class="nocase">cowplot</span>: Streamlined
Plot Theme and Plot Annotations for “<span
class="nocase">ggplot2</span>”*.
<https://CRAN.R-project.org/package=cowplot>.

</div>

<div id="ref-rattle" class="csl-entry">

Williams, Graham J. 2011. *Data Mining with Rattle and R: The Art of
Excavating Data for Knowledge Discovery*. Use r! Springer.
<https://rd.springer.com/book/10.1007/978-1-4419-9890-3>.

</div>

<div id="ref-SPMpriors" class="csl-entry">

Winker, Henning. 2024. *SPMpriors: SPM Prior Generation with FishLife*.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

<div id="ref-ineq" class="csl-entry">

Zeileis, Achim. 2014. *<span class="nocase">ineq</span>: Measuring
Inequality, Concentration, and Poverty*.
<https://CRAN.R-project.org/package=ineq>.

</div>

<div id="ref-zoo" class="csl-entry">

Zeileis, Achim, and Gabor Grothendieck. 2005. “<span
class="nocase">zoo</span>: S3 Infrastructure for Regular and Irregular
Time Series.” *Journal of Statistical Software* 14 (6): 1–27.
<https://doi.org/10.18637/jss.v014.i06>.

</div>

<div id="ref-party2008b" class="csl-entry">

Zeileis, Achim, Torsten Hothorn, and Kurt Hornik. 2008a. “Model-Based
Recursive Partitioning.” *Journal of Computational and Graphical
Statistics* 17 (2): 492–514. <https://doi.org/10.1198/106186008X319331>.

</div>

<div id="ref-partykit2008" class="csl-entry">

———. 2008b. “Model-Based Recursive Partitioning.” *Journal of
Computational and Graphical Statistics* 17 (2): 492–514.
<https://doi.org/10.1198/106186008X319331>.

</div>

</div>

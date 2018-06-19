# PerFitShiny
This repository contains a [shiny](http://shiny.rstudio.com/) application that allows performing a person-fit analysis of results from exams, categorized as correct responses (codified as 1s) and incorrect ones (codified as 0s).

The app requires the following packages:
* [shiny](http://cran.r-project.org/package=shiny) (version >= 1.0.5)
* [lattice](http://cran.r-project.org/package=lattice) (version >= 0.20-35)
* [splines](http://cran.r-project.org/) (available within base R)
* [corrplot](http://cran.r-project.org/package=corrplot) (version >= 0.84)

These packages can be installed using the following function call:
```r
install.packages(c("shiny", "shinytheme"), dependencies = TRUE)
```
and then the app can be directly invoked using the command:

In order to run the app the easiest way would be:
```R
shiny::runGitHub("PerFitShiny", "albamrt")
```

# PerFitShiny
This repository contains a [shiny](http://shiny.rstudio.com/) application that allows performing a person-fit analysis of results from exams, categorized as correct responses (codified as 1s) and incorrect ones (codified as 0s).

The app requires [R](http://cran.r-project.org/) and the following packages:
* [shiny](http://cran.r-project.org/package=shiny) 
* [shinythemes](https://cran.r-project.org/package=shinythemes)
* [PerFit](https://cran.r-project.org/package=PerFit)
* [plotly](https://cran.r-project.org/package=plotly)
* [ltm](https://cran.r-project.org/package=ltm)
* [DT](https://cran.r-project.org/package=DT)
* [mokken](https://cran.r-project.org/package=mokken)


These packages can be installed using the following function call:
```r
install.packages(c("shiny", "shinythemes", "PerFit", "plotly", "ltm", "DT", "mokken"), 
                 dependencies = TRUE)
```
and then the app can be directly invoked using the command:
```R
shiny::runGitHub("PerFitShiny", "albamrt")
```
The app will automatically load these packages. If you would like to interactively run the code in your own R session, then you 
will need first to load the packages using the commands:
```r
library("shiny")
library("shinythemes")
library("PerFit")
library("plotly")
library("ltm")
library("DT")
```

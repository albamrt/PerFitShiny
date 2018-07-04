library("plotly")
library("shinythemes")

fluidPage(
  theme = shinytheme("yeti"),
  tags$style(mycss),
  tags$head(tags$style(".progress-bar{background-color:yellowgreen;}")),
  navbarPage("PerFitShiny", 
             tabPanel("About",
                      code("PerFitShiny"), 
                      "provides the tools to perform a person-fit analysis of educational tests, in order to detect aberrant item score patterns. It includes:", 
                      br(),
                      br(),
                      tags$ul(
                        tags$li("Charging data  on",tags$b("Data"),"page"), 
                        tags$li("Exploration of scores and items on",tags$b("Summary"),"page"), 
                        tags$li("Checking of the assumptions made for some of the indexs and the goodness of fit for the parametric ones in ", tags$b("Assumptions & Goodness of fit"), "page"),
                        tags$li("Person-Fit analysis on" , tags$b("Analysis"), "page") 
                      )
             ),
             tabPanel("Data",
                      sidebarLayout(
                        sidebarPanel(
                          div(style="display: inline-block;vertical-align:top; width: 150px; height: 27px;","Choose CSV File"),
                          div(style="display: inline-block;vertical-align:top; width: 150px; height: 27px;",actionButton("warning", label = "",  icon = icon("info", lib = "font-awesome"), width = '20%', style='padding: 4px; font-size:80%')),
                          uiOutput("Warning"),
                          fileInput("file", NULL,
                                    multiple = TRUE, accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")),
                          checkboxInput("header", "First row as column names", TRUE),
                          fluidRow(
                            column(6,
                          radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";",
                                                                       Tab = "\t"), selected = ";"),
                          radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"',
                                                                     "Single Quote" = "'"), 
                                       selected = '"')),
                          column(5,
                          radioButtons("na", "NA", choices = c("NA" = NA, "empty" = ""),
                                       selected = NA),
                          radioButtons("disp", "Display", choices = c(Head = "head", All = "all"),
                                       selected = "head")))
                        ),
                        
                        mainPanel(
                          h4("Preview:"),
                          tableOutput("contents")
                        )
                      )
             ),
             tabPanel("Summary",
                      div(style = "display: inline-block;", h4("Sample:")),
                      div(style = "display: inline-block;", h5(textOutput("sample", inline = TRUE), style="color:gray")),
                      
                      fluidRow(
                        column(6,
                               h4("Proportions of correct answers per item:"),
                               plotlyOutput("perc"),
                               br(),
                               sliderInput("xlabsize", "Size of the x axis labels:",
                                    min = 1, max = 20, value = 12, width = '30%')),
                        column(6,
                               h4("Frequencies of all possible total scores of the test:"),
                               plotlyOutput("items")))
                      ),
             navbarMenu("Assumptions & goodness of fit",  
                        tabPanel("Local independence & goodness of fit",
                                 h3("MODFIT - adjusted ratios:"),
                                 fluidRow(
                                   column(5,
                                          wellPanel(radioButtons("IRT.PModel3", HTML("IRT Parameter Model:"),
                                                       c("1PL" = "1PL",
                                                         "2PL" = "2PL",
                                                         "3PL" = "3PL"), selected = "2PL", inline = T)
                                                    ,style = 'width: 220px'),
                                          h4("Summary"),
                                          tableOutput("modfitSummary")
                                          ),
                                   column(7, 
                                          tabsetPanel(type = "tabs",
                                                      tabPanel("Singlets", DT::dataTableOutput("casesItemsSinglets", width = '30%')),
                                                      tabPanel("Doublets", DT::dataTableOutput("casesItemsDoublets", width = '30%')),
                                                      tabPanel("Triplets", DT::dataTableOutput("casesItemsTriplets", width = '30%'))
                                          )),
                                 absolutePanel(top = NULL, left = 0, right = 0, bottom = 0,
                                               width = NULL, height = NULL, draggable = TRUE, fixed = TRUE,
                                               cursor = "move",
                                               div(
                                                 style="border-bottom: 1px solid #CCC; background: #ffffff; text-align:center;",
                                               helpText("For more information (from a similar function) check",
                                                    a(href = "https://www.rdocumentation.org/packages/GGUM/versions/0.3.1/topics/MODFIT", target = "_blank", "MODFIT"),".")
                                           )))),
                        tabPanel("Unidimensionality",
                                 h3("Unidimensionality Check using Modified Parallel Analysis"),
                                 htmlOutput("printUN"),
                                 h4("Eigenvalues of the observed data:"),
                                 plotlyOutput("vaps"),
                                 h4("Simulated values for the second eigenvalue:"),
                                 plotlyOutput("boot2vap"),
                                 absolutePanel(top = NULL, left = 0, right = 0, bottom = 0,
                                               width = NULL, height = NULL, draggable = TRUE, fixed = TRUE,
                                               cursor = "move",
                                               div(
                                                 style="border-bottom: 1px solid #CCC; background: #ffffff; text-align:center;",
                                               helpText("For more information about the method check",
                                          a(href = "https://www.rdocumentation.org/packages/ltm/versions/1.1-0/topics/unidimTest", 
                                            target = "_blank", "Unidimensionality Check using Modified Parallel Analysis"),".")
                        ))),
                        tabPanel("Monotonicity",
                                 h3("Non-decreasing Monotonicity Check"),
                                 br(),
                                 conditionalPanel(
                                   condition = "output.nonMonIndexsCondition",
                                   "There are no items violating the assumption."
                                 ),
                                 conditionalPanel(
                                   condition = "!output.nonMonIndexsCondition",{
                                 fluidRow(
                                   column(3,
                                          h5("Items violating the assumption:"),
                                          tableOutput("summaryNonMonIndexs")),
                                   column(9,
                                            uiOutput("si"),
                                            plotlyOutput("monotonicityPlot"))
                                          )}),
                                 absolutePanel(top = NULL, left = 0, right = 0, bottom = 0,
                                               width = NULL, height = NULL, draggable = TRUE, fixed = TRUE,
                                               cursor = "move",
                                               div(
                                                 style="border-bottom: 1px solid #CCC; background: #ffffff; text-align:center;",
                                                 helpText("For more information about the method see",
                                          a(href = "https://www.rdocumentation.org/packages/mokken/versions/2.8.10/topics/check.monotonicity", 
                                            target = "_blank", "Check Of Monotonicity"),".")
                                 )))
             ),
             navbarMenu("Analysis",
                        tabPanel("One index",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     radioButtons("index", "Index",
                                                  c("C" = "C",
                                                    "C*" = "C*",
                                                    "U3" = "U3",
                                                    "Ht" = "Ht",
                                                    "lz" = "lz"), selected = "C", inline=T),
                                     br(),
                                     
                                     radioButtons("IRT.PModel", HTML("IRT Parameter Model: <br />(only for parametric indices)"),
                                                  c("1PL" = "1PL",
                                                    "2PL" = "2PL",
                                                    "3PL" = "3PL"), selected = "2PL", inline = T),
                                     br(),
                                     radioButtons("cutoff.chosen", label = "Cutoff value:",
                                                  choices = list("Median" = "value", "Most Conservative Bound of the Confidence Interval" = "conservativeIC"), 
                                                  selected = "value"),
                                     br()
                                     
                                     , width = 3),
                                   
                                   mainPanel(
                                     splitLayout(cellWidths = c("30%", "70%"), 
                                                 DT::dataTableOutput("index.tab"), 
                                                 plotlyOutput("index.plot")),
                                     absolutePanel(top = NULL, left = 0, right = 0, bottom = 0,
                                                   width = NULL, height = NULL, draggable = TRUE, fixed = TRUE,
                                                   cursor = "move",
                                                   div(
                                                     style="border-bottom: 1px solid #CCC; background: #ffffff; text-align:center;",
                                                   helpText("For more information about the indices and the methods check",
                                                            a(href = "https://CRAN.R-project.org/package=PerFit", target = "_blank", "PerFit: Person Fit"),".")
                                                   ))
                                     ))),
                        
                        tabPanel("Several indices",
                                 sidebarLayout(
                                   sidebarPanel(
                                     checkboxGroupInput("indexs", "Indices:",
                                                        c("C" = "C.Sato",
                                                          "C*" = "Cstar",
                                                          "U3" = "U3",
                                                          "Ht" = "Ht",
                                                          "lz" = "lz"), 
                                                        selected = c("C.Sato", "Ht"),
                                                        inline = T),
                                     br(),
                                     
                                     radioButtons("IRT.PModel2", HTML("IRT Parameter Model: <br />(only for parametric indices)"),
                                                  c("1PL" = "1PL",
                                                    "2PL" = "2PL",
                                                    "3PL" = "3PL"), selected = "1PL", inline = T),
                                     br(),
                                     radioButtons("cutoff.chosen2", label = "Cutoff value:",
                                                  choices = list("Median" = "value", "Most Conservative Bound of the Confidence Interval" = "conservativeIC"), 
                                                  selected = "value")
                                     , width = 3),
                                   mainPanel(
                                     h4("Flagged cases:"),
                                     DT::dataTableOutput("allflagged.tab"),
                                     h4("Profiles:"),
                                     tableOutput("profiles.table")
                                   ))
                        ),
                        tabPanel("Diagnostic",
                                 fluidRow(
                                   column(4,
                                          wellPanel(
                                            uiOutput("selectInd"),
                                            numericInput("num", label = "Number of answer options for each item", value = 1)
                                          ),
                                          # tableOutput("D1"),
                                          # tableOutput("D2"),
                                          uiOutput("htmltable")
                                          
                                          ),
                                   column(8,
                                          plotlyOutput("DiffPlot")
                                          )
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4,
                                          plotlyOutput("ProfilePlot"),
                                          plotlyOutput("ProfilePlot_Normal")
                                          ),
                                   column(4,
                                          plotlyOutput("ProfilePlot_Cheater"),
                                          plotlyOutput("ProfilePlot_Lucky")
                                   ),
                                   column(4,
                                          plotlyOutput("ProfilePlot_Creative"),
                                          plotlyOutput("ProfilePlot_Careless")
                                          )
                                 )
                                 )
                        )
                        )
                                   )

function(input, output, session) {
  
  observeEvent(input$warning, {
    # Save the ID for removal later
    showNotification({
      HTML(paste("Dataframe must be a csv file where rows correspond to individuals and 
columns to items. If an item is answered correctly by an individual the value of the 
cell is 1, if it is incorrect, it is a 0. The structure must be similar to the one
shown below."),'<img src="ex.png" height="90px" width="160px" class="center">')
      }
, duration = 15)
  })
  
  # observeEvent(input$warning, {
  #   showModal(modalDialog(
  #     title = NULL,
  #     paste("Dataframe must be a csv file where rows correspond to individuals 
  #                   and columns to items. The structure must be similar to the one
  #           shown below."),
  #     HTML('<img src="ex.png" height="90px" width="160px" class="center">'),
  #     
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })

  data <- reactive({
    if(is.null(input$file))     return(NULL)else{
      read.csv(input$file$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote)}})
  
  output$contents <- renderTable({
    if(is.null(data)) return(NULL) else{
      if(input$disp == "head") {
        return(head(data()))
      }
      else {
        return(data())
      }}})
  
  ########### SUMMARY:
  output$sample <- renderPrint({
    cat(paste(ncol(data()), " items,", nrow(data()), " sample units and", paste(sum(is.na(data()))), " missing values."))
        })
  
  
  output$perc <- renderPlotly({plotPerc(data(), input$xlabsize)})
  output$items <- renderPlotly({plotItems(data())})

  ########### INDICES:

  Indexs <- reactive(
    list(C.Sato = PerFit::C.Sato(data(), IRT.PModel = input$IRT.PModel),
         Cstar = PerFit::Cstar(data(), IRT.PModel = input$IRT.PModel), 
         U3 = PerFit::U3(data(), IRT.PModel = input$IRT.PModel), 
         Ht = PerFit::Ht(data(), IRT.PModel = input$IRT.PModel), 
         lz = PerFit::lz(data(), IRT.PModel = input$IRT.PModel)))
  Index <- reactive(switch(input$index,
                             "C" = Indexs()[["C.Sato"]],
                             "C*" = Indexs()[["Cstar"]],
                             "U3" = Indexs()[["U3"]],
                             "Ht" = Indexs()[["Ht"]],
                             "lz" = Indexs()[["lz"]]))
  Cutoffs <- reactive(lapply(Indexs(), cutoff))
  Cutoff <- reactive(switch(input$index,
                           "C" = Cutoffs()[["C.Sato"]],
                           "C*" = Cutoffs()[["Cstar"]],
                           "U3" = Cutoffs()[["U3"]],
                           "Ht" = Cutoffs()[["Ht"]],
                           "lz" = Cutoffs()[["lz"]]))
  
  cutoff.value <- reactive(switch(input$cutoff.chosen,
                         "value" = NA, 
                         "conservativeIC" = if(Cutoff()$Tail == "upper"){
                           Cutoff()$Cutoff.CI[2]}else{
                             Cutoff()$Cutoff.CI[1]}))
  
  cutoff.value.all <- reactive(lapply(Cutoffs(), function(x) {
                                      switch(input$cutoff.chosen,
                                  "value" = NA, 
                                  "conservativeIC" = if(x$Tail == "upper"){
                                    x$Cutoff.CI[2]}else{
                                      x$Cutoff.CI[1]})}))
  
  Flagged.all <- reactive(lapply(seq_along(Indexs()), function(x) {
    flagged.resp(Indexs()[[x]], UDlvl = cutoff.value.all()[[x]], ord = TRUE)
  }))
  
  Flagged <- reactive(switch(input$index,
                            "C" = Flagged.all()[["C.Sato"]],
                            "C*" = Flagged.all()[["Cstar"]],
                            "U3" = Flagged.all()[["U3"]],
                            "Ht" = Flagged.all()[["Ht"]],
                            "lz" = Flagged.all()[["lz"]]))

  Flagged.index.values <- reactive(Flagged()$Scores[,c(1,ncol(Flagged()$Scores))])
  output$index.tab <- DT::renderDataTable(DT::datatable({
    Flagged.index.values()
  }))
  
  output$index.plot <- renderPlotly({
    plot(Index(), UDlvl = cutoff.value())
  })
  
  cutoff.value.all <- reactive(lapply(Cutoffs(), function(x) {
    switch(input$cutoff.chosen,
           "value" = NA, 
           "conservativeIC" = if(x$Tail == "upper"){
             x$Cutoff.CI[2]}else{
               x$Cutoff.CI[1]})}))
  
  allflagged <- reactive(AllFlagged(indexsList = Indexs()[which(names(Indexs()) %in% input$indexs)], 
                                    UDlvl = input$cutoff.chosen2, IRT.PModel = input$IRT.PModel2))
  
  
  output$allflagged.tab <- DT::renderDataTable({
    DT::datatable(allflagged(), rownames = FALSE) %>%  formatRound(columns = colnames(allflagged())[-ncol(allflagged())], digits = c(0, rep(2, times = (ncol(allflagged()) - 2))))
  })
  
  Profiles <- reactive(profiles(flagged.dataframe = allflagged()))
  Profiles2 <- reactive({
    Profiles2 <- Profiles()[,-which(colnames(Profiles()) %in% "count")]
    Profiles2[,1:length(input$indexs)][Profiles2[,1:length(input$indexs)] == 0] <- "⚪"
    Profiles2[,1:length(input$indexs)][Profiles2[,1:length(input$indexs)] == 1] <- "⚫"
    Profiles2
  })
  
  ind.hline <- reactive({
    tab <- as.data.frame(table(Profiles2()$flags))
    tab <- tab[order(tab$Var1, decreasing = TRUE), ]
    ind.hline <- cumsum(tab$Freq)[-6]
    ind.hline
  })
  
  
  output$profiles.table <- renderTable(Profiles2(), na = "")
  
 
  ########### Goodness of fit & local independency
  

  fit <- reactive({
    require(irtoys)
    est(data(), model = input$IRT.PModel3, engine = "ltm", nqp = 20)})
  par <- reactive(fit()$est)
  
  modfit <- reactive({
    MODFIT(data = data(), IP = par(), const = FALSE, precision = 4)
  })
  
  output$modfitSummary <- renderTable({
    summary <- modfit()$Summary.table
    summary2 <- data.frame("Under 3" = rowSums(summary[,1:3]), "Over 3" = rowSums(summary[,4:7]),
                           "Mean" = summary[,"Mean"], "SD" = summary[,"SD"])
    summary2
  }, rownames = TRUE, sanitize.colnames.function=function(x)gsub("\\."," ",x))
  
  output$casesItemsSinglets <- DT::renderDataTable(data.frame(chisq.adj.df = modfit()$Singlets[,7], check.names = FALSE),
                                             rownames = TRUE)
  output$casesItemsDoublets <- DT::renderDataTable({
    DoubletsItems <- data.frame("Item1" = colnames(data())[as.numeric(modfit()$Doublets[,"Item1"])])
    DoubletsItems[,"Item2"] <- colnames(data())[as.numeric(modfit()$Doublets[,"Item2"])]
    DoubletsItems[,"chisq.adj.df"] <- modfit()$Doublets[,"chisq.adj.df"]
    DoubletsItems
    },rownames = FALSE)
  
  output$casesItemsTriplets <- DT::renderDataTable({
    TripletsItems <- data.frame("Item1" = colnames(data())[
      as.numeric(modfit()$Triplets[,"Item1"])])
    TripletsItems[,"Item2"] <- colnames(data())[as.numeric(modfit()$Triplets[,"Item2"])]
    TripletsItems[,"Item3"] <- colnames(data())[as.numeric(modfit()$Triplets[,"Item3"])]
    TripletsItems[,"chisq.adj.df"] <- modfit()$Triplets[,"chisq.adj.df"]
    TripletsItems
  },rownames = FALSE)
  

  ########### Unidimensionality 
  # 1
  UN <- reactive({
    unidimTest.jorge1(ltm(data() ~ z1))
  })
  digits = 3
  output$printUN <- renderPrint({
    cat("<b>Alternative hypothesis</b>: the second eigenvalue of the observed data is substantially larger 
        than the second eigenvalue of data under the assumed IRT model <br/>
        Second eigenvalue in the observed data:", round(UN()$Tobs[2], digits), "<br/>",
        "Average of second eigenvalues in Monte Carlo samples:", round(mean(UN()$T.boot[, 2], na.rm = TRUE), digits), "<br/>",
        "Monte Carlo samples:", NROW(UN()$T.boot), "<br/>",
        "p-value:", round(UN()$p.value, digits), "<br/>")
  })
  output$vaps <- renderPlotly({
    y1 <- UN()$Tobs
    y2 <- colMeans(UN()$T.boot, na.rm = TRUE)
    
    y12 <- y1/sum(y1)
    y22 <- y2/sum(y2)
    
    plot_ly(x = c(1:length(UN()$Tobs)), y = y12, mode = "markers", 
            type = "scatter", mode = "line", marker = list(color = "rgba(154, 205, 50, 1)"),
            name = "Observed") %>%
    add_trace(x = c(1:length(UN()$Tobs)), y = y22, mode = "markers", 
              marker = list(color = "rgba(102, 102, 102, 1)"), name = 'Mean of simulations')%>%
      layout(xaxis = list(title = "Eigenvalue number"), yaxis = list(title = "% variance explained"))
    })
  output$boot2vap <- renderPlotly(plot(UN()))
  
  
  ##### Monotonicity
  
  require(mokken)
  mon <- reactive(check.monotonicity(data(), minvi = 0.03))
  nonMon <- reactive({
    sumMon <- summary(mon())
    nonMon <- sumMon[sumMon[,"#zsig"]>0,] # items that violate the assumption
    nonMon
    })
  nonMonIndexs <- reactive({which(colnames(data()) %in% rownames(nonMon()))})
  
  output$nonMonIndexsCondition <- reactive({
    length(nonMonIndexs()) == 0
  })
  outputOptions(output, 'nonMonIndexsCondition', suspendWhenHidden = FALSE)
  
  output$summaryNonMonIndexs <- renderTable(data.frame("Items" = rownames(nonMon())),
                                            colnames = FALSE)
  
  nonMonIndexsList <- reactive({
    nonMonIndexsList <- as.list(nonMonIndexs())
    names(nonMonIndexsList) <- rownames(nonMon())
    nonMonIndexsList
  })
  output$si <- renderUI(
    selectInput(inputId = "ItemMonotonicity", label = "Select one:", 
                choices = nonMonIndexsList(), selected = nonMonIndexsList()[[1]], 
                multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
  )
  
  output$monotonicityPlot <- renderPlotly({
    plotMonotonicity(mon(), item = as.numeric(input$ItemMonotonicity))
    })
  
  
  ## Diagnostic:
  IDs <- reactive({
    IDs <- as.list(allflagged()$FlaggedIDs)
    names(IDs) <- paste("Individual", allflagged()$FlaggedIDs)
    IDs
  })
  
  output$selectInd <- renderUI(
    selectInput(inputId = "SelectIndividual", label = "Select one:", 
                choices = IDs(), selected = IDs()[[1]], 
                multiple = FALSE, selectize = TRUE, width = NULL, size = NULL))
  output$DiffPlot <- renderPlotly({
    plotDiff(data(), caseRow = as.numeric(input$SelectIndividual))
  })
  
  m <- reactive(apply(data(), 2, sum))
  data2 <- reactive(data()[,  order(m(), decreasing = TRUE)])
  
  Patterns <- reactive({
    patterns(data = data2(), caseRow = as.numeric(input$SelectIndividual),
             responseOptions = input$num) })
  Patterns_thirds <- reactive({
    lapply(Patterns(), sum_tertiles)})
  Observed_thirds <- reactive({
    sum_tertiles(as.numeric(data2()[as.numeric(input$SelectIndividual),]))
  })
  Observed_thirds_relative <- reactive({
    sum_tertiles(as.numeric(data2()[as.numeric(input$SelectIndividual),]), frequence = "relative")
  })
  
  D <- reactive({
    D <- unlist(lapply(Patterns_thirds(), function(x) dist(rbind(x, Observed_thirds()))))
    names(D) <- c("Normal", "Cheater", "Creative", "Lucky Guesser", "Careless")
    D <- as.data.frame(t(D))
    D
  })
  
  output$D1 <- renderTable(D()[,1:3])
  output$D2 <- renderTable(D()[,4:5])

  output$htmltable <- renderUI({
    # define CSS tags
    css <- c("#bggreen {background-color:    #e1f0c1;}")
    # example data frame 
    # add the tag inside the cells
    tab <- D()
    tab[tab == min(tab)] <- paste(round(tab[tab == min(tab)], 2), "#bggreen")
    # generate html table with pander package and markdown package
    require("pander")
    require("markdown")
    require("stringr")
    htmltab <- markdownToHTML(
      text=pandoc.table.return(
        tab, 
        style="rmarkdown", split.tables=Inf
      ), 
      fragment.only=TRUE
    ) 
    colortable(htmltab, css)
  })
  
  output$ProfilePlot <- renderPlotly({
    plotProfile(data(), caseRow = as.numeric(input$SelectIndividual), main = "Observed pattern")
  })
  
  data_aux1 <- reactive({
    dat <- data2()
    dat[as.numeric(input$SelectIndividual),] <- Patterns()[["simNormal"]]
    dat
  })
  output$ProfilePlot_Normal <- renderPlotly({
    plotProfile(data_aux1(), caseRow = as.numeric(input$SelectIndividual), main = "Normal pattern")%>%
      add_trace(x = c(1:3), y = Observed_thirds_relative(), type = "scatter", mode = "lines + markers",
                color = I("gray"), inherit = FALSE, showlegend = FALSE)
  })
  
  data_aux2 <- reactive({
    dat <- data2()
    dat[as.numeric(input$SelectIndividual),] <- Patterns()[["simCheater"]]
    dat
  })
  output$ProfilePlot_Cheater <- renderPlotly({
    plotProfile(data_aux2(), caseRow = as.numeric(input$SelectIndividual), main = "Cheater pattern")%>%
      add_trace(x = c(1:3), y = Observed_thirds_relative(), type = "scatter", mode = "lines + markers",
                color = I("gray"), inherit = FALSE, showlegend = FALSE)
  })
  
  data_aux3 <- reactive({
    dat <- data2()
    dat[as.numeric(input$SelectIndividual),] <- Patterns()[["simCreative"]]
    dat
  })
  output$ProfilePlot_Creative <- renderPlotly({
    plotProfile(data_aux3(), caseRow = as.numeric(input$SelectIndividual), main = "Creative pattern")%>%
      add_trace(x = c(1:3), y = Observed_thirds_relative(), type = "scatter", mode = "lines + markers",
                color = I("gray"), inherit = FALSE, showlegend = FALSE)
  })
  
  data_aux4 <- reactive({
    dat <- data2()
    dat[as.numeric(input$SelectIndividual),] <- Patterns()[["simLucky"]]
    dat
  })
  output$ProfilePlot_Lucky <- renderPlotly({
    plotProfile(data_aux4(), caseRow = as.numeric(input$SelectIndividual), main = "Lucky guesser pattern")%>%
      add_trace(x = c(1:3), y = Observed_thirds_relative(), type = "scatter", mode = "lines + markers",
                color = I("gray"), inherit = FALSE, showlegend = FALSE)
  })
  
  data_aux5 <- reactive({
    dat <- data2()
    dat[as.numeric(input$SelectIndividual),] <- Patterns()[["simCareless"]]
    dat
  })
  output$ProfilePlot_Careless <- renderPlotly({
    plotProfile(data_aux5(), caseRow = as.numeric(input$SelectIndividual), main = "Careless pattern")%>%
      add_trace(x = c(1:3), y = Observed_thirds_relative(), type = "scatter", mode = "lines + markers",
                color = I("gray"), inherit = FALSE, showlegend = FALSE)
  })
  
  
  

}


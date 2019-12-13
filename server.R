#server

server <- function(input, output, session) {
  
  fileObs <- callModule(file_upload, "fileObs")
  fileSim <- callModule(file_upload, "fileSim")
  
  observeEvent(fileObs(),{
    updateSelectizeInput(session, inputId = "yvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "xvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "stratvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "predvar", choices = names(fileObs()))
    
  })

  binlessInputs <- callModule(binless_inputs, "binlessInputs1", 
                              reactive({vpc.Binless}))
  

 # binningInputs <- callModule(binning_inputs, "binningInputs1", vpc.Binning)
  
  
  stratformula <- reactive({
    if (!is.null(input$stratvar)) {
      as.formula(paste0("~ ", input$stratvar))
    } else {
      NULL
    }
  })
  
  vpc.Binless <- eventReactive(input$buttonPlot, {
    #strat, optimized, predcorrect
    if(!is.null(stratformula()) && input$isPred && input$typeVPC == "Binless") {
     vpc <- observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        stratify(stratformula()) %>%
        predcorrect(pred = !!rlang::sym(input$predvar)) %>%
        binlessaugment(qpred = binlessInputs()$piUser, interval = binlessInputs()$intervalUser, loess.ypc = TRUE) %>%
        binlessfit() %>%
        binlessvpcstats()
    }
    #strat, optimized, nopredcorrect
    if(!is.null(stratformula()) && !input$isPred && input$typeVPC == "Binless") {
      vpc <- observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        stratify(stratformula()) %>%
        binlessaugment(qpred = binlessInputs()$piUser, interval = binlessInputs()$intervalUser, loess.ypc = FALSE) %>%
        binlessfit() %>%
        binlessvpcstats()
    }
    #nostrat, optimized, predcorrect
    if(is.null(stratformula()) && input$isPred && input$typeVPC == "Binless") {
     vpc <- observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        predcorrect(pred = !!rlang::sym(input$predvar)) %>%
        binlessaugment(qpred = binlessInputs()$piUser, interval = binlessInputs()$intervalUser, loess.ypc = TRUE) %>%
        binlessfit() %>%
        binlessvpcstats()
   }    
    #nostrat, optimized, no predcorrect
   if(input$isAutoOptimize && is.null(stratformula()) && !input$isPred && input$typeVPC == "Binless") {
    vpc <-  observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
       simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
       binlessaugment(qpred = binlessInputs()$piUser, interval = c(0,7), loess.ypc = TRUE) %>%
       binlessfit() %>%
       binlessvpcstats()
}
   #nostrat, no optimized, predcorrect
    if(!input$isAutoOptimize  && is.null(stratformula()) && input$isPred && input$typeVPC == "Binless") {
     vpc <-  observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        predcorrect(pred = !!rlang::sym(input$predvar)) %>%
        binlessaugment(qpred = binlessInputs()$piUser, interval = binlessInputs()$intervalUser, loess.ypc = TRUE) %>%
        binlessfit(llam.quant = binlessInputs()$lamUser, span =  binlessInputs()$spanUser) %>%
        binlessvpcstats()
    }
    
    #nostrat, no optimized, no predcorrect
    if(!input$isAutoOptimize  && is.null(stratformula()) && !input$isPred && input$typeVPC == "Binless") {
      vpc <-  observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        binlessaugment(qpred = binlessInputs()$piUser, interval = binlessInputs()$intervalUser, loess.ypc = TRUE) %>%
        binlessfit(llam.quant = binlessInputs()$lamUser) %>%
        binlessvpcstats()
    }
    
    vpc
    
  })
  
  vpc.Binning <- eventReactive(input$buttonPlot, {
    #Binning, strat, predcorrect
    if (input$typeVPC == "Binning" && !is.null(stratformula()) && input$isPred ) {
    vpc <- observed(fileObs(), x= !!rlang::sym(input$xvar),  y= !!rlang::sym(input$yvar)) %>%
          simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
          stratify(stratformula()) %>%
          binning(!!rlang::sym(input$xvar)) %>%
          predcorrect(!!rlang::sym(input$predvar)) %>%
          vpcstats()
    } 
    #Binning, strat, no pred correct
    if (input$typeVPC == "Binning" && !is.null(stratformula()) && !input$isPred) {
      vpc <- observed(fileObs(), x= !!rlang::sym(input$xvar),  y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        stratify(stratformula()) %>%
        binning(!!rlang::sym(input$xvar)) %>%
        vpcstats()
    } 
    
    #Binning, no strat, pred correct
    if (input$typeVPC == "Binning" && is.null(stratformula()) && input$isPred) {
      vpc <- observed(fileObs(), x= !!rlang::sym(input$xvar),  y= !!rlang::sym(input$yvar)) %>%
        simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
        binning(!!rlang::sym(input$xvar)) %>%
        predcorrect(!!rlang::sym(input$predvar)) %>%
        vpcstats()
    } 
    
    vpc

  })
    
  
  observe({
    if (input$typeVPC == "Binless") {
    callModule(plot_binless, "plotVPC1", vpc = vpc.Binless)
    } else {
    callModule(plot_binless, "plotVPC1", vpc = vpc.Binning)
    }
  })
  
  observeEvent(input$plotCustom, {
    showModal(modalDialog(
      title = "Plot Style",
      "This modal dialog will contain inputs to style plot.",
      easyClose = TRUE,
      footer = NULL)
    )
  })
  
  output$tableObs <- renderDataTable({
    fileObs()
  })
  
  
  
}
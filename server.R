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

  
  piUser <- reactive({
    c(input$piLo, input$piMed, input$piHi)
  })
  
  lamUser <- reactive({
    c(input$lambdaLo, input$lambdaMed, input$lambdaHi)
  })
  
  spanUser <- reactive({
    input$span
  })
  
  stratformula <- reactive({
      if (!is.null(input$stratvar)) {
        as.formula(paste0("~ ", input$stratvar))
      } else {
        NULL
      }
    })
  
    
    vpc.Binless <- eventReactive(input$buttonPlot, {
      if(!is.null(stratformula())) {
        observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
          simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
          stratify(stratformula()) %>%
          predcorrect(pred = !!rlang::sym(input$predvar)) %>%
          binlessaugment(qpred = piUser(), interval = input$interval, loess.ypc = TRUE) %>%
          binlessfit() %>%
          binlessvpcstats()
      } else if(input$isOpt) {
        observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
          simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
          predcorrect(pred = !!rlang::sym(input$predvar)) %>%
          binlessaugment(qpred = piUser(),  interval = input$interval, loess.ypc = TRUE) %>%
          binlessfit() %>%
          binlessvpcstats()
      } else {
        observed(fileObs(), x= !!rlang::sym(input$xvar), y= !!rlang::sym(input$yvar)) %>%
          simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
          predcorrect(pred = !!rlang::sym(input$predvar)) %>%
          binlessaugment(qpred = piUser(),  interval = input$interval, loess.ypc = TRUE) %>%
          binlessfit(llam.qpred = lamUser(), span = spanUser()) %>%
          binlessvpcstats()
      }
    })
  
  observe({
    updateSliderInput(session, "span", value = vpc.Binless()$span)
    updateSliderInput(session, "lambdaLo", value = vpc.Binless()$llam.qpred[[1]], min = input$interval[[1]], max = input$interval[[2]])
    updateSliderInput(session, "lambdaMed", value = vpc.Binless()$llam.qpred[[2]],min = input$interval[[1]], max = input$interval[[2]])
    updateSliderInput(session, "lambdaHi", value = vpc.Binless()$llam.qpred[[3]], min = input$interval[[1]], max = input$interval[[2]])
  })

  observe({
      callModule(plot_binless, "plotVPC1", vpc = vpc.Binless)
  })
  
}
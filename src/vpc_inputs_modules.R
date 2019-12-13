#' Binless Parameters
#' 
#' @param input
#' @param output
#' @param session
#' 
#' @return list with the following components:
#' \describe{
#'   \item{llam}{reactive slider for lambda selection}
#'   }
#'
#'


binless_inputs_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
     column(6,
         tags$h4("Quantiles"),
         tags$hr(),
         sliderInput(inputId = ns('piHi'), label = "Hi", value = .9, min = 0, max = 1, step = .05),
         sliderInput(inputId = ns('piMed'), label = "Med", value = .5, min = 0, max = 1, step = .05),
         sliderInput(inputId = ns('piLo'), label = "Lo", value = .1, min = 0, max = 1, step = .05)
          ),
     column(6,
         tags$h4("Smoothing"),
         tags$hr(),
         sliderInput(ns("interval"), label = "Optimization Interval", min = -2, max = 10, value = c(0, 7)), 
         tags$h5("Additive Quantile Regression"),
         sliderInput(inputId = ns('lambdaHi'), label = "Lambda Hi",value = 3, min = 0, max = 7, step = .01),
         sliderInput(inputId = ns('lambdaMed'), label = "Lambda Med", value = 3, min = 0, max = 7, step = .01),
         sliderInput(inputId = ns('lambdaLo'), label = "Lambda Lo", value = 3, min = 0, max = 7, step = .01),
         checkboxInput(ns("isLoessYPC"), label = "Loess Prediction Corrected"),
         conditionalPanel(
           condition = "input.isLoessYPC == true", ns = ns,
           sliderInput(inputId = ns("span"), label = "Span", min = 0, max = 1, value = .5))
     ) 
  )

}


binless_inputs <- function(input, output, session, vpc.Binless) {

  piUser <- reactive({
    c(input$piLo, input$piMed, input$piHi)
  })
  
  lamUser <- reactive({
    c(input$lambdaLo, input$lambdaMed, input$lambdaHi)
  })
  
  spanUser <- reactive({
    input$span
  })
  
  intervalUser <- reactive({
    input$interval
  })
  
  # 
  # observe({
  #   updateSliderInput(session, "span", value = vpc.Binless()$span)
  #   updateSliderInput(session, "lambdaLo", value = vpc.Binless()$llam.qpred[[1]], min = input$interval[[1]], max = input$interval[[2]])
  #   updateSliderInput(session, "lambdaMed", value = vpc.Binless()$llam.qpred[[2]],min = input$interval[[1]], max = input$interval[[2]])
  #   updateSliderInput(session, "lambdaHi", value = vpc.Binless()$llam.qpred[[3]], min = input$interval[[1]], max = input$interval[[2]])
  # })
  # 
  
  return( reactive({
    list(piUser = piUser(),
         lamUser = lamUser(),
         spanUser = spanUser(),
         intervalUser = intervalUser()
    )
  })
  )
  
}

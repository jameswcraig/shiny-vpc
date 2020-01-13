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
         tags$h4("Binless"),
         tags$hr(),
         sliderInput(ns("interval"), label = "Optimization Interval", min = -2, max = 10, value = c(0, 7)), 
         tags$h5("Additive Quantile Regression"),
         sliderInput(inputId = ns('lambdaHi'), label = "Lambda Hi",value = 3, min = 0, max = 7, step = .01),
         sliderInput(inputId = ns('lambdaMed'), label = "Lambda Med", value = 3, min = 0, max = 7, step = .01),
         sliderInput(inputId = ns('lambdaLo'), label = "Lambda Lo", value = 3, min = 0, max = 7, step = .01),
         checkboxInput("isLoessYPC", label = "Loess Prediction Corrected"),
         conditionalPanel(
           condition = "input.isLoessYPC == true",
           #condition = paste0("input.", ns("isLoessYPC")),
        tags$h5("LOESS"),
        sliderInput(inputId = ns("span"), label = "Span", min = 0, max = 1, value = .5))
     #) 
  )

}


binless_inputs <- function(input, output, session) {

  session = session$ns
  
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
    list(lamUser = lamUser(),
         spanUser = spanUser(),
         intervalUser = intervalUser()
    )
  })
  )
  
}

plot_aesthetics_ui <- function(id) {
ns <- NS(id)
lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
tagList(
  fluidRow(
    column(4,
  selectizeInput(ns("lineTypeHi"), "Quanitle High", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"),
  selectizeInput(ns("lineTypeMed"), "Quantile Median", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
  selectizeInput(ns("lineTypeLo"), "Quantile Low", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dotted")
    )
)
)
myModal <- function() {
  modalDialog(
    renderUI({
      tagList(
        selectizeInput("plotTheme", "Theme",  choices = c("theme_bw", "theme_grey", "theme_gray", "theme_light")),
        fluidRow(
          column(4,
                 tags$h4("Line Type"),
                 selectizeInput("lineTypeHi", "Quanitle High", choices = lineTypes, selected = "dashed"),
                 selectizeInput("lineTypeMed", "Quantile Median", choices = lineTypes, selected = "solid"),
                 selectizeInput("lineTypeLo", "Quantile Low", choices = lineTypes, selected = "dotted")
          )
        )
      )
    }),
    footer = modalButton("Close")
  )
}

}
plot_aesthetics <- function(input, output, session) {

lineTypes <- reactive({
  c(input$lineTypeHi, input$lineTypeMed, input$lineTypeLo)
})

return(
  reactive({lineTypes})
)
}

quantiles_ui <- function(id){
  ns <- NS(id)
  tagList(
         tags$h4("Quantiles"),
         tags$hr(),
         textInput(ns("piUser"), label = "", value = c("0.05,0.5,0.95"))
   )
}

quantiles_server <- function(input, output, session) {
    
    session = session$ns
    
    piUser <- reactive({
      as.numeric(unlist(strsplit(input$piUser, split = ",")))
      })
    
    return(
      reactive({
        piUser = piUser()
      })
    )
}


confidence_interval_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4("Confidence Level"),
    tags$hr(),
    numericInput(ns("ciUser"), label = "", value = .95)
  )
}

confidence_interval_server <- function(input, output, session) {
  
  session = session$ns
  
  ciUser <- reactive({
    input$ciUser
  })
  
  return(
    reactive({
      ciUser = ciUser()
    })
  )
}
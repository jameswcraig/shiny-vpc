# modal_plot_ui <- function(id) {
#   ns <- NS(id)
#   
#   actionButton(ns("openPlotModalBtn"), "Customize Plot")
# }
# 
# # Modal module server
# modal_plot <- function(input, output, session) {
#   
#   myModal <- function() {
#       modalDialog(
#         renderUI({
#           plot_aesthetics_ui("plotInputsUser")
#               }),
#         footer = modalButton("Close")
#       )
#     }
#   
#   # Show modal dialog on start up
#   observeEvent(input$openPlotModalBtn,
#                ignoreNULL = TRUE,
#                showModal(myModal())
#   )
#   
#   lineTypes <- callModule(plot_aesthetics, "plotInputsUser")
#   
#   return(
#     reactive({lineTypes})
#   )
#   
# }

#plot_aesthetics_ui <- function(id) {
  #ns <- NS(id)
  
  #lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  
  # tagList(
  #   fluidRow(
  #     column(4,
  #   selectizeInput(ns("lineTypeHi"), "Quanitle High", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"),
  #   selectizeInput(ns("lineTypeMed"), "Quantile Median", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
  #   selectizeInput(ns("lineTypeLo"), "Quantile Low", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dotted")
  #     )
  # )
  # )
  # myModal <- function() {
  #   modalDialog(
  #     renderUI({
  #       tagList(
  #         selectizeInput("plotTheme", "Theme",  choices = c("theme_bw", "theme_grey", "theme_gray", "theme_light")),
  #         fluidRow(
  #           column(4,
  #                  tags$h4("Line Type"),
  #                  selectizeInput("lineTypeHi", "Quanitle High", choices = lineTypes, selected = "dashed"),
  #                  selectizeInput("lineTypeMed", "Quantile Median", choices = lineTypes, selected = "solid"),
  #                  selectizeInput("lineTypeLo", "Quantile Low", choices = lineTypes, selected = "dotted")
  #           )
  #         )
  #       )
  #     }),
  #     footer = modalButton("Close")
  #   )
  # }
#  
#}

#plot_aesthetics <- function(input, output, session) {
#
# lineTypes <- reactive({
#   c(input$lineTypeHi, input$lineTypeMed, input$lineTypeLo)
# })
#   
# return(
#   reactive({lineTypes})
# )
  

#}

plot_binless_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
  withSpinner(plotOutput(ns("plotVPC"), height = "650px"), type = 8)
  )
  
}

#Server

plot_binless <- function(input, output, session, vpc, plotAes, buttonVal){
  
  require(vpc())
  
  stratformula <- reactive({
    if(!is.null(vpc()$strat)) {
      vpc()$strat.formula
    }
  })
  
  output$plotVPC <- renderPlot({
    if (buttonVal() < 1) {
      plot(vpc())
    } else {
      req(plotAes())
      plot(vpc(), show.points = plotAes()$show.points,
           #show.boundaries = plotAes()$showBoundaries,
           #show.stats = input$showStats,
           xlab = plotAes()$xlab, 
           ylab = plotAes()$ylab, 
           color = plotAes()$color, 
           linetype = plotAes()$linetype,
           legend.position = plotAes()$legendPosition,
           facet.scales = plotAes()$facetScales,
           custom.theme = plotAes()$themeType)
    }
  })
  
  }
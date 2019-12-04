#global

source("lib/libraries.R")
source("src/plot_modules.R")
source("src/data_modules.R")



title <- tags$a(href = "https://certara.com",
                tags$img(src = "certara-shiny.png", height = '35', width = '150'))
# 
# vpcNav <-    wellPanel(style = "background: light grey;",
#                                                  fluidRow(
#                                                    column(12,
#                                                           column(5,
#                                                          column(2,
#                                                            div(
#                                                             dropdown(
#                                                               tags$h5("Quant High"),
#                                                               sliderInput(inputId = 'piHi', label = "", value = .9, min = 0, max = 1, step = .05),
#                                                               tags$h5("Quant Med"),
#                                                               sliderInput(inputId = 'piMed', label = "", value = .5, min = 0, max = 1, step = .05),
#                                                               tags$h5("Quant Low"),
#                                                               sliderInput(inputId = 'piLo', label = "", value = .1, min = 0, max = 1, step = .05),
#                                                              style = "default",
#                                                              label = "Quantile",
#                                                              status = "default",
#                                                              size = "sm",
#                                                              width = "250px",
#                                                              tooltip = tooltipOptions(title = "Select Quantile for Binless VPC"
#                                                              ),
#                                                              animate = animateOptions(
#                                                                enter = animations$fading_entrances$fadeInDown,
#                                                                exit = animations$fading_exits$fadeOutUp)),
#                                                             style="font-size:80%; font-family:Helvetica; margin-top:5px;")
#                                                           ),
#                       column(2,
#                         checkboxInput("isOptLambda", "Optimize Lambda", value = TRUE),
#                         conditionalPanel(
#                           condition = "input.isOptLambda == false",
#                          sliderInput(inputId = 'lambdaHi', label = "",value = 3,min = 0, max = 7, step = .01),
#                          sliderInput(inputId = 'lambdaMed', label = "", value = 3, min = 0, max = 7, step = .01),
#                          sliderInput(inputId = 'lambdaLo', label = "", value = 3, min = 0, max = 7, step = .01))
#                       )
#                                                           )
#                                                    )
#                                                  )
# )
#                                                 
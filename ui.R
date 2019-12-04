#ui

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = title
                                    ),
                    dashboardSidebar(
                                    tags$h3("Data"),
                                    file_upload_ui("fileObs"),
                                    file_upload_ui("fileSim"),
                                    tags$br(),
                                    selectizeInput("yvar", label = "Y Variable", choices = NULL, multiple = FALSE ),
                                    selectizeInput("xvar", label = "X Variable", choices = NULL, multiple = FALSE ),
                                    selectizeInput("stratvar", label = "Stratification Variable(s)", choices = NULL, multiple = TRUE ),
                                    checkboxInput("isPred", label = "Prediction Corrected", value = FALSE, width = NULL),
                                    conditionalPanel(
                                      condition = "input.isPred == true", 
                                      selectizeInput("predvar", label = "Prediction Variable", choices = NULL)
                                    ),
                                    tags$hr(),
                                    actionButton("buttonPlot", label = "Plot VPC")
                                    ),
                    
                    dashboardBody(
                      #vpcNav,
                      fluidPage(
                       fluidRow(
                         column(8,
                          wellPanel(
                             withSpinner(plot_binless_ui("plotVPC1"), type = 8),
                             style = "background: white;")
                              ),
                         column(4, 
                             box(
                               column(6,
                                tags$h4("Quantiles"),
                                tags$hr(),
                               sliderInput(inputId = 'piHi', label = "Hi", value = .9, min = 0, max = 1, step = .05),
                               sliderInput(inputId = 'piMed', label = "Med", value = .5, min = 0, max = 1, step = .05),
                               sliderInput(inputId = 'piLo', label = "Lo", value = .1, min = 0, max = 1, step = .05)
                               ),
                               column(6,
                               tags$h4("Smoothing"),
                               tags$hr(),
                               checkboxInput("isOpt", "Optimize Smoothing Parameters", value = TRUE),
                               sliderInput("interval", label = "Optimization Interval", min = -2, 
                                           max = 10, value = c(0, 7)), 
                               conditionalPanel(condition = "input.isOpt == false",
                                tags$h5("Additive Quantile Regression"),
                                sliderInput(inputId = 'lambdaHi', label = "Lambda Hi",value = 3, min = 0, max = 7, step = .01),
                                sliderInput(inputId = 'lambdaMed', label = "Lambda Med", value = 3, min = 0, max = 7, step = .01),
                                sliderInput(inputId = 'lambdaLo', label = "Lambda Lo", value = 3, min = 0, max = 7, step = .01)
                               ),
                               conditionalPanel(condition = "input.isPred == true & input.isOpt == false", 
                               tags$h5("LOESS Prediction Corrected"),
                               sliderInput("span", label = "Span", min = 0, 
                                           max = 1, value = .5)))
                               , width = 12)
                              )
                            )
                          )
                        )
)
                 
                    

      
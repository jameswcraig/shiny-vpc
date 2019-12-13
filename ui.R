
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
                      radioButtons("typeVPC", label = "", choices = c("Binning", "Binless"), selected = "Binning" ),
                      actionButton("buttonPlot", label = "Plot VPC")),
                    
                    dashboardBody(
                      #vpcNav,
                      fluidPage(
                        fluidRow(
                          column(7,
                                 wellPanel(
                                   withSpinner(plot_binless_ui("plotVPC1"), type = 8),
                                   style = "background: white;")
                          ),
                          column(5, 
                                 tabsetPanel(
                                   tabPanel("Binning",
                                            wellPanel(
                                      radioButtons("typeBinning", label = "Binning Type", choices = c("nbins", "centers", "breaks")),
                                      conditionalPanel(condition = "input.typeBinning == 'nbins'",
                                     numericInput("nbins", label = "Number of Bins", min = 1, max = 12, value = 6, step = 1)))),
                                   tabPanel("Binless",
                                            wellPanel(
                                 conditionalPanel(condition = "input.isAutoOptimize == false",
                                 binless_inputs_ui("binlessInputs1")),
                                 checkboxInput("isAutoOptimize", label = "Optimize Binless VPC"))
                                 )
                              ),                           
                              actionButton("plotCustom", "Customize Plot"),
                          ),
                          dataTableOutput("tableObs")
                        )
                      )
                    )
)

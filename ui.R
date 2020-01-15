ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = title
                    ),
                    dashboardSidebar(
                      file_observed_ui("obs"),
                      file_simulated_ui("sim"),
                      tags$br(),
                      #fluidRow(
                        #column(6,
                      selectizeInput("yvar", label = "Y Variable", choices = NULL, multiple = FALSE, width = '150%' ),
                        #),
                      # column(2,
                      # checkboxInput("log_dv", label = "Log", width = '60%')
                      # ),
                      # column(1,
                      # checkboxInput("exp_dv", label = "Exp", width = '60%'))
                      #  ),
                      selectizeInput("xvar", label = "X Variable", choices = NULL, multiple = FALSE ),
                      #checkboxInput("log_x", label = "Log", width = '60%'),
                      selectizeInput("stratvar", label = "Stratification Variable(s)", choices = NULL, multiple = TRUE ),
                      checkboxInput("isPred", label = "Prediction Corrected", value = FALSE, width = NULL),
                      conditionalPanel(
                        condition = "input.isPred == true", 
                        selectizeInput("predvar", label = "Prediction Variable", choices = NULL)
                      ),
                     checkboxInput("isCensoring", label = "Censoring", value = FALSE, width = NULL),
                     conditionalPanel(
                       condition = "input.isCensoring == true", 
                       radioButtons("censorType", label = "Censor on:", choices = c("Value", "Variable"), selected = "Value"),
                       conditionalPanel(condition = "input.censorType == 'Variable'",
                       selectizeInput("censorvar", label = "Censoring Variable", choices = NULL)
                       ),
                       conditionalPanel(condition = "input.censorType == 'Value'",
                       numericInput("userLLOQ", label = "LLOQ Value", value = 0),
                      )
                     ),
                      radioButtons("typeVPC", label = "", choices = c("Binning", "Binless"), selected = "Binning" ),
                      fluidRow(
                        column(4,
                         actionButton("buttonPlot", label = "Plot", width = '150%' )
                        ),
                        column(4,
                         actionButton("generateCode", "Generate Code")
                        )
                      )
                    ),
                    
                    dashboardBody(
                      fluidPage(
                        fluidRow(
                          column(7,
                              wellPanel(
                               withSpinner(plotOutput("plotVPC", height = "625px"), type = 8),
                               conditionalPanel(condition = "input.isPlotBlq == true",
                                withSpinner(plotOutput("plotBlq", height = "325px"), type = 8),
                                style = "background: white;")
                              )
                          ),
                          column(5,
                              wellPanel(width = 12,
                              tabsetPanel(
                                tabPanel("VPC",
                                    fluidRow(
                                         column(6,
                                                quantiles_ui("qpred1"),
                                         ),
                                         column(6,
                                                confidence_interval_ui("ci1")
                                         )
                                    ),
                                    fluidRow(
                                  conditionalPanel(condition = "input.typeVPC == 'Binning'",
                                    column(6,
                                        tags$h4("Binning"),
                                        tags$hr(),
                                      radioButtons("typeBinning", label = "Binning Type", choices = c("x-variable", "ntile", "sd", "equal", "pretty", "quantile", "kmeans", "jenks", "centers", "breaks")),
                                      conditionalPanel(condition = "input.typeBinning == 'ntile'",
                                        numericInput("nbins", label = "Number of Bins", min = 1, max = 12, value = 6, step = 1)),
                                      conditionalPanel(condition = "input.typeBinning == 'breaks'",
                                        textInput("breaks", label = "Specify Breaks", value = "0,1,3,5")),
                                      conditionalPanel(condition = "input.typeBinning == 'centers'",
                                        textInput("centers", label = "Specify Centers", value = "0,1,3,5"))
                                      ), 
                                    column(6,
                                      tags$h4("Midpoint"),
                                      tags$hr(),
                                      radioButtons("midPoint", label = "Midpoint", choices = c("xmedian", "xmean", "xmid", "xcenter"))
                                    ))
                                    ),
                                  fluidRow(
                                  conditionalPanel(condition = "input.typeVPC == 'Binless'",
                                    column(12,
                                      conditionalPanel(condition = "input.isAutoOptimize == false",
                                      binless_inputs_ui("binlessInputs1"),
                                      uiOutput("stratLambdas")),
                                      checkboxInput("isAutoOptimize", label = "Optimize Binless VPC", value = FALSE),
                                      conditionalPanel(condition = "input.isAutoOptimize == true",
                                      tableOutput("optLambda"),
                                      tableOutput("optSpan"))
                                    )
                                  )
                                 )
                               ),
                               tabPanel("Customize Plot",
                                          fluidPage(
                                          fluidRow(
                                          column(3,
                                                 tags$h4("Line Type"),
                                                 tags$hr(),
                                                 selectizeInput("lineTypeHi", "Quantile High", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"),
                                                 selectizeInput("lineTypeMed", "Quantile Median", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
                                                 selectizeInput("lineTypeLo", "Quantile Low", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dotted"),
                                                 checkboxInput("showPoints", "Show Points", value = TRUE)
                                          ),
                                          column(3,
                                                 tags$h4("Line Color"),
                                                 tags$hr(),
                                                 selectizeInput("colorTypeHi", "Quantile High", choices = c("red", "black", "blue"), selected = "red"),
                                                 selectizeInput("colorTypeMed", "Quantile Median", choices = c("red", "black", "blue"), selected = "blue"),
                                                 selectizeInput("colorTypeLo", "Quantile Low", choices = c("red", "black", "blue"), selected = "red"),
                                                 numericInput("colorFill", "Fill Transparency", value = .1, min = .01, max = .99, step = .01)
                                          ),
                                          column(3,
                                                 tags$h4("Labels"),
                                                 tags$hr(),
                                                 textInput("xlabel", "X-Label", value = "TIME"),
                                                 textInput("ylabel", "Y-Label", value = "Concentration"),
                                                 #tags$h4("Theme"),
                                                 #selectizeInput("themeType", "Select Theme", choices = c("theme_grey", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic"), selected = "theme_bw")
                                          ),
                                          column(3,
                                                 tags$h4("Arrange"),
                                                 tags$hr(),
                                                 selectizeInput("legendPosition", "Legend", choices = c("top", "bottom", "left", "right"), selected = "top"),
                                                 selectizeInput("facetScales", "Axis Scales", choices = c("free", "fixed"), selected = "top"),
                                                 checkboxInput("isLogDV", "Log Y", value = FALSE),
                                                 checkboxInput("isLogX", "Log X", value = FALSE),
                                                 checkboxInput("showBinning", "Show Binning", value = TRUE), 
                                                 checkboxInput("showBoundaries", "Show Boundaries", value = TRUE),
                                                 checkboxInput("showStats", "Show Stats", value = TRUE), 
                                                 checkboxInput("facetQuantile", "Facet Quantiles", value = FALSE), 
                                                 checkboxInput("isPlotBlq", "Show BLQ", value = FALSE)
                                 )
                                )
                               )
                              )
                              )
                            ) 
                          ),
                          dataTableOutput("tableObs")
                        )
                      )
                    )
)


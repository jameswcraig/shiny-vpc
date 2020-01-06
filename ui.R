
#create separate action plot vpc buttons in conditional panels
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = title
                    ),
                    dashboardSidebar(
                      tags$h3("Data"),
                      file_observed_ui("fileObs"),
                     # file_simulated_ui("fileSim"),
                     fileInput( "file_sim_t",
                       label = "Upload Simulated File",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv", ".fit", ".RDS")),
                     
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
                          column(8,
                                 wellPanel(
                                   withSpinner(plotOutput("plotVPC"), type = 8),
                                   style = "background: white;")
                          ),
                          column(4,
                                 tabsetPanel(
                                   tabPanel("VPC",
                                 conditionalPanel(condition = "input.typeVPC == 'Binning'",
                                  wellPanel(
                                    quantiles_ui("qpred1"),
                                      radioButtons("typeBinning", label = "Binning Type", choices = c("x-variable", "ntile", "sd", "equal", "pretty", "quantile", "kmeans", "jenks", "breaks")),
                                      radioButtons("midPoint", label = "Midpoint", choices = c("xmedian", "xmean", "xmid", "xcenter")),
                                      conditionalPanel(condition = "input.typeBinning == 'ntile'",
                                     numericInput("nbins", label = "Number of Bins", min = 1, max = 12, value = 6, step = 1)))),
                                 conditionalPanel(condition = "input.typeVPC == 'Binless'",
                                  wellPanel(
                                    conditionalPanel(condition = "input.isAutoOptimize == false",
                                 quantiles_ui("qpred1"),
                                 binless_inputs_ui("binlessInputs1")),
                                 checkboxInput("isAutoOptimize", label = "Optimize Binless VPC", value = FALSE),
                                 tableOutput("optLambda")
                                  )
                                 )
                               ),
                               tabPanel("Customize Plot",
                                        fluidRow(
                                          column(4,
                                                 tags$h4("Line Type"),
                                                 selectizeInput("lineTypeHi", "Quanitle High", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"),
                                                 selectizeInput("lineTypeMed", "Quantile Median", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
                                                 selectizeInput("lineTypeLo", "Quantile Low", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dotted"),
                                                 tags$hr(),
                                                 tags$h4("Theme"),
                                                 selectizeInput("themeType", "Select Theme", choices = c("theme_grey", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic"), selected = "theme_bw")
                                                 
                                          ),
                                          column(4,
                                                 tags$h4("Line Color"),
                                                 selectizeInput("colorTypeHi", "Quantile High", choices = c("red", "black", "blue"), selected = "red"),
                                                 selectizeInput("colorTypeMed", "Quantile Median", choices = c("red", "black", "blue"), selected = "blue"),
                                                 selectizeInput("colorTypeLo", "Quantile Low", choices = c("red", "black", "blue"), selected = "red")
                                          ),
                                          column(2,
                                                 tags$h4("Labels"),
                                                 textInput("xlabel", "X-Label", value = "TIME"),
                                                 textInput("ylabel", "Y-Label", value = "Concentration")
                                          ),
                                          column(4,
                                                 checkboxInput("showPoints", "Show Points", value = TRUE),
                                                 checkboxInput("showBoundaries", "Show Boundaries", value = TRUE),
                                                 checkboxInput("showStats", "Show Stats", value = TRUE),
                                                 selectizeInput("legendPosition", "Legend Position", choices = c("top", "bottom"), selected = "top"),
                                                 selectizeInput("facetScales", "Facet Scales", choices = c("free", "fixed"), selected = "top")
                                 )
                                )
                               )
                              ), 
                              #actionButton("customPlot", "Customize Plot"),
                              actionButton("generateCode", "Generate Code")
                          ),
                          dataTableOutput("tableObs")
                        )
                      )
                    )
)


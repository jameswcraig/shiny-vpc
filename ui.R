ui <- dashboardPagePlus(
  skin = "black-light",
  header = dashboardHeaderPlus(title = tagList(
    span(class = "logo-lg", title), 
    img(src = "certara_thumbnail.png", height = "34", width = "34", padding_left = "-10")),
                               enable_rightsidebar = TRUE,
                               rightSidebarIcon = "paint-brush"),
  sidebar = dashboardSidebar(
    tags$style(
      tags$link(rel = "stylesheet", type = "text/css", href = "certara.css")
    ),
    sidebarMenu(
      menuItem("Upload Data", icon = icon("upload"),
               file_observed_ui("obs"),
               file_simulated_ui("sim")
      ),
      menuItem("Variables", icon = icon("check-double"), 
               selectizeInput("yvar", label = "Y Variable", choices = NULL, multiple = FALSE, width = '150%' ),
               checkboxInput("log_dv", label = "Log Y", width = '60%'),
               selectizeInput("xvar", label = "X Variable", choices = NULL, multiple = FALSE ),
               selectizeInput("stratvar", label = "Stratification Variable(s)", choices = NULL, multiple = TRUE ),
               checkboxInput("isPred", label = "Prediction Corrected", value = FALSE, width = NULL),
               conditionalPanel(condition = "input.isPred == true", 
                                selectizeInput("predvar", label = "Prediction Variable", choices = NULL)),
               checkboxInput("isCensoring", label = "Censoring", value = FALSE, width = NULL),
               conditionalPanel(condition = "input.isCensoring == true", 
                                radioButtons("censorType", label = "Censor on:", choices = c("Value", "Variable"), selected = "Value"),
                                conditionalPanel(condition = "input.censorType == 'Variable'",
                                                 selectizeInput("censorvar", label = "Censoring Variable", choices = NULL)),
                                conditionalPanel(condition = "input.censorType == 'Value'",
                                                 numericInput("userLLOQ", label = "LLOQ Value", value = 0)))),
      menuItem("Quantiles", icon = icon("line-chart"),
               quantiles_ui("qpred1")),
      menuItem("Confidence Level", icon = icon("percentage"),
               confidence_interval_ui("ci1")),
      menuItem("Controls", icon = icon("sliders-h"),
               radioButtons("typeVPC", label = "", choices = c("Binning", "Binless"), selected = "Binning"),
               fluidRow(
                 column(4,
                        actionButton("buttonPlot", label = "Plot", width = "150%")
                 ),
                 column(4,
                        actionButton("generateCode", label = "", icon = icon("code"))
                 )
               )
      )
    )
  ),
  
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "certara.css")),
    tags$body(
      tags$link(rel = "stylesheet", type = "text/css", href = "certara.css")),
    tags$br(),
    fluidRow(
      column(9,
             wellPanel(width = NULL,
                       fluidRow(
                         withSpinner(plotOutput("plotVPC", width = "100%", height = "auto"), type = 8),
                         conditionalPanel(condition = "input.isPlotBlq == true",
                                          withSpinner(plotOutput("plotBlq", height = "267px"),type = 8),
                                          style = "background: white;")),
                       dataTableOutput("tableObs"),
             )
      ),
      column(3,
             box(width = NULL,
                 conditionalPanel(condition = "input.typeVPC == 'Binning'",
                                  tags$h4("Binning Methods"),
                                  tags$hr(),
                                  conditionalPanel("input.isBinStrat == true && input.stratvar.length  < 2",
                                                   uiOutput("stratpanels")),
                                  conditionalPanel("input.isBinStrat == false",
                                                   radioButtons("typeBinning", label = "Binning Type", choices = c("x-variable", "ntile", "pam", "sd", "equal", "pretty", "quantile", "kmeans", "jenks", "centers", "breaks")),
                                                   conditionalPanel(condition = "input.typeBinning == 'breaks'",
                                                                    textInput("breaks", label = "Breaks", value = "0,1,3,5")),
                                                   conditionalPanel(condition = "input.typeBinning == 'centers'",
                                                                    textInput("centers", label = "Centers", value = "0,1,3,5")),
                                                   numericInput("nbins", label = "Number of Bins", min = 1, max = 12, value = 6, step = 1, width = "30%"),
                                                   tags$br(),     
                                                   tags$h4("Midpoint"),
                                                   tags$hr(),
                                                   radioButtons("midPoint", label = "Midpoint", choices = c("xmedian", "xmean", "xmid", "xcenter"))
                                  ), 
                                  tags$br(),
                                  conditionalPanel(condition = "input.stratvar.length  == 1",
                                                   checkboxInput("isBinStrat", label = "Bin by Strata", value = FALSE)),
                                  
                 ) ,
                 conditionalPanel(condition = "input.typeVPC == 'Binless'",
                                  column(12,
                                         tags$h4("Binless Parameters"),
                                         tags$hr(),
                                         conditionalPanel(condition = "(input.isAutoOptimize == false && input.stratvar.length < 1)",
                                                          binless_inputs_ui("binlessInputs1")),
                                         checkboxInput("isBinlessStrata", label = "Binless by Strata", value = FALSE),
                                         switchInput("isAutoOptimize", label = "Smoothing", onLabel = "Auto", offLabel = "Man", value = FALSE),
                                         conditionalPanel("input.isBinlessStrata == true && input.isAutoOptimize == false && input.stratvar.length < 2",
                                                          uiOutput("stratLambdas")),
                                  )
                 )
             )
      )
    ), tags$footer()
  ),
  
  rightsidebar = rightSidebar(background = "light",
                              rightSidebarTabContent(
                                id = "linesSB",
                                title = NULL,
                                icon = "chart-line",
                                active = FALSE,
                                tags$h4("Line Type"),
                                selectizeInput("lineTypeHi", "Quantile High", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dashed"),
                                selectizeInput("lineTypeMed", "Quantile Median", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
                                selectizeInput("lineTypeLo", "Quantile Low", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dotted"),
                                tags$hr(),
                                tags$h4("Line Color"),
                                selectizeInput("colorTypeHi", "Quantile High", choices = c("red", "black", "blue"), selected = "red"),
                                selectizeInput("colorTypeMed", "Quantile Median", choices = c("red", "black", "blue"), selected = "blue"),
                                selectizeInput("colorTypeLo", "Quantile Low", choices = c("red", "black", "blue"), selected = "red"),
                                numericInput("colorFill", "Fill Transparency", value = .1, min = .01, max = .99, step = .01)
                              ),
                              rightSidebarTabContent(
                                id = "layoutSB",
                                title = NULL,
                                active = TRUE,
                                icon = "th-large",
                                tags$h4("Layout"),
                                selectizeInput("legendPosition", "Legend", choices = c("top", "bottom", "left", "right"), selected = "top"),
                                selectizeInput("facetScales", "Axis Scales", choices = c("free", "fixed"), selected = "top"),
                                checkboxInput("facetQuantile", "Facet Quantiles", value = FALSE),
                                checkboxInput("isLogDV", "Log Y", value = FALSE),
                                checkboxInput("isLogX", "Log X", value = FALSE),
                                tags$hr(),
                                tags$h4("Show/Hide"),
                                checkboxInput("showPoints", "Points", value = TRUE),
                                checkboxInput("showBinning", "Binning", value = TRUE),
                                checkboxInput("showBoundaries", "Boundaries", value = TRUE),
                                checkboxInput("showStats", "Stats", value = TRUE),
                                checkboxInput("isPlotBlq", "BLQ", value = FALSE)
                              ),
                              rightSidebarTabContent(
                                id = "edit",
                                title = NULL, 
                                icon = "edit",
                                tags$h4("Labels"),
                                tags$hr(),
                                textInput("xlabel", "X-Label", value = "TIME"),
                                textInput("ylabel", "Y-Label", value = "Concentration")
                              ),
                              tags$script(HTML(
                                "$(\".control-sidebar\").on('webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend', function() { $(window).trigger(\"resize\"); } );"
                              ))
  ),
  footer = dashboardFooter(
   right_text =  tags$div(class = "ficon", tagList(facebook, twitter, linkedin, youtube)))
)


#' File Import Module
#' @param id
#' @return list containing file input ui elements



file_observed_ui <- function(id) {
  ns <- NS(id)
  
    fileInput(
      ns("dataFile"), label = "Upload Observed File",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv", ".fit", ".RDS"))
  
  
}

file_observed_server <- function(input, output, session) {
 
   observeEvent(is.null(input$dataFile), {
    showNotification("Please upload observed and simulated data files")
  })
  
  data <- metaReactive2({
    req(input$dataFile)
    if (endsWith(input$dataFile$datapath, "RDS")) {
      metaExpr({
        readRDS(..(input$dataFile$datapath))
        })
    } else {
      metaExpr({
        fread(..(input$dataFile$datapath))
        })
    }# skip=1, #only skip if fit?
    #header=T, stringsAsFactors=
  })
}


file_simulated_ui <- function(id) {
  ns <- NS(id)
  
  fileInput(
    ns("dataFile"), label = "Upload Simulated File",
    multiple = FALSE,
    accept = c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv", ".fit", ".RDS"))
  
}

file_simulated_server <- function(input, output, session) {
  
  data <- metaReactive2({
    req(input$dataFile)
    if (endsWith(input$dataFile$datapath, "RDS")) {
      metaExpr({
        readRDS(..(input$dataFile$datapath))
      })
    } else {
      metaExpr({
        fread(..(input$dataFile$datapath))
      })
    }# skip=1, #only skip if fit?
    #header=T, stringsAsFactors=
  })
}
# 
# renderUI({
#   verbatimTextOutput(outputId = "fileExt" input$dataFile)
#            )

#To do, check if simulated is multiple of observed
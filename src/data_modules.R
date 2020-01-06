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

file_observed <- function(input, output, session) {
  
  reactive({
    req(input$dataFile)
    if (endsWith(input$dataFile$datapath, "RDS")) {
      readRDS(input$dataFile$datapath)
    } else {
      fread(input$dataFile$datapath)
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

file_simulated <- function(input, output, session) {
  
  metaReactive2({
    req(input$dataFile)
    if (endsWith(input$dataFile$datapath, "RDS")) {
      metaExpr({
    readRDS(..(input$dataFile$datapath))
  })
    } else {
      fread(input$dataFile$datapath)
    }# skip=1, #only skip if fit?
    #header=T, stringsAsFactors=
  })
}
# 
# renderUI({
#   verbatimTextOutput(outputId = "fileExt" input$dataFile)
#            )

#To do, check if simulated is multiple of observed
#' File Import Module
#' @param id
#' @return list containing file input ui elements



file_upload_ui <- function(id) {
  ns <- NS(id)
  
    fileInput(
      ns("dataFile"), "Upload Data File",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv", ".fit", ".RDS"))
  
  
}

file_upload <- function(input, output, session) {
  
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



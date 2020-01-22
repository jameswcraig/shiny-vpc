#Helper Functions


render_lam_strat <- function(m, data){
  r <- list()
  l <- lapply(data, unique)
  stratlevels <- as.character(unlist(l))
  stratname <- names(unlist(l))
  stratname <- gsub('[[:digit:]]+', '', stratname)
  name <- sort(paste0(stratname, stratlevels))
  for (i in 1:length(name)) {
    r[[i]] <- textInput(paste0(name[[i]]), label = paste0(name[[i]]), value = "3,3,3")
  }
  r
}

# render_strat_binning <- function(m, data){
#   r <- list()
#   l <- lapply(data, unique)
#   stratlevels <- as.character(unlist(l))
#   stratname <- names(unlist(l))
#   stratname <- gsub('[[:digit:]]+', '', stratname)
#   name <- sort(paste0(stratname, stratlevels))
#   for (i in 1:length(name)) {
#     r[[i]] <- dropdownBlock(id = paste0(name[[i]]),  title = paste0(name[[i]]), badgeStatus = NULL,
#                           radioButtons(paste0("typeBinning", name[[i]]), label = paste("Binning Type", name[[i]], sep = ": "), choices = c("x-variable", "ntile", "pam", "sd", "equal", "pretty", "quantile", "kmeans", "jenks", "centers", "breaks")),
#                           radioButtons(paste0("midpoint", name[[i]]), label = paste("Midpoint", name[[i]], sep = ": "), choices = c("xmedian", "xmean", "xmid", "xcenter"))
#                           )
#   }
#   r
# }


render_strat_binning <- function(m, data){
  r <- list()
  l <- lapply(data, unique)
  stratlevels <- as.character(unlist(l))
  stratname <- names(unlist(l))
  stratname <- gsub('[[:digit:]]+', '', stratname)
  name <- sort(paste(stratname, stratlevels, sep = "-"))
  ord <- order(name)
  for (i in 1:length(name)) {
    r[[i]] <- fluidRow(
      column(8,
            selectizeInput(paste0("typeBinning", ord[[i]]), label = paste("Binning Type", name[[i]], sep = ": "), choices = c("x-variable", "ntile", "pam", "sd", "equal", "pretty", "quantile", "kmeans", "jenks", "centers", "breaks")),
            conditionalPanel(condition = paste0("input.typeBinning", ord[[i]], " != 'centers'", " && ", "input.typeBinning", ord[[i]], " != 'breaks'"),
              numericInput(paste0("nbins", ord[[i]]), label = paste("N Bins", name[[i]], sep = ": "), value = 4, step = 1)),
            conditionalPanel(condition = paste0("input.typeBinning", ord[[i]], " == 'centers'"),
              textInput(paste0("centers", ord[[i]]), label = paste("Centers", name[[i]], sep = ": "), value = "1,3,5")),
            conditionalPanel(condition = paste0("input.typeBinning", ord[[i]], " == 'breaks'"),
              textInput(paste0("breaks", ord[[i]]), label = paste("Breaks", name[[i]], sep = ": "), value = "1,3,5"))
            #selectizeInput(paste0("midpoint", ord[[i]]), label = paste("Midpoint", name[[i]], sep = ": "), choices = c("xmedian", "xmean", "xmid", "xcenter"))
    )
    )
  }
  r
}


getnum <- function(t){
  as.numeric(unlist(strsplit(t, split = ",")))
}
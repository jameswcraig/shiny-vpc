#server

server <- function(input, output, session) {
  
  fileObs <- callModule(file_observed, "fileObs")
  #fileSim <- callModule(file_simulated, "fileSim")
  
  fileSim <- metaReactive({
      readRDS(..(input$file_sim_t$datapath))
  })

  observeEvent(fileObs(),{
    updateSelectizeInput(session, inputId = "yvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "xvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "stratvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "predvar", choices = names(fileObs()))
    updateSelectizeInput(session, inputId = "censorvar", choices = names(fileObs()))
    
  })

 binlessInputs <- callModule(binless_inputs, "binlessInputs1")
 
 userQuantiles <- callModule(quantiles_server, "qpred1")
 
 confidenceInterval <- callModule(confidence_interval_server, "ci1")
   
 plotInputs <- callModule(modal_plot, "customPlot")

 
 output$vpccode <-  metaRender(renderPrint,{
   ..(vpc())
 })
 
 vpc <- metaReactive2({
   req(input$buttonPlot) 
   isolate({
     if(length(input$stratvar > 1)) {
       strata <- paste(input$stratvar, collapse = " + ")
       form <- formula(paste0("~", strata))
     } else if(length(input$stratvar == 1)) {
       strata <- input$stratvar
       form <- formula(paste0("~", strata))
     } else {
       form <- NULL
     }
     
     
     #strat, optimized, predcorrect
     if(input$typeVPC == "Binless" && input$isAutoOptimize && input$isPred && !is.null(input$stratvar)) {
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           stratify(..(form)) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval())) %>%
           vpcstats()
       })
       #strat, optimized, nopredcorrect
     } else if (input$typeVPC == "Binless" && input$isAutoOptimize && !is.null(input$stratvar) && !input$isPred) {
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           stratify(..(form)) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = FALSE) %>%
           binlessfit(conf.level = ..(confidenceInterval())) %>%
           vpcstats()
       })
     } else if(input$typeVPC == "Binless" && input$isAutoOptimize && is.null(input$stratvar) && input$isPred) {
       #nostrat, optimized, predcorrect
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval())) %>%
           vpcstats()
       })
       #nostrat, optimized, no predcorrect
     } else if(input$typeVPC == "Binless" && input$isAutoOptimize && is.null(input$stratvar) && !input$isPred) {
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval())) %>%
           vpcstats()
       })
       #nostrat, no optimized, no predcorrect
     } else if (input$typeVPC == "Binless" && !input$isAutoOptimize  && is.null(input$stratvar) && !input$isPred) {
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval()), llam.quant = ..(binlessInputs()$lamUser)) %>%
           vpcstats()
       })
       #nostrat no optimized, predcorrect
     } else if (input$typeVPC == "Binless" && !input$isAutoOptimize  && is.null(input$stratvar) && input$isPred) {
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval()), llam.quant = ..(binlessInputs()$lamUser), span = ..(binlessInputs()$spanUser)) %>%
           vpcstats()
       })
       #Binning, strat, no pred correct, censoring
     } else if (input$typeVPC == "Binning" && !is.null(input$stratvar) && !input$isPred && input$isCensoring) {
       if (input$typeBinning == "x-variable") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
       #Binning, strat, no pred correct, no censoring
     } else if (input$typeVPC == "Binning" && !is.null(input$stratvar) && !input$isPred && !input$isCensoring) {
       if (input$typeBinning == "x-variable") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
       #Binning, strat, pred correct, no censoring
     } else if (input$typeVPC == "Binning" && !is.null(input$stratvar) && input$isPred && !input$isCensoring) {
       if (input$typeBinning == "x-variable") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
       #Binning, strat, pred correct, censoring
     } else if (input$typeVPC == "Binning" && !is.null(input$stratvar) && input$isPred && input$isCensoring) {
       if (input$typeBinning == "x-variable") {
       metaExpr({
         observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
           stratify(..(form)) %>%
           binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
           predcorrect(!!rlang::sym(..(input$predvar))) %>%
           vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
       })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             stratify(..(form)) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
       #Binning, no strat, pred correct, no censoring
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && input$isPred && !input$isCensoring) {
       if (input$typeBinning == "x-variable") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
       #Binning, no strat, pred correct, censoring
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && input$isPred && input$isCensoring) {
       if (input$typeBinning == "x-variable") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             predcorrect(!!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
       #Binning, no strat, no pred correct, censoring
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && !input$isPred && input$isCensoring) {
         if (input$typeBinning == "x-variable") {
           metaExpr({
             observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
               simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
               censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
               binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
               vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
           })
         } else if (input$typeBinning == "centers") {
           metaExpr({
             observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
               simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
               censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
               binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
               vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
           })
         } else if (input$typeBinning == "breaks") {
           metaExpr({
             observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
               simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
               censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
               binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
               vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
           })
         } else {
           metaExpr({
             observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
               simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
               censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar))) %>%
               binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
               vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
           })
         }
      #Binning, no strat, no pred correct, no censoring
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && !input$isPred && !input$isCensoring) {
       if (input$typeBinning == "x-variable") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
     } else {
       stop("Must upload data")
     }
  })
   
 })


  
  observeEvent(input$generateCode, {
    code <- expandChain(
      quote({
        library(ggplot2)
        library(vpcstats)
      }),
      fileSim(),
      output$vpccode(), 
      output$plotVPC()
)
    
    displayCodeModal(
      code = code,
      title = "Code to reproduce VPC"
    )
  })

  plotAesthetics <- reactive({
    list(
    linetype = c(input$lineTypeLo, input$lineTypeMed, input$lineTypeHi),
    color = c(input$colorTypeHi, input$colorTypeMed, input$colorTypeLo),
    color.fill = input$colorFill,
    custom.theme = input$themeType,
    show.points = input$showPoints,
    show.boundaries = input$showBoundaries,
    show.stats = input$showStats,
    legend.position = input$legendPosition,
    facet.scales = input$facetScales,
    xlabel = input$xlabel,
    ylabel = input$ylabel,
    qlabel = userQuantiles(),
    facet.scales.type = input$facetScales,
    conf.level = confidenceInterval()
    )
  })
  

  output$tableObs <- renderDataTable({
    datatable(vpc()$stats, rownames = FALSE)
  })
  
  
 vpcPlot <- metaReactive2({
   req(vpc())
   isolate({
   if(length(input$stratvar > 1)) {
     strata <- paste(input$stratvar, collapse = " + ")
     form <- formula(paste0("~", strata))
   } else if(length(input$stratvar == 1)) {
     strata <- input$stratvar
     form <- formula(paste0("~", strata))
   } else {
     form <- NULL
   }
   
   if(input$typeVPC == "Binless") {
    g <- metaExpr({ 
         ggplot(
           vpc()$stats, aes(x=x)) })
    
   } else {
     g <- metaExpr({ 
       ggplot(
         vpc()$stats, aes(x=xbin)) })
   }
   })
     
  if(!is.null(form)) {
    if(input$facetScales == "free") {
     g <- metaExpr({
       ..(g) + 
          facet_wrap(..(form), scales = "free") +
          geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
          geom_line(aes(y=md, col=qname, group=qname)) +
          geom_line(aes(y=y, linetype=qname), size=1) 
     })
  } else {
    g <- metaExpr({
      ..(g) + 
        facet_grid(..(form)) +
        geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
        geom_line(aes(y=md, col=qname, group=qname)) +
        geom_line(aes(y=y, linetype=qname), size=1) 
  })
  }
    } else {
    g
  }
    if(input$isCensoring) {
      if(!is.null(form)) {
     g <- metaExpr({
       ..(g) +
           geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=..(plotAesthetics()$color.fill), col=NA) +
           geom_line(aes(y=md, col=qname, group=qname)) +
           geom_line(aes(y=y, linetype=qname), size=1) +
           geom_hline(data=unique(vpc()$data[, .(..(input$stratvar)), by = eval(..(input$censorvar))]),
                    aes(yintercept= !!as.symbol(..(input$censorvar))), linetype="dotted", size=1) +
           geom_text(data=unique(vpc()$data[, .(ISM, LLOQ)]),
                   aes(x=10, y=LLOQ, label=paste("LLOQ", LLOQ, sep="="),), vjust=-1) +
           scale_colour_manual(
          name=..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
          breaks=..(paste0("q", plotAesthetics()$qlabel)) ,
          values= ..(plotAesthetics()$color),
          labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
         scale_fill_manual(
          name=..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
          breaks=..(paste0("q", plotAesthetics()$qlabel)),
          values=..(plotAesthetics()$color),
          labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
         scale_linetype_manual(
          name=..(paste0("Observed Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
          breaks=..(paste0("q", plotAesthetics()$qlabel)),
          values= ..(plotAesthetics()$linetype),
          labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
         guides(
          fill=guide_legend(order=2),
          colour=guide_legend(order=2),
          linetype=guide_legend(order=1)) +
         theme(
          legend.position=..(plotAesthetics()$legend.position),
          legend.key.width=grid::unit(2, "cm")) +
         labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
    })
      } else {
        g <- metaExpr({
          ..(g) +
            geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=..(plotAesthetics()$color.fill), col=NA) +
            geom_line(aes(y=md, col=qname, group=qname)) +
            geom_line(aes(y=y, linetype=qname), size=1) +
            geom_hline(aes(yintercept= 25), linetype="dotted", size=1) +
            geom_text(aes(x=10, y=25, label=paste("LLOQ", 25, sep="="),), vjust=-1) +
            scale_colour_manual(
              name=..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
              breaks=..(paste0("q", plotAesthetics()$qlabel)) ,
              values= ..(plotAesthetics()$color),
              labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
            scale_fill_manual(
              name=..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
              breaks=..(paste0("q", plotAesthetics()$qlabel)),
              values=..(plotAesthetics()$color),
              labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
            scale_linetype_manual(
              name=..(paste0("Observed Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
              breaks=..(paste0("q", plotAesthetics()$qlabel)),
              values= ..(plotAesthetics()$linetype),
              labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
            guides(
              fill=guide_legend(order=2),
              colour=guide_legend(order=2),
              linetype=guide_legend(order=1)) +
            theme(
              legend.position=..(plotAesthetics()$legend.position),
              legend.key.width=grid::unit(2, "cm")) +
            labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
        })
      }
    } else {
     g <- metaExpr({
       ..(g) + 
         geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=..(plotAesthetics()$color.fill), col=NA) +
        geom_line(aes(y=md, col=qname, group=qname)) +
        geom_line(aes(y=y, linetype=qname), size=1) +
        scale_colour_manual(
          name=..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
          breaks=..(paste0("q", plotAesthetics()$qlabel)) ,
          values= ..(plotAesthetics()$color),
          labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
        scale_fill_manual(
          name=..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
          breaks=..(paste0("q", plotAesthetics()$qlabel)),
          values=..(plotAesthetics()$color),
          labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
        scale_linetype_manual(
          name=..(paste0("Observed Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
          breaks=..(paste0("q", plotAesthetics()$qlabel)),
          values= ..(plotAesthetics()$linetype),
          labels=..(paste0(plotAesthetics()$qlabel * 100, "%"))) +
        guides(
          fill=guide_legend(order=2),
          colour=guide_legend(order=2),
          linetype=guide_legend(order=1)) +
        theme(
          legend.position=..(plotAesthetics()$legend.position),
          legend.key.width=grid::unit(2, "cm")) +
        labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
     })
    }
     
      if(input$showPoints) {
       g <-  isolate(metaExpr({
          ..(g) + ggplot2::geom_point(data=vpc()$obs, ggplot2::aes(x=x, y=y), size=1, alpha=0.4, show.legend=F) 
        }))
      } else {
        g
      }
 })

  output$plotVPC <- metaRender(renderPlot, {
   ..(vpcPlot())
  })
  
  # output$optLambda <- renderTable({
  # t <- as.data.table(vpc()$llam.qpred)
  # setnames(t, "Lambda")
  #   })
  
  
}
#server

options(shiny.maxRequestSize=100*1024^2)

server <- function(input, output, session) {
  
  
  fileObs <- callModule(file_observed_server, "obs")
  fileSim <- callModule(file_simulated_server, "sim")

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
 
 stratlist <- reactive({
   input$stratvar
 })
 
 lamStrat <- reactive({
   req(input$stratvar)
   len <- list(numeric(length(stratlist())))
   # lev <- length(levels(factor(stratlist)))
   for (i in 1:length(stratlist())) {
     len[[i]] <-  fluidRow(
       column(6,
              sliderInput(inputId = paste0('lambdaStratHi0_', i), label = paste0("Lambda Hi 0", i),value = 3, min = 0, max = 7, step = .01),
              sliderInput(inputId = paste0('lambdaStratMed0_', i), label = paste0("Lambda Med 0", i), value = 3, min = 0, max = 7, step = .01),
              sliderInput(inputId = paste0('lambdaStratLo0_', i), label = paste0("Lambda Lo 0", i), value = 3, min = 0, max = 7, step = .01)
       ),
       column(6,                                
              sliderInput(inputId = paste0('lambdaStratHi1_', i), label = paste0("Lambda Hi 1", i),value = 3, min = 0, max = 7, step = .01),
              sliderInput(inputId = paste0('lambdaStratMed1_', i), label = paste0("Lambda Med 1", i), value = 3, min = 0, max = 7, step = .01),
              sliderInput(inputId = paste0('lambdaStratLo1_', i), label = paste0("Lambda Lo 1", i), value = 3, min = 0, max = 7, step = .01)
       )
     )
   }
   len
 })
 
 output$stratLambdas <- renderUI(lamStrat())
 
 userLamStrat <- metaReactive({
   data.table(
     group0 = c(..(input$lambdaStratLo0_1), ..(input$lambdaStratMed0_1), ..(input$lambdaStratHi0_1)),
     group1 = c(..(input$lambdaStratLo1_1), ..(input$lambdaStratMed1_1), ..(input$lambdaStratHi1_1))
   )
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

     vpcUser <- metaExpr({
       observed(..(fileObs()), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
         simulated(..(fileSim()), y= !!rlang::sym(..(input$yvar))) 
     })
       
     if(input$isCensoring) {
       if (input$censorType == "Value") {
         req(input$userLLOQ)
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < ..(input$userLLOQ), lloq = ..(input$userLLOQ))
         })
       } else {
        req(input$censorvar)
        vpcUser <-  metaExpr({
           ..(vpcUser) %>%
             censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar)))
        })
       }
     }
     
     if(!is.null(form)) {
       req(input$stratvar)
       vpcUser <- metaExpr({
         ..(vpcUser) %>%
           stratify(..(form))
       })
     }
     
     if (input$typeVPC == "Binning" && !input$isPred) {
       if (input$typeBinning == "x-variable") {
        vpcUser <- metaExpr({ 
          ..(vpcUser) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
     }
     
     if (input$typeVPC == "Binning" && input$isPred && input$log_dv) {
       if (input$typeBinning == "x-variable") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
     }
     
     if (input$typeVPC == "Binning" && input$isPred && !input$log_dv) {
       if (input$typeBinning == "x-variable") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "centers") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = "centers", centers = ..(as.numeric(unlist(strsplit(input$centers, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else if (input$typeBinning == "breaks") {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = "breaks", breaks = ..(as.numeric(unlist(strsplit(input$breaks, split = ",")))), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       } else {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
             vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
         })
       }
     }
     
     
     if(input$typeVPC == "Binless" && input$isPred) {
       if(input$isAutoOptimize) {
       vpcUser <- metaExpr({
        ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval())) %>%
           vpcstats()
       })
       } else {
         if(!is.null(form)) {
         vpcUser <- metaExpr({
           ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval()), llam.quant = ..(userLamStrat())) %>%
           vpcstats()
         })
         } else {
           vpcUser <- metaExpr({
           ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit(conf.level = ..(confidenceInterval()), llam.quant = ..(binlessInputs()$lamUser), span = ..(binlessInputs()$spanUser)) %>%
           vpcstats() 
           })
         }
       }
     }
       
     if(input$typeVPC == "Binless" && !input$isPred) {
       if(input$isAutoOptimize) {
          vpcUser <- metaExpr({
           ..(vpcUser) %>%
            binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser)) %>%
            binlessfit(conf.level = ..(confidenceInterval())) %>%
            vpcstats()
           })
         } else {
           if(!is.null(form)) {
           vpcUser <- metaExpr({
           ..(vpcUser) %>%
            binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser)) %>%
            binlessfit(conf.level = ..(confidenceInterval()), llam.quant = ..(userLamStrat())) %>%
            vpcstats()
           })
           } else {
           vpcUser <- metaExpr({
           ..(vpcUser) %>%
            binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser)) %>%
            binlessfit(conf.level = ..(confidenceInterval()), llam.quant = ..(binlessInputs()$lamUser)) %>%
            vpcstats() 
           })
         }
       }
     }
   })
   vpcUser
 })


  plotAesthetics <- reactive({
    list(
    linetype = c(input$lineTypeLo, input$lineTypeMed, input$lineTypeHi),
    color = c(input$colorTypeHi, input$colorTypeMed, input$colorTypeLo),
    color.fill = input$colorFill,
    #custom.theme = input$themeType,
    show.points = input$showPoints,
    #show.boundaries = input$showBoundaries,
    #show.stats = input$showStats,
    legend.position = input$legendPosition,
    facet.scales = input$facetScales,
    xlabel = input$xlabel,
    ylabel = input$ylabel,
    qlabel = userQuantiles(),
    facet.scales.type = input$facetScales,
    conf.level = confidenceInterval()
    )
  })
  


 vpcPlot <- metaReactive2({
   req(vpc())
   isolate({
   if(length(input$stratvar > 1)) {
     facet_formula <- paste0("~", paste(input$stratvar, collapse = " + "))
    } else if (length(input$stratvar == 1)) {
     facet_formula <- paste0("~", input$stratvar)
   } else {
     facet_formula <- ""
   }
 
   if(input$typeVPC == "Binless") {
    g <- metaExpr({ 
         ggplot(
           ..(vpc())$stats, aes(x=x)) 
      })
   } else {
    g <- metaExpr({ 
         ggplot(
           ..(vpc())$stats, aes(x=xbin)) 
       })
     }
   })
   
  if(facet_formula != "") {
    if(input$facetQuantile) {
      facet_formula <- as.formula(paste0("qname", facet_formula))
    } else {
      facet_formula <- as.formula(facet_formula)
    }
    if(input$facetScales == "free") {
     g <- metaExpr({
       ..(g) +
          facet_grid(..(facet_formula), scales = "free", as.table = FALSE)
     })
  } else {
    g <- metaExpr({
      ..(g) +
        facet_grid(..(facet_formula), scales = "fixed", as.table = FALSE) 
    })
   }
  }
   
   if(facet_formula == "" && input$facetQuantile) {
       facet_formula <- as.formula(paste0("~", "qname"))
     if(input$facetScales == "free") {
       g <- metaExpr({
         ..(g) +
           facet_wrap(..(facet_formula), scales = "free", as.table = TRUE)
       })
     } else {
       g <- metaExpr({
         ..(g) +
           facet_wrap(..(facet_formula), scales = "fixed", as.table = TRUE)
       })
     }
   }
   
   g <- metaExpr({
     ..(g) + 
     geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=..(plotAesthetics()$color.fill), col=NA) +
     geom_line(aes(y=md, col=qname, group=qname)) +
     geom_line(aes(y=y, linetype=qname), size=1)
   })
   
   
 isolate({
   if(input$isCensoring) {
     if(input$censorType == "Variable")
      if(!is.null(input$stratvar)) {
     g <- metaExpr({
       ..(g) +
           geom_hline(data=unique(..(vpc())$data[, .(LLOQ), by = eval(..(input$stratvar))]),
                    aes(yintercept = !!as.symbol(..(input$censorvar))), linetype="dotted", size=1) +
           geom_text(data=unique(..(vpc())$data[, .(LLOQ), by = eval(..(input$stratvar))]),
                   aes(x=10, y=LLOQ, label=paste("LLOQ", LLOQ, sep="="),), vjust=-1)
       })
      } else {
        g <- metaExpr({
          ..(g) +
            geom_hline(data=unique(..(vpc())$data[, .(LLOQ)]),
                       aes(yintercept = !!as.symbol(..(input$censorvar))), linetype="dotted", size=1) +
            geom_text(data=unique(..(vpc())$data[, .(LLOQ)]),
                      aes(x=10, y=LLOQ, label=paste("LLOQ", LLOQ, sep="="),), vjust=-1)
        })
      } else {
      g <-  metaExpr({
        ..(g) +
           geom_hline(aes(yintercept = ..(input$userLLOQ)), linetype="dotted", size=1) +
           geom_text(aes(x=10, y=..(input$userLLOQ), label=paste("LLOQ", ..(input$userLLOQ), sep="="),), vjust=-1)
      })
     }
    }
   })
 
    if(input$isLogDV) {
       g <-  metaExpr({
        ..(g) + scale_y_log10()
        })
      }
 
    if(input$isLogX) {
       g <-  metaExpr({
        ..(g) + scale_x_log10()
        })
      }
 
     g <- metaExpr({
       ..(g) +
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

   if(!input$showStats && input$typeVPC == "Binning") {
     if(facet_formula != "") {
       if(input$facetScales == "free") {
     g <- isolate(metaExpr({
       ggplot2::ggplot(vpc()$strat) +
         facet_wrap(..(form), scales = "free") +
         theme(
           legend.position=..(plotAesthetics()$legend.position),
           legend.key.width=grid::unit(2, "cm")) +
         labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
     }))
       } else {
         g <- isolate(metaExpr({
           ggplot2::ggplot(vpc()$strat) +
             facet_grid(..(facet_formula)) +
             theme(
               legend.position=..(plotAesthetics()$legend.position),
               legend.key.width=grid::unit(2, "cm")) +
             labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
   }))
       }
     } else {
       g <- isolate(metaExpr({
         ggplot2::ggplot(vpc()$strat) +
           theme(
             legend.position=..(plotAesthetics()$legend.position),
             legend.key.width=grid::unit(2, "cm")) +
           labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
     }))
     }
   }

      if(input$showPoints) {
       g <-  isolate(metaExpr({
          ..(g) + ggplot2::geom_point(data=..(vpc())$obs, ggplot2::aes(x=x, y=y), size=1, alpha=0.4, show.legend=F)
        }))
      } else {
        g
      }

   if (input$showBoundaries) {
     if(is.null(vpc()$rqss.obs.fits)) {
       if (!is.null(vpc()$strat)) {
         boundaries <- isolate(metaExpr({
           bininfo(vpc())[, .(x=sort(unique(c(xleft, xright)))), by=names(vpc()$strat)]
         }))
       } else {
         boundaries <- isolate(metaExpr({
           bininfo(vpc())[, .(x=sort(unique(c(xleft, xright))))]
         }))
       }
       if (input$showBinning) {
         g <- isolate(metaExpr({
           ..(g) + ggplot2::geom_vline(data=boundaries, ggplot2::aes(xintercept=x), size=rel(0.5), col="gray80") +
           ggplot2::theme(panel.grid=ggplot2::element_blank())
         }))
       }
       g <- isolate(metaExpr({
         ..(g) + ggplot2::geom_rug(data=boundaries, ggplot2::aes(x=x), sides="t", size=1)
       }))
     }
   } else {
     g
   }

   g
   
 })
 
 observe({
   if(!input$isCensoring && input$isPlotBlq) {
   showNotification("Please specify censoring value before plotting BLQ", type = "error")
   }
 })
 
 blqPlot <- metaReactive2({
   req(vpc())
   req(input$isCensoring)
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
   })
   
   if(input$isPlotBlq) {
    g <- metaExpr({
        ggplot(
          ..(vpc())$pctblq) +
        geom_ribbon(aes(x = xbin, ymin= lo, ymax = hi), fill = "green2", alpha = .2) + 
        geom_line(aes(x = xbin, y = y)) + 
        labs(x= ..(plotAesthetics()$xlabel), y= "% BLQ")
    })
   }
   
   if(!is.null(form)) {
     g <- metaExpr({
      ..(g) +
     facet_wrap(..(form)) 
   })
   }
   g
   })
  
  
 observeEvent(input$generateCode, {
   code <- expandChain(
     quote({
       library(ggplot2)
       library(vpcstats)
     }),
     output$vpccode(), 
     output$plotVPC()#,
     #output$plotBlq()
   )
   
   displayCodeModal(
     code = code,
     title = "Code to reproduce VPC"
   )
 })

  output$plotVPC <- metaRender(renderPlot, {
    ..(vpcPlot())
  })
  
  output$plotBlq <- metaRender(renderPlot, {
    ..(blqPlot())
  })
  
  output$vpccode <-  metaRender(renderPrint,{
    ..(vpc())
  })
  
  output$tableObs <- renderDataTable({
    datatable(vpc()$stats, rownames = FALSE) %>%
      formatRound(c("y", "lo", "md", "hi"), digits = 2)
  })
  
  # output$optLambda <- renderTable({
  # l <- as.data.table(rev(vpc()$llam.qpred))
  # setnames(l, "Lambda")
  #   })
  
  #spanOut <- 
  # output$optSpan <- renderTable({
  #   s <- as.data.table(vpc()$span)
  #   setnames(s, "Span")
  # })
  
  
}
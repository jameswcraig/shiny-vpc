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
    
  })

 binlessInputs <- callModule(binless_inputs, "binlessInputs1")
 
 userQuantiles <- callModule(quantiles_server, "qpred1")
  
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
          binlessfit() %>%
          vpcstats()
    })
    #strat, optimized, nopredcorrect
    } else if (input$typeVPC == "Binless" && input$isAutoOptimize && !is.null(input$stratvar) && !input$isPred) {
      metaExpr({
        observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
          simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
          stratify(..(form)) %>%
          binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = FALSE) %>%
          binlessfit() %>%
          vpcstats()
      })
    } else if(input$typeVPC == "Binless" && input$isAutoOptimize && is.null(input$stratvar) && input$isPred) {
       #nostrat, optimized, predcorrect
      metaExpr({
        observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit() %>%
           vpcstats()
      })
      #nostrat, optimized, no predcorrect
     } else if(input$typeVPC == "Binless" && input$isAutoOptimize && is.null(input$stratvar) && !input$isPred) {
       metaExpr({
       observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
           simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
           binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
           binlessfit() %>%
           vpcstats()
       })
      #nostrat, no optimized, no predcorrect
     } else if (input$typeVPC == "Binless" && !input$isAutoOptimize  && is.null(input$stratvar) && !input$isPred) {
        metaExpr({
          observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
            simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
            binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
            binlessfit(llam.quant = ..(binlessInputs()$lamUser)) %>%
            vpcstats()
        })
      #nostrat no optimized, predcorrect
     } else if (input$typeVPC == "Binless" && !input$isAutoOptimize  && is.null(input$stratvar) && input$isPred) {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)), y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
             binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
             binlessfit(llam.quant = ..(binlessInputs()$lamUser), span = ..(binlessInputs()$spanUser)) %>%
             vpcstats()
         })
    #Binning, strat, no pred correct
     } else if (input$typeVPC == "Binning" && !is.null(input$stratvar) && !input$isPred ) {
       if(input$typeBinning == "x-variable") {
          metaExpr({
            observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
            simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
            stratify(..(form)) %>%
            binning(!!rlang::sym(..(input$xvar))) %>%
            vpcstats()
        })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
             simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
             stratify(..(form)) %>%
             binning(bin = ..(input$typeBinning), nbins = ..(input$nbins), xbin = ..(input$midPoint)) %>%
             vpcstats()
       })
       }
    #Binning, strat, pred correct
     } else if (input$typeVPC == "Binning" && !is.null(input$stratvar) && input$isPred ) {
          metaExpr({
            observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
            simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
            stratify(..(form)) %>%
            binning(!!rlang::sym(..(input$xvar))) %>%
            predcorrect(!!rlang::sym(..(input$predvar))) %>%
            vpcstats()
        })
       #Binning, no strat, no pred correct
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && !input$isPred) {
          metaExpr({
            observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
            simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
            binning(!!rlang::sym(..(input$xvar))) %>%
            vpcstats()
        })
       #Binning, no strat, pred correct
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && input$isPred) {
          metaExpr({
            observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
            simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
            binning(!!rlang::sym(..(input$xvar))) %>%
            predcorrect(!!rlang::sym(..(input$predvar))) %>%
            vpcstats()
        })
       #Binning ntile, no strat, no prediction corrected
     } else if (input$typeVPC == "Binning" && is.null(input$stratvar) && !input$isPred) {
       if (input$typeBinning == "x-variable") {
          metaExpr({
            observed(fileObs(), x= !!rlang::sym(..(input$xvar)),  y= !!rlang::sym(..(input$yvar))) %>%
            simulated(fileSim(), y= !!rlang::sym(..(input$yvar))) %>%
              binning(!!rlang::sym(..(input$xvar))) %>%
              predcorrect(!!rlang::sym(..(input$predvar))) %>%
            vpcstats()
        })
       } else {
         metaExpr({
           observed(fileObs(), x= !!rlang::sym(input$xvar),  y= !!rlang::sym(input$yvar)) %>%
             simulated(fileSim(), y= !!rlang::sym(input$yvar)) %>%
             binning(bin = input$typeBinning, nbins = input$nbins, xbin = input$midPoint) %>%
             predcorrect(!!rlang::sym(input$predvar)) %>%
             vpcstats()
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
    custom.theme = input$themeType,
    show.points = input$showPoints,
    show.boundaries = input$showBoundaries,
    show.stats = input$showStats,
    legend.position = input$legendPosition,
    facet.scales = input$facetScales,
    xlabel = input$xlabel,
    ylabel = input$ylabel,
    qlabel = userQuantiles(),
    facet.scales.type = input$facetScales
    )
  })
  

  output$tableObs <- renderDataTable({
    datatable(vpc()$obs, rownames = FALSE)
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
       ..(g) + facet_wrap(..(form), scales = "free") +
         geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
         geom_line(aes(y=md, col=qname, group=qname)) +
         geom_line(aes(y=y, linetype=qname), size=1) 
     })
  } else {
    g <- metaExpr({
      ..(g) + facet_grid(..(form)) +
        geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
        geom_line(aes(y=md, col=qname, group=qname)) +
        geom_line(aes(y=y, linetype=qname), size=1) 
  })
  }
    } else {
    g
  }
    
     g <- metaExpr({
       ..(g) +
           geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
           geom_line(aes(y=md, col=qname, group=qname)) +
           geom_line(aes(y=y, linetype=qname), size=1) +
        # geom_hline(data=unique(exampleobs[, .(ISM, LLOQ)]),
        #            aes(yintercept=LLOQ), linetype="dotted", size=1) +
        # geom_text(data=unique(exampleobs[, .(ISM, LLOQ)]),
        #           aes(x=10, y=LLOQ, label=paste("LLOQ", LLOQ, sep="="),), vjust=-1) +
        scale_colour_manual(
          name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
          breaks=c("q0.05", "q0.5", "q0.95"),
          values= ..(plotAesthetics()$color),
          labels=..(plotAesthetics()$qlabel)) +
        scale_fill_manual(
          name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
          breaks=c("q0.05", "q0.5", "q0.95"),
          values=..(plotAesthetics()$color),
          labels=..(plotAesthetics()$qlabel)) +
        scale_linetype_manual(
          name="Observed Percentiles\n(black lines)",
          breaks=c("q0.05", "q0.5", "q0.95"),
          values= ..(plotAesthetics()$linetype),
          labels=..(plotAesthetics()$qlabel)) +
        guides(
          fill=guide_legend(order=2),
          colour=guide_legend(order=2),
          linetype=guide_legend(order=1)) +
        theme(
          legend.position="top",
          legend.key.width=grid::unit(2, "cm")) +
        labs(x= ..(plotAesthetics()$xlabel), y= ..(plotAesthetics()$ylabel))
    })
   
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
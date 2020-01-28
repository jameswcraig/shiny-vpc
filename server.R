#server

options(shiny.maxRequestSize=10000*1024^2)

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
 
 stratdata <- reactive({
   subset(fileObs(), select = stratlist())
 })
 
 output$stratTable <- renderTable({
   lapply(stratdata(), unique)
 })
 
 stratnamelvl <- reactive({
   l <- lapply(stratdata(), unique)
   stratlevels <- as.character(unlist(l))
   stratname <- names(unlist(l))
   stratname <- gsub('[[:digit:]]+', '', stratname)
   name <- sort(paste0(stratname, stratlevels))
   return(name)
 })
 
 userStratNames <- reactive({
   dt <- vector("list", length = length(stratnamelvl()))
   for (i in seq_along(stratnamelvl())) {
     dt[[i]] <- eval(parse(text = paste0("input$", stratnamelvl()[[i]])))
   }
   dt <- as.data.table(dt)
   setnames(dt, stratnamelvl())
 })
 
 
 userStratLvl <- metaReactive({
   as.data.table(..(lapply(userStratNames(), getnum)))
 })
 
 
 output$lamTable <- renderTable({
   req(userStratNames())
   setnames(userStratNames(), stratnamelvl())
 })
 
 
 
 t <- reactive({
   req(input$stratvar)
   l <- render_lam_strat(stratlist(), stratdata())
 })
 
 tt <- reactive({
   req(input$stratvar)
   ll <- render_strat_binning(stratlist(), stratdata())
 })
 
 
 output$stratpanels <- renderUI({
   tagList(
     tags$h5("Select Binning Method by Strata"),
     tt())
 })
 
 output$stratLambdas <- renderUI({
   tagList(
     tags$h5("Select Lambda Smoothing by Strata"),
     t(),
     tags$h6("(Lower, Median, Upper)")
   )
 })
 # userLamStrat <- metaReactive({
 #   data.table(
 #     group0 = c(..(input$lambdaStratLo0_1), ..(input$lambdaStratMed0_1), ..(input$lambdaStratHi0_1)),
 #     group1 = c(..(input$lambdaStratLo1_1), ..(input$lambdaStratMed1_1), ..(input$lambdaStratHi1_1))
 #   )
 # })
 
 stratdata <- reactive({
   subset(fileObs(), select = input$stratvar)
 })

 namesstrat <- reactive({
   names(stratdata())
 })
 
 stratlvl <- reactive({
   l <- lapply(stratdata(), unique)
   l <- lapply(l, sort)
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
     
     
     if (input$typeVPC == "Binning" && !input$isBinStrat)  {
      if(input$typeBinning == "x-variable")
        vpcUser <- metaExpr({
         ..(vpcUser) %>%
           binning(bin =!!rlang::sym(..(input$xvar)))
        })
      
     if (input$typeBinning == "centers") {
      centers <- as.numeric(unlist(strsplit(input$centers, split = ",")))
       vpcUser <- metaExpr({
         ..(vpcUser) %>%
           binning(bin = "centers", centers = ..(centers))
     })
     }
      
      if(input$typeBinning == "breaks") {
      breaks <- as.numeric(unlist(strsplit(input$breaks, split = ",")))
      vpcUser <- metaExpr({
        ..(vpcUser) %>%
          binning(bin = "breaks", breaks = ..(breaks))
      })
     }
      
      if(input$typeBinning == "breaks") {
        breaks <- as.numeric(unlist(strsplit(input$breaks, split = ",")))
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "breaks", breaks = ..(breaks))
        })
      }
    
      if(is_false(input$typeBinning %in% c("breaks","centers","x-variable"))) {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = ..(input$typeBinning), nbins = ..(input$nbins))
      })
      }
     }
         
         
     
     
     #Different binning by strata
     if (input$typeVPC == "Binning" && input$isBinStrat && !is.null(form)) { # && !input$isPred && input$isBinStrat && !is.null(form)) {
       #1 Strat variable, 2 level
        if (length(stratlvl()[[1]]) == 2) {
         b1 <- input$typeBinning1
         b2 <- input$typeBinning2
         l1 <- list(stratlvl()[[1]][[1]])
         l2 <- list(stratlvl()[[1]][[2]])
         names(l1) <- namesstrat()
         names(l2) <- namesstrat()
         if(b1 == "centers") {
           centers1 <- as.numeric(unlist(strsplit(input$centers1, split = ",")))
         } else {
           centers1 <- NULL
         }
         if(b1 == "breaks") {
           breaks1 <- as.numeric(unlist(strsplit(input$breaks1, split = ",")))
         } else {
           breaks1 <- NULL
         }
         if(b2 == "centers") {
           centers2 <- as.numeric(unlist(strsplit(input$centers2, split = ",")))
         } else {
           centers2 <- NULL
         }
         if(b2 == "breaks") {
           breaks2 <- as.numeric(unlist(strsplit(input$breaks2, split = ",")))
         } else {
           breaks2 <- NULL
         }
         
         if(b1 == "x-variable" && b2!="x-variable") {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l1), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T) %>%
               binning(stratum = ..(l2), bin = ..(b2), xbin = ..(input$midPoint), nbins = ..(input$nbins2), centers = ..(centers2), breaks = ..(breaks2), by.strata = T) 
           })
         } else if(b1 != "x-variable" && b2 =="x-variable") {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l1), bin = ..(b1), xbin = ..(input$midPoint), nbins = ..(input$nbins1), centers = ..(centers1), breaks = ..(breaks1), by.strata = T) %>%
               binning(stratum = ..(l2), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T) 
           })
         } else if(b1 == "x-variable" && b2 =="x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l1), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T) %>%
                 binning(stratum = ..(l2), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T) 
             })
         } else {
         vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
             binning(stratum = ..(l1), bin = ..(b1), xbin = ..(input$midPoint), nbins = ..(input$nbins1), centers = ..(centers1), breaks = ..(breaks1), by.strata = T) %>%
             binning(stratum = ..(l2), bin = ..(b2), xbin = ..(input$midPoint), nbins = ..(input$nbins2), centers = ..(centers2), breaks = ..(breaks2), by.strata = T) 
         })
         }
        }
         
        if (length(stratlvl()[[1]]) == 3) {
         b1 <- input$typeBinning1
         b2 <- input$typeBinning2
         b3 <- input$typeBinning3
         l1 <- list(stratlvl()[[1]][[1]])
         l2 <- list(stratlvl()[[1]][[2]])
         l3 <- list(stratlvl()[[1]][[3]])
         names(l1) <- namesstrat()
         names(l2) <- namesstrat()
         names(l3) <- namesstrat()
         if(b1 == "centers") {
           centers1 <- as.numeric(unlist(strsplit(input$centers1, split = ",")))
         } else {
           centers1 <- NULL
         }
         if(b1 == "breaks") {
           breaks1 <- as.numeric(unlist(strsplit(input$breaks1, split = ",")))
         } else {
           breaks1 <- NULL
         }
         if(b2 == "centers") {
           centers2 <- as.numeric(unlist(strsplit(input$centers2, split = ",")))
         } else {
           centers2 <- NULL
         }
         if(b2 == "breaks") {
           breaks2 <- as.numeric(unlist(strsplit(input$breaks2, split = ",")))
         } else {
           breaks2 <- NULL
         }
         if(b3 == "centers") {
           centers3 <- as.numeric(unlist(strsplit(input$centers3, split = ",")))
         } else {
           centers3 <- NULL
         }
         if(b3 == "breaks") {
           breaks3 <- as.numeric(unlist(strsplit(input$breaks3, split = ",")))
         } else {
           breaks3 <- NULL
         }
         
         if(b1 == "x-variable") {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l1), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
           })
         } else {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l1), bin = ..(b1), xbin = ..(input$midPoint), nbins = ..(input$nbins1), centers = ..(centers1), breaks = ..(breaks1), by.strata = T)           
             })
         }
         
         if(b2 == "x-variable") {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l2), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
           })
         } else {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l2), bin = ..(b2), xbin = ..(input$midPoint), nbins = ..(input$nbins2), centers = ..(centers2), breaks = ..(breaks2), by.strata = T)           
           })
         }
         
         if(b3 == "x-variable") {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l3), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
           })
         } else {
           vpcUser <- metaExpr({ 
             ..(vpcUser) %>%
               binning(stratum = ..(l3), bin = ..(b3), xbin = ..(input$midPoint), nbins = ..(input$nbins3), centers = ..(centers3), breaks = ..(breaks3), by.strata = T)           
           })
         }
        }
         if (length(stratlvl()[[1]]) == 4) {
           b1 <- input$typeBinning1
           b2 <- input$typeBinning2
           b3 <- input$typeBinning3
           b4 <- input$typeBinning4
           l1 <- list(stratlvl()[[1]][[1]])
           l2 <- list(stratlvl()[[1]][[2]])
           l3 <- list(stratlvl()[[1]][[3]])
           l4 <- list(stratlvl()[[1]][[4]])
           names(l1) <- namesstrat()
           names(l2) <- namesstrat()
           names(l3) <- namesstrat()
           names(l4) <- namesstrat()
           if(b1 == "centers") {
             centers1 <- as.numeric(unlist(strsplit(input$centers1, split = ",")))
           } else {
             centers1 <- NULL
           }
           if(b1 == "breaks") {
             breaks1 <- as.numeric(unlist(strsplit(input$breaks1, split = ",")))
           } else {
             breaks1 <- NULL
           }
           if(b2 == "centers") {
             centers2 <- as.numeric(unlist(strsplit(input$centers2, split = ",")))
           } else {
             centers2 <- NULL
           }
           if(b2 == "breaks") {
             breaks2 <- as.numeric(unlist(strsplit(input$breaks2, split = ",")))
           } else {
             breaks2 <- NULL
           }
           if(b3 == "centers") {
             centers3 <- as.numeric(unlist(strsplit(input$centers3, split = ",")))
           } else {
             centers3 <- NULL
           }
           if(b3 == "breaks") {
             breaks3 <- as.numeric(unlist(strsplit(input$breaks3, split = ",")))
           } else {
             breaks3 <- NULL
           }
           if(b4 == "centers") {
             centers4 <- as.numeric(unlist(strsplit(input$centers4, split = ",")))
           } else {
             centers4 <- NULL
           }
           if(b4 == "breaks") {
             breaks4 <- as.numeric(unlist(strsplit(input$breaks4, split = ",")))
           } else {
             breaks4 <- NULL
           }
           
           if(b1 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l1), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l1), bin = ..(b1), xbin = ..(input$midPoint), nbins = ..(input$nbins1), centers = ..(centers1), breaks = ..(breaks1), by.strata = T)           
             })
           }
           
           if(b2 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l2), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l2), bin = ..(b2), xbin = ..(input$midPoint), nbins = ..(input$nbins2), centers = ..(centers2), breaks = ..(breaks2), by.strata = T)           
             })
           }
           
           if(b3 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l3), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l3), bin = ..(b3), xbin = ..(input$midPoint), nbins = ..(input$nbins3), centers = ..(centers3), breaks = ..(breaks3), by.strata = T)           
             })
           }
           
           if(b4 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l4), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l4), bin = ..(b4), xbin = ..(input$midPoint), nbins = ..(input$nbins4), centers = ..(centers4), breaks = ..(breaks4), by.strata = T)           
             })
           }
         }
       
         if (length(stratlvl()[[1]]) == 5) {
           b1 <- input$typeBinning1
           b2 <- input$typeBinning2
           b3 <- input$typeBinning3
           b4 <- input$typeBinning4
           b5 <- input$typeBinning5
           l1 <- list(stratlvl()[[1]][[1]])
           l2 <- list(stratlvl()[[1]][[2]])
           l3 <- list(stratlvl()[[1]][[3]])
           l4 <- list(stratlvl()[[1]][[4]])
           l5 <- list(stratlvl()[[1]][[5]])
           names(l1) <- namesstrat()
           names(l2) <- namesstrat()
           names(l3) <- namesstrat()
           names(l4) <- namesstrat()
           names(l5) <- namesstrat()
           if(b1 == "centers") {
             centers1 <- as.numeric(unlist(strsplit(input$centers1, split = ",")))
           } else {
             centers1 <- NULL
           }
           if(b1 == "breaks") {
             breaks1 <- as.numeric(unlist(strsplit(input$breaks1, split = ",")))
           } else {
             breaks1 <- NULL
           }
           if(b2 == "centers") {
             centers2 <- as.numeric(unlist(strsplit(input$centers2, split = ",")))
           } else {
             centers2 <- NULL
           }
           if(b2 == "breaks") {
             breaks2 <- as.numeric(unlist(strsplit(input$breaks2, split = ",")))
           } else {
             breaks2 <- NULL
           }
           if(b3 == "centers") {
             centers3 <- as.numeric(unlist(strsplit(input$centers3, split = ",")))
           } else {
             centers3 <- NULL
           }
           if(b3 == "breaks") {
             breaks3 <- as.numeric(unlist(strsplit(input$breaks3, split = ",")))
           } else {
             breaks3 <- NULL
           }
           if(b4 == "centers") {
             centers4 <- as.numeric(unlist(strsplit(input$centers4, split = ",")))
           } else {
             centers4 <- NULL
           }
           if(b4 == "breaks") {
             breaks4 <- as.numeric(unlist(strsplit(input$breaks4, split = ",")))
           } else {
             breaks4 <- NULL
           }
           if(b5 == "centers") {
             centers5 <- as.numeric(unlist(strsplit(input$centers5, split = ",")))
           } else {
             centers5 <- NULL
           }
           if(b5 == "breaks") {
             breaks5 <- as.numeric(unlist(strsplit(input$breaks5, split = ",")))
           } else {
             breaks5 <- NULL
           }
           
           if(b1 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l1), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l1), bin = ..(b1), xbin = ..(input$midPoint), nbins = ..(input$nbins1), centers = ..(centers1), breaks = ..(breaks1), by.strata = T)           
             })
           }
           
           if(b2 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l2), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l2), bin = ..(b2), xbin = ..(input$midPoint), nbins = ..(input$nbins2), centers = ..(centers2), breaks = ..(breaks2), by.strata = T)           
             })
           }
           
           if(b3 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l3), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l3), bin = ..(b3), xbin = ..(input$midPoint), nbins = ..(input$nbins3), centers = ..(centers3), breaks = ..(breaks3), by.strata = T)           
             })
           }
           
           if(b4 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l4), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l4), bin = ..(b4), xbin = ..(input$midPoint), nbins = ..(input$nbins4), centers = ..(centers4), breaks = ..(breaks4), by.strata = T)           
             })
           }
           
           if(b5 == "x-variable") {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l5), bin = !!rlang::sym(..(input$xvar)), xbin = ..(input$midPoint), by.strata = T)
             })
           } else {
             vpcUser <- metaExpr({ 
               ..(vpcUser) %>%
                 binning(stratum = ..(l5), bin = ..(b5), xbin = ..(input$midPoint), nbins = ..(input$nbins5), centers = ..(centers5), breaks = ..(breaks5), by.strata = T)           
             })
           }
         }
       }
     
     if (input$typeVPC == "Binning" && input$isPred && !input$log_dv) {
       vpcUser <- metaExpr({
         ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar)))
       })
     }
     
     if (input$typeVPC == "Binning" && input$isPred && input$log_dv) {
         vpcUser <- metaExpr({
           ..(vpcUser) %>%
             predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE)
         })
       }
     
     if (input$typeVPC == "Binning") {
       vpcUser <- metaExpr({ 
           ..(vpcUser) %>%
              vpcstats(qpred = ..(userQuantiles()), conf.level = ..(confidenceInterval()))
     })
     }
     

     if(input$typeVPC == "Binless" && input$isPred) {
       if(input$isAutoOptimize) {
       # vpcUser <- metaExpr({
       #  ..(vpcUser) %>%
       #     predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
       #     binlessaugment(qpred = ..(userQuantiles()), interval = ..(binlessInputs()$intervalUser), loess.ypc = TRUE) %>%
       #     binlessfit(conf.level = ..(confidenceInterval())) %>%
       #     vpcstats()
       vpcUser <- metaExpr({
        ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>% 
           binless(qpred = ..(userQuantiles()), optimize = TRUE, optimization.interval = ..(binlessInputs()$intervalUser), conf.level = ..(confidenceInterval()), loess.ypc = TRUE) %>%
           vpcstats()
       })
       } else {
         if(!is.null(form)) {
         vpcUser <- metaExpr({
           ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binless(qpred = ..(userQuantiles()), optimize = FALSE, conf.level = ..(confidenceInterval()), lambda = ..(userStratLvl()), span = NULL, loess.ypc = TRUE) %>%
           vpcstats()
         })
         } else {
           vpcUser <- metaExpr({
           ..(vpcUser) %>%
           predcorrect(pred = !!rlang::sym(..(input$predvar))) %>%
           binless(qpred = ..(userQuantiles()), optimize = FALSE, conf.level = ..(confidenceInterval()), lambda = ..(binlessInputs()$lamUser), span = ..(binlessInputs()$spanUser), loess.ypc = TRUE) %>%
           vpcstats()
           })
         }
       }
     }

     if(input$typeVPC == "Binless" && !input$isPred) {
       if(input$isAutoOptimize) {
          vpcUser <- metaExpr({
           ..(vpcUser) %>%
            binless(qpred = ..(userQuantiles()), optimize = TRUE, optimization.interval = ..(binlessInputs()$intervalUser), conf.level = ..(confidenceInterval())) %>%
            vpcstats()
           })
         } else {
           if(!is.null(form)) {
           vpcUser <- metaExpr({
           ..(vpcUser) %>%
            binless(qpred = ..(userQuantiles()), optimize = FALSE, conf.level = ..(confidenceInterval()), lambda = ..(userStratLvl())) %>%
            vpcstats()
           })
           } else {
           vpcUser <- metaExpr({
           ..(vpcUser) %>%
            binless(qpred = ..(userQuantiles()), optimize = FALSE, conf.level = ..(confidenceInterval()), lambda = ..(binlessInputs()$lamUser)) %>%
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
 
     g <- isolate({
       metaExpr({
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
           bininfo(..(vpc()))[, .(x=sort(unique(c(xleft, xright)))), by=names(..(vpc())$strat)]
         }))
       } else {
         boundaries <- isolate(metaExpr({
           bininfo(..(vpc()))[, .(x=sort(unique(c(xleft, xright))))]
         }))
       }
       if (input$showBinning) {
         g <- isolate(metaExpr({
           ..(g) + ggplot2::geom_vline(data=..(boundaries), ggplot2::aes(xintercept=x), size=rel(0.5), col="gray80") +
           ggplot2::theme(panel.grid=ggplot2::element_blank())
         }))
       }
       g <- isolate(metaExpr({
         ..(g) + ggplot2::geom_rug(data=..(boundaries), ggplot2::aes(x=x), sides="t", size=1)
       }))
     }
   } else {
     g
   }

   g
   
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
     if(input$typeVPC == "Binning") {
    g <- metaExpr({
        ggplot(
          ..(vpc())$pctblq) +
        geom_ribbon(aes(x = xbin, ymin= lo, ymax = hi), fill = "green2", alpha = .2) + 
        geom_line(aes(x = xbin, y = y)) + 
        labs(x= ..(plotAesthetics()$xlabel), y= "% BLQ")
    })
     }
     if(input$typeVPC == "Binless") {
       g <- metaExpr({
         ggplot(
           ..(vpc())$pctblq) +
           geom_ribbon(aes(x = x, ymin= lo, ymax = hi), fill = "green2", alpha = .2) + 
           geom_line(aes(x = x, y = y)) + 
           labs(x= ..(plotAesthetics()$xlabel), y= "% BLQ")
       })
     }
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
     output$plotVPC()
   )
   
   displayCodeModal(
     code = code,
     title = "Code to reproduce VPC"
   )
 })

  output$plotVPC <- metaRender(renderPlot, {
    ..(vpcPlot())
  }, height = function() {
    session$clientData$output_plotVPC_width * .6
    }, 
     width = function() {
       session$clientData$output_plotVPC_width
     },
  execOnResize = TRUE)
  
  output$plotBlq <- metaRender(renderPlot, {
    ..(blqPlot())
  })
  
  output$vpccode <-  metaRender(renderPrint,{
    ..(vpc())
  })
  
  output$tableObs <- renderDataTable({
    datatable(vpc()$stats, rownames = FALSE, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#0a7bc1', 'color': '#FFFFFF'});",
        "}")
    )) %>%
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
  
  #Notifications
  observe({
    if(!input$isCensoring && input$isPlotBlq) {
      showNotification("Please select censoring value before plotting BLQ", type = "error")
    }
  })

  observe({
    if(is.null(input$stratvar) && input$isBinStrat) {
      showNotification("Please select stratfication variable before binning by strat.", type = "error")
    }
  })
  
  observe({
    if(input$isBinStrat && length(input$stratvar) > 1) {
      showNotification("Bin by strata limited to one stratification variable.", type = "error")
    }
  })
  
  
}
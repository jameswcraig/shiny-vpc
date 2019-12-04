#' Slider module server-side processing
#' 
#' @param input
#' @param output
#' @param session
#' 
#' @return list with the following components:
#' \describe{
#'   \item{llam}{reactive slider for lambda selection}
#'   }
#'
#'


binless_par_ui <- function(id, label = "Lambda (Binless Fit)") {
  ns <- NS(id)
  tagList(
    fluidPage(
    fluidRow(
      column(6,
        sliderInput(ns("hi"), "High",  value = .9, min = 0, max =  1, step = 0.05),
        sliderInput(ns("med"), "Med", value = .5, min = 0, max =  1, step = 0.05),
        sliderInput(ns("lo"), "Low", value = .1, min = 0, max =  1, step = 0.05),
        verbatimTextOutput(ns("qout"))
      ),
      column(6,
        sliderInput(ns("llam_slider_lo"), "Lambda High", 0, 7, 0),
        sliderInput(ns("llam_slider_med"), "Lambda Med", 0, 7, 0),
        sliderInput(ns("llam_slider_hi"), "Lambda Low", 0, 7, 0),
        verbatimTextOutput(ns("llamout"))
      )
    )
  )
  )
    
    
    
  
}

#Server

binless_par <- function(input, output, session){
 llam <- reactive({
   c(input$llam_slider_lo, input$llam_slider_med, input$llam_slider_hi)
   })
 output$llamout <- renderText({
   llam()
 })
 
 quantiles <- reactive({
   c(input$lo, input$med, input$hi)
 })
 
 output$qout <- renderText({
   quantiles()
 })
 
 par <- reactive({
   list(quant = quantiles(), lambda = llam())
 })
 
 return(par())
}



#' Plot module server-side processing
#' 
#' @param input
#' @param output
#' @param session
#' @param vpc vpc object
#' 
#' @return list with the following components:
#' \describe{
#'   \item{llam}{reactive slider for lambda selection}
#'   }
#'
#'



# Server call
# vpc.Binless <- reactive({
#   callModule(vpc_binless, "plotVPC",
#              obs = fileObs, 
#              sim = fileSim,
#              piUser = piUser,
#              lamUser = lamUser,
#              optLam = input$isOptLambda,
#              spanUser = spanUser,
#              x = reactive({input$xvar}),
#              y = reactive({input$yvar}),
#              pred = reactive({input$predvar}),
#              stratvar = reactive({input$stratvar})
#   )
# })

vpc_binless_ui <- function(id) {
  ns <- NS(id)
  
  actionButton(ns("buttonPlot"), label = "Plot VPC")
}


vpc_binless <- function(input, output, session, obs, sim, piUser, lamUser, optLam, spanUser, x, y, pred, stratvar) {
  
  stratformula <- reactive({
    if (!is.null(stratvar())) {
      as.formula(paste0("~ ", stratvar()))
    } else {
      NULL
    }
  })
  
  vpc <- eventReactive(input$buttonPlot, {
    if(!is.null(stratformula())) {
    observed(obs(), x= !!rlang::sym(x()), y= !!rlang::sym(y())) %>%
    simulated(sim(), y= !!rlang::sym(y())) %>%
    stratify(stratformula()) %>%
    predcorrect(pred = !!rlang::sym(pred())) %>%
    binlessaugment(qpred = piUser(), loess.ypc = TRUE) %>%
    binlessfit() %>%
    binlessvpcstats()
    } else if(optLam) {
      observed(obs(), x= !!rlang::sym(x()), y= !!rlang::sym(y())) %>%
        simulated(sim(), y= !!rlang::sym(y())) %>%
        predcorrect(pred = !!rlang::sym(pred())) %>%
        binlessaugment(qpred = piUser(), loess.ypc = TRUE) %>%
        binlessfit() %>%
        binlessvpcstats()
    } else {
      observed(obs(), x= !!rlang::sym(x()), y= !!rlang::sym(y())) %>%
        simulated(sim(), y= !!rlang::sym(y())) %>%
        predcorrect(pred = !!rlang::sym(pred())) %>%
        binlessaugment(qpred = piUser(), loess.ypc = TRUE) %>%
        binlessfit(llam.qpred = lamUser(), span = spanUser()) %>%
        binlessvpcstats()
    }
})
  # } else {
  #   vpc <- eventReactive(input$buttonPlot, {
  #     observed(obs(), x= !!rlang::sym(x()), y= !!rlang::sym(y())) %>%
  #       simulated(sim(), y= !!rlang::sym(y())) %>%
  #       predcorrect(pred = !!rlang::sym(pred())) %>%
  #       binlessaugment(qpred = piUser(), loess.ypc = TRUE) %>%
  #       binlessfit() %>%
  #       binlessvpcstats()
  #   })
  # }
   
   return(vpc())
}

#' Plot module server-side processing
#' 
#' @param input
#' @param output
#' @param session
#' @param vpc vpc object
#' 
#' @return list with the following components:
#' \describe{
#'   \item{llam}{reactive slider for lambda selection}
#'   }
#'
#'

plot_binless_ui <- function(id) {
  ns <- NS(id)
  
  withSpinner(plotOutput(ns("plotVPC")), type = 8)

  
}

#Server

plot_binless <- function(input, output, session, vpc){
 
  stratformula <- reactive({
  if(!is.null(vpc()$strat)) {
    vpc()$strat.formula
  }
  })
  
  output$plotVPC <- renderPlot({
    if(!is.null(vpc()$strat)) {
    ggplot(vpc()$stats, aes(x=x)) +
      facet_grid(stratformula()) +
      geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
      geom_line(aes(y=med, col=qname, group=qname)) +
      geom_line(aes(y=y, linetype=qname), size=1) +
      geom_point(aes(y = l.ypc, x = x), data = vpc()$obs)  +
      scale_colour_manual(
        name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
        breaks=c("q0.1", "q0.5", "q0.9"),
        values=c("red", "blue", "red"),
        labels=c("5%", "50%", "95%")) +
      scale_fill_manual(
        name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
        breaks=c("q0.1", "q0.5", "q0.9"),
        values=c("red", "blue", "red"),
        labels=c("5%", "50%", "95%")) +
      scale_linetype_manual(
        name="Observed Percentiles\n(black lines)",
        breaks=c("q0.1", "q0.5", "q0.9"),
        values=c("dotted", "solid", "dashed"),
        labels=c("10%", "50%", "90%")) +
      guides(
        fill=guide_legend(order=2),
        colour=guide_legend(order=2),
        linetype=guide_legend(order=1)) +
      theme(
        legend.position="top",
        legend.key.width=grid::unit(2, "cm")) +
      labs(x="Time (h)", y="Concentration (ng/mL)")
    } else {
      ggplot(vpc()$stats, aes(x=x)) +
        geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
        geom_line(aes(y=med, col=qname, group=qname)) +
        geom_line(aes(y=y, linetype=qname), size=1) +
        geom_point(aes(y = l.ypc, x = x), data = vpc()$obs)  +
        scale_colour_manual(
          name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
          breaks=c("q0.1", "q0.5", "q0.9"),
          values=c("red", "blue", "red"),
          labels=c("5%", "50%", "95%")) +
        scale_fill_manual(
          name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
          breaks=c("q0.1", "q0.5", "q0.9"),
          values=c("red", "blue", "red"),
          labels=c("5%", "50%", "95%")) +
        scale_linetype_manual(
          name="Observed Percentiles\n(black lines)",
          breaks=c("q0.1", "q0.5", "q0.9"),
          values=c("dotted", "solid", "dashed"),
          labels=c("10%", "50%", "90%")) +
        guides(
          fill=guide_legend(order=2),
          colour=guide_legend(order=2),
          linetype=guide_legend(order=1)) +
        theme(
          legend.position="top",
          legend.key.width=grid::unit(2, "cm")) +
        labs(x="Time (h)", y="Concentration (ng/mL)")
    }
  })
}




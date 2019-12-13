plot_binless_ui <- function(id) {
  ns <- NS(id)
  
  withSpinner(plotOutput(ns("plotVPC"), height = "600px"), type = 8)
  
  
}

#Server

plot_binless <- function(input, output, session, vpc){
  
  require(vpc())
  
  stratformula <- reactive({
    if(!is.null(vpc()$strat)) {
      vpc()$strat.formula
    }
  })
  
  output$plotVPC <- renderPlot({
    if(!is.null(vpc()$strat)) {
     p <- ggplot(vpc()$stats, aes(x=x)) +
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
    } 
    #no strat and predcorrect
    if (is.null(vpc()$strat) && isTRUE(vpc()$loess.ypc)) {
     p <- ggplot(vpc()$stats, aes(x=x)) +
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
    #no strat and nopredcorrect
    if (is.null(vpc()$strat) && is.null(vpc()$loess.ypc)) {
      p <- ggplot(vpc()$stats, aes(x=x)) +
        geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
        geom_line(aes(y=med, col=qname, group=qname)) +
        geom_line(aes(y=y, linetype=qname), size=1) +
        geom_point(aes(y = y, x = x), data = vpc()$obs)  +
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
    
    if (!is.null(vpc()$xbin)) {
      p <- plot(vpc())
    }
    
    p
  })
}
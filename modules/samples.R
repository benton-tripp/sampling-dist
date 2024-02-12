samples.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  samples.env <- environment()
  
  observeEvent(c(input$sampleDatasets, input$triggerSample), {
    s.dt <- get("s.dt", server.env)[[input$sampleDatasets]]
    .dist <- get(".dist", server.env)
    
    # Population Density Plot
    s.plot <- ggplot(data=s.dt, aes(x=y, y=after_stat(density))) +
      geom_histogram(fill="white", color="black")
    if (!(.dist %in% c("Binomial", "Geometric", "Poisson"))) {
      s.plot <- s.plot + geom_density(color="darkblue", 
                                      linetype="dashed", linewidth=1.2)
    }
    s.plot <- s.plot + theme_minimal() +
      labs(title=paste("Density - Sample #", input$sampleDatasets),
           y="Density", x="Sampled y-values")
    
    .round <- ifelse(.dist %in% c("Binomial", "Geometric", "Poisson"), 0, 5)
    output$samp_data <- renderDT(data.preview(s.dt, .round, "Sample Data Preview"))
    
    output$samp_dens <- renderPlot(s.plot)
  }, ignoreInit=T, ignoreNULL=T)
  
}
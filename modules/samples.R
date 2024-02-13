samples.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  samples.env <- environment()
  
  observeEvent(c(input$sampleDatasets, input$triggerSample), {
    s.dt <- get("s.dt", server.env)[[input$sampleDatasets]]
    .dist <- get(".dist", server.env)
    
    # Population Density Plot
    s.plot <- tryCatch({
      .plot <- ggplot(data=s.dt, aes(x=y, y=after_stat(density))) +
        geom_histogram(fill="white", color="black")
      if (!(.dist %in% c("Binomial", "Geometric", "Poisson"))) {
        .plot <- .plot + geom_density(color="darkblue", 
                                      linetype="dashed", linewidth=1.2)
      }
      .plot + theme_minimal() +
        labs(title=paste("Density - Sample #", input$sampleDatasets),
             y="Density", x="Sampled y-values")
    }, error=function(e) NULL)
    
    .round <- ifelse(.dist %in% c("Binomial", "Geometric", "Poisson"), 0, 5)
    output$samp_data <- renderDT({
      tryCatch({data.preview(s.dt, .round, "Sample Data Preview")},
               error=function(e) NULL)
    })
    output$samp_dens <- renderPlot(s.plot)
    output$samp_summary <- renderDT({
      tryCatch({
        get.summary.stats(s.dt) %>%
          summary.table()
      }, error=function(e) NULL)
    })
  }, ignoreInit=T, ignoreNULL=T)
  
}
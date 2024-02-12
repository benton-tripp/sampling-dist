sampling.dist.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  sampling.dist.env <- environment()
  
  observeEvent(input$triggerSamplingDist, {
    s.dt <- get("s.dt", server.env) 
    .n <- get(".n", server.env)
    p.dt <- get("p.dt", server.env)
    
    p.summary <- get.summary.stats(p.dt)
    
    # Sampling distributions of: 
    # "Sample Mean", "Standardized Sample Mean", 
    # "Sample Variance", "Sample Standard Deviation",
    # "Sample Median", "Sample Min.", "Sample Max."
    sd.dt <- purrr::map_df(s.dt, function(s) {
      s.summary <- get.summary.stats(s)
      s.summary[, .stz.mean := get.z(s.summary$.mean, p.summary$.mean, .n)]
      return(s.summary)
    })
    
    summary.stats <- names(sd.dt) %>%
      purrr::set_names() %>%
      purrr::map(
        ~get.summary.stats(sd.dt, .x)
      )
    
    # Create Summary Tables
    
    assign("sd.dt", value=sd.dt, envir=sampling.dist.env)
    
    # Trigger Plot Update
    runjs('Shiny.setInputValue("triggerSamplingDistPlot", Math.random());')
  }, ignoreInit=T, ignoreNULL=T)
  
  observeEvent(c(input$selectStat, input$triggerSamplingDistPlot), {
    sd.dt <- get("sd.dt", sampling.dist.env) 
    .dist <- get(".dist", server.env)
    
    .selected.stat <- case_when(
      input$selectStat=="Sample Mean"~".mean",
      input$selectStat=="Standardized Sample Mean"~".stz.mean",
      input$selectStat=="Sample Variance"~".variance",
      input$selectStat=="Sample Standard Deviation"~".stdev",
      input$selectStat=="Sample Median"~".median",
      input$selectStat=="Sample Min."~".min",
      input$selectStat=="Sample Max."~".max"
    )
    # Sampling distribution of the <sample statistic> plot
    sd.plot <- ggplot(data=sd.dt, aes_string(x=.selected.stat)) +
      geom_histogram(aes(y=after_stat(density)), fill="white", color="black") +
      geom_density(aes(y=after_stat(density)), color="darkgreen", 
                   linetype="dashed", linewidth=1.2) + 
      theme_minimal() +
      labs(title=paste("Sampling Distribution of", input$selectStat),
           y="Density", x="Observed Values")
    output$samp_dist_dens <- renderPlot(sd.plot)
    
    # Data preview
    .round <- ifelse(.dist %in% c("Binomial", "Geometric", "Poisson"), 0, 5)
    output$samp_dist_data <- renderDT({
      data.preview(sd.dt[, .(y=sd.dt[[.selected.stat]])], 
                   .round, "Sampling Dist. Data Preview")
      })
    
    
    js$finishedLoading()
  }, ignoreInit=T, ignoreNULL=T)
}
apply.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  apply.env <- environment()
  
  # On apply button click
  observeEvent(input$apply, {
    if (isolate(input$sampleDatasets) != 1) {
      # Update Sample DS Selection
      updateNumericInput(session, inputId="sampleDatasets", 
                         value=1, 
                         max=isolate(input$totalDatasets))
      # Restart in order for it to "kick in"
      shinyjs::click("apply")
    } else {
      js$loading()
      # Selected distribution
      .dist <- isolate(input$distribution)
      if ((.dist == "Uniform" & input$min < input$max) | .dist != "Uniform") {
        # Selected parameter(s)
        .params <- data.table(
          distribution=c("Beta", "Beta", "Binomial", "Binomial", "Cauchy", "Cauchy", 
                         "Chi-Square", "Exponential", "Gamma", "Gamma", "Geometric", 
                         "Normal", "Normal", "Poisson", "Uniform", "Uniform"),
          parameter=c("Alpha", "Beta", "Size", "Probability of Success", "Location", 
                      "Scale", "DF", "Rate", "Shape", "Scale", "Probability of Success", 
                      "Mean", "Standard Dev.", "Rate", "Minimum", "Maximum"),
          value=c(isolate(input$alpha), isolate(input$beta), isolate(input$size), 
                  isolate(input$probSuccess), isolate(input$location), isolate(input$scale),
                  isolate(input$df), isolate(input$rate), isolate(input$shape), 
                  isolate(input$scaleGamma), isolate(input$probSuccessGeom), 
                  isolate(input$mean), isolate(input$sd), isolate(input$ratePoiss), 
                  isolate(input$min), isolate(input$max))
        ) %>%
          .[distribution == .dist]
        
        # Render the "Overview"
        output$formula <- renderText(get.formula(.params))
        output$code <- renderUI(tags$cod(get.code(.params)))
        
        # Sample size
        .n <- isolate(input$sampleSize)
        # Population size
        .N <- .n * 1e2
        if (.dist == "Beta") {
          d <- rbeta(n=.N, 
                     .params[parameter=="Alpha"]$value, 
                     .params[parameter=="Beta"]$value)
        } else if (.dist == "Binomial") {
          d <- rbinom(n=.N, 
                      size=.params[parameter=="Size"]$value,
                      prob=.params[parameter=="Probability of Success"]$value)
        } else if (.dist == "Cauchy") {
          d <- rcauchy(n=.N, 
                       .params[parameter=="Location"]$value,
                       .params[parameter=="Scale"]$value)
        } else if (.dist == "Chi-Square") {
          d <- rchisq(n=.N, .params[parameter=="DF"]$value)
        } else if (.dist == "Exponential") {
          d <- rexp(n=.N, .params[parameter=="Rate"]$value)
        } else if (.dist == "Gamma") {
          d <- rgamma(.N, shape=.params[parameter=="Shape"]$value,
                      scale=.params[parameter=="Scale"]$value)
        } else if (.dist == "Geometric") {
          d <- rgeom(.N, prob=.params[parameter=="Probability of Success"]$value)
        } else if (.dist == "Normal") {
          d <- rnorm(.N, 
                     .params[parameter=="Mean"]$value, 
                     .params[parameter=="Standard Dev."]$value)
        } else if (.dist == "Poisson") {
          d <- rpois(.N, .params[parameter=="Rate"]$value)
        } else if (.dist == "Uniform") {
          d <- runif(.N, 
                     min=.params[parameter=="Minimum"]$value,
                     max=.params[parameter=="Maximum"]$value)
        }
        
        # Create population dataset
        p.dt <- data.table(y=d)
        # Sample dataset(s)
        s.dt <- purrr::map(1:isolate(input$totalDatasets), ~{
          p.dt[sample.int(.N, .n, replace=F),]
        })
        
        # Population Density Plot
        pd.plot <- ggplot(data=p.dt, aes(x=y, y=after_stat(density))) +
          geom_histogram(fill="white", color="black")
        if (!(.dist %in% c("Binomial", "Geometric", "Poisson"))) {
          pd.plot <- pd.plot + geom_density(color="darkred", 
                                            linetype="dashed", 
                                            linewidth=1.2)
        }
        pd.plot <- pd.plot + theme_minimal() +
          labs(title=paste(.dist, "Density"),
               y="f(y)", x="y")
        
        output$pop_dens <- renderPlot(pd.plot)
        
        .round <- ifelse(.dist %in% c("Binomial", "Geometric", "Poisson"), 0, 5)
        output$pop_data <- renderDT(data.preview(p.dt, .round, "Population Data Preview"))
        
        # Assign values to server environment
        assign(".n", value=.n, envir=server.env)
        assign(".N", value=.N, envir=server.env)
        assign("s.dt", value=s.dt, envir=server.env)
        assign("p.dt", value=p.dt, envir=server.env)
        assign(".dist", value=.dist, envir=server.env)
        
        # Trigger update to sample visualizations
        runjs('Shiny.setInputValue("triggerSample", Math.random());')
        # Trigger update to sampling dist. visualizations
        runjs('Shiny.setInputValue("triggerSamplingDist", Math.random());')
      }
    }
  })
}
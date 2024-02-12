update.params.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  up.env <- environment()
  
  # Update Min/Max values for uniform distribution based on selections
  observeEvent(input$min, {
    if (input$min >= input$max) {
      updateNumericInput(session, inputId="max", value=input$min+1)
    }
  })
  observeEvent(input$max, {
    if (input$min >= input$max) {
      updateNumericInput(session, inputId="min", value=input$max-1)
    }
  })
  
  
}
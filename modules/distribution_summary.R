dist.summary.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  dist.summary.env <- environment()
  
  dist.summary.ui <- div(
    div(
      id="codeSample",
      tags$b(style="margin-right:5px;", "Code:"),
      uiOutput("code")
    ),
    div(
      style="margin:5px;",
      uiOutput("dist_summary_table")
    )
  )
  
  observeEvent(input$showSummary, {
    .dist <- get(".dist", server.env)
    .params <- get(".params", server.env)
    
    # Render outputs
    output$dist_summary_table <- renderUI({
      
      dt <- get.dist.details(.dist)
      dt[, Details := purrr::map_chr(dt$Details, ~{
        as.character(
          withMathJax(helpText(.x))
        )
      })]
      div(
        tags$script(
          src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"
        ),
        renderDataTable({
          datatable(
            dt,
            escape=F, 
            rownames=F, 
            colnames=c("", ""),
            selection="none", 
            filter='none',
            options=list(
              searching=F, 
              paging=F, 
              scrollX=F, 
              scrollY=F, 
              orderMulti=F, 
              info=F,
              ordering=F, 
              lengthChange=F
            )
          ) %>%
            DT::formatStyle(1:2, "white-space"="nowrap") %>%
            DT::formatStyle(1:2, fontSize="15px") %>%
            DT::formatStyle(1:2, "cursor"="default")
        })
      )
    })
    
    output$code <- renderUI({
      tags$code(
        style="font-size:15px;",
        get.code(.params)
      )
    })
    
    # Render the "Overview" in a modal pop-up
    showModal(
      modalDialog(
        title="Overview",
        dist.summary.ui,
        easyClose=F
      )
    )
  })
}
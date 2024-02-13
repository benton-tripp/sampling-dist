# Load libraries
suppressPackageStartupMessages(library(shiny))
suppressWarnings(suppressPackageStartupMessages(library(shinyjs)))
suppressWarnings(suppressPackageStartupMessages(library(DT)))

# Define UI
ui <- fluidPage(
  id="uiPage",
  includeScript("www/scripts.js"),
  includeCSS("www/styles.css"),
  tags$head(
    useShinyjs(),  
    extendShinyjs(text=readr::read_file('www/scripts.js'),
                  functions = c("loading", "finishedLoading")),
    tags$title("Sampling Distributions"),
    tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.css", 
        integrity = "sha384-MlJdn/WNKDGXveldHDdyRP1R4CTHr3FeuDNfhsLPYrq2t0UBkUdK2jyTnXPEK1NQ", 
        crossorigin = "anonymous"
      ),
      tags$script(
        defer = "", 
        src = "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.js", 
        integrity = "sha384-VQ8d8WVFw0yHhCk5E8I86oOhv48xLpnDZx5T9GogA/Y84DcCKWXDmSDfn13bzFZY", 
        crossorigin = "anonymous"
      ),
      tags$link(
        rel="stylesheet",
        href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css",
        integrity="sha512-DTOQO9RWCH3ppGqcWaEA1BIZOC6xxalwEsw9c2QQeAIftl+Vegovlnee1c9QX4TctnWMn13TZye+giMm8e2LwA==",
        crossorigin="anonymous",
        referrerpolicy="no-referrer"
      )
    ),
  ),
  title="Sampling Distributions",
  h2("Sampling Distribution Examples"),
  div(
    id="layoutSection",
    class="wrapper",
    div(
      id="sidebarSection",
      div(
        id="sidebarOptions",
        h3("Distribution"),
        selectInput(inputId="distribution", label=NULL, selected="Normal",
                    choices=c("Beta", "Binomial", "Cauchy", "Chi-Square", 
                              "Exponential", "Gamma", "Geometric", "Normal",
                              "Poisson", "Uniform")),
        conditionalPanel(
          "input.distribution==='Beta'",
          id="betaParams",
          div(
            numericInput(inputId="alpha", label="Alpha", value=1, min=0),
            numericInput(inputId="beta", label="Beta", value=1, min=0)
          )
        ),
        conditionalPanel(
          "input.distribution==='Binomial'",
          id="binomialParams",
          numericInput(inputId="size", label="Size",
                       value=1, min=0, step=1),
          numericInput(inputId="probSuccess", label="Probability of Success",
                       value=0.5, min=0, max=1)
        ),
        conditionalPanel(
          "input.distribution==='Cauchy'",
          id="cauchyParams",
          div(
            numericInput(inputId="location", label="Location", value=0),
            numericInput(inputId="scale", label="Scale", value=1, min=1e-5)
          )
        ),
        conditionalPanel(
          "input.distribution==='Chi-Square'",
          id="chisquareParams",
          numericInput(inputId="df", label="DF", value=1, min=1, step=1)
        ),
        conditionalPanel(
          "input.distribution==='Exponential'",
          id="exponentialParams",
          numericInput(inputId="rate", label="Rate", value=1, min=1e-5)
        ),
        conditionalPanel(
          "input.distribution==='Gamma'",
          id="gammaParams",
          div(
            numericInput(inputId="shape", label="Shape", value=1, min=1e-5),
            numericInput(inputId="scaleGamma", label="Scale", value=1, min=1e-5)
          )
        ),
        conditionalPanel(
          "input.distribution==='Geometric'",
          id="geometricParams",
          numericInput(inputId="probSuccessGeom", label="Probability of Success",
                       value=0.5, min=1e-5, max=1-1e-5)
        ),
        conditionalPanel(
          "input.distribution==='Normal'",
          id="normalParams",
          numericInput(inputId="mean", label="Mean", value=0),
          numericInput(inputId="sd", label="Standard Dev.", value=1, min=1e-5)
        ),
        conditionalPanel(
          "input.distribution==='Poisson'",
          id="poissonParams",
          numericInput(inputId="ratePoiss", label="Rate", value=1, min=1e-5)
        ),
        conditionalPanel(
          "input.distribution==='Uniform'",
          id="uniformParams",
          # Minimum (a): Any real number, but less than b.
          # Maximum (b): Any real number, greater than a.
          numericInput(inputId="min", label="Minimum", value=0),
          numericInput(inputId="max", label="Maximum", value=1)
        ),
        br(),
        sliderInput(inputId="sampleSize", label="Sample Size", value=10,
                     min=2, max=1e2, step=1, ticks=F),
        sliderInput(inputId="totalDatasets", label="Number of Datasets", value=1, 
                    min=1, max=1e3, step=1, ticks=F),
        br(),
        actionButton(inputId="apply", label="Apply Selection")
      )
    ),
    div(
      id="mainSection",
      div(
        id="mainSectionTopRow",
        div(
          id="displaySelect",
          tags$b(style="margin:5px;", "Select Display:"),
          div(
            style="max-width: 170px;",
            selectInput(inputId="vizType", label=NULL,
                        choices=c("Plot", "Summary Statistics",
                                  "Data Preview"))
          )
        ),
        tags$button(
          id="showSummary",
          type="button",
          class="btn btn-default action-button shiny-bound-input",
          style="max-height:35px;",
          div(
            style="shiny-row",
            tags$i(
              style="margin-right:5px; font-size:20px;",
              class="fa fa-info-circle",
              `aria-hidden`="true"
            ),
            tags$span(
              style="position: relative; top: -2px;",
              "Show Distribution Overview"
            )
          )
        )
      ),
      div(
        class="main-section-row",
        div(
          id="popVizHeader",
          h4("Parent Population Distribution"),
          conditionalPanel(
            "input.vizType === 'Plot'",
            div(
              style="margin-top:25px;",
              plotOutput("pop_dens", width=400, height=450)
            )
          ),
          conditionalPanel(
            "input.vizType === 'Summary Statistics'",
            class="preview-data",
            div(
              style="margin-top:45px;",
              DTOutput("pop_summary")
            )
          ),
          conditionalPanel(
            "input.vizType === 'Data Preview'",
            class="preview-data",
            div(
              style="margin-top:45px;",
              DTOutput("pop_data")
            )
          )
        ),
        div(
          id="sampleSections",
          div(
            id="sampleVizHeader",
            h4("Selected Sample Data"),
            div(
              style="margin-left: 30px; width: 170px;",
              numericInput(inputId="sampleDatasets", 
                           label="Sample Dataset #", value=1, 
                           min=1, max=1, step=1) 
            )
          ),
          div(
            conditionalPanel(
              "input.vizType === 'Plot'",
              div(
                id="sampleVizSection",
                style="margin-right:10px;",
                plotOutput("samp_dens", width=400, height=430)
              )
            ),
            conditionalPanel(
              "input.vizType === 'Summary Statistics'",
              div(
                class="preview-data",
                DTOutput("samp_summary")
              )
            ),
            conditionalPanel(
              "input.vizType === 'Data Preview'",
              div(
                class="preview-data",
                DTOutput("samp_data")
              )
            )
          )
        ),
        div(
          id="sampleDistSections",
          div(
            id="sampleDistVizHeader",
            h4("Sampling Distribution"),
            div(
              style="margin-left: 30px; width: 220px;",
              selectInput(inputId="selectStat", label="Statistic", 
                          selected="Sample Mean",
                          choices=c("Sample Mean", "Standardized Sample Mean", 
                                    "Sample Variance", 
                                    "Sample Standard Deviation",
                                    "Sample Median", "Sample Min.", 
                                    "Sample Max."))
            )
          ),
          div(
            conditionalPanel(
              "input.vizType === 'Plot'",
              div(
                plotOutput("samp_dist_dens", width=400, height=430)
              )
            ),
            conditionalPanel(
              "input.vizType === 'Summary Statistics'",
              div(
                class="preview-data",
                DTOutput("samp_dist_summary")
              )
            ),
            conditionalPanel(
              "input.vizType === 'Data Preview'",
              div(
                class="preview-data",
                DTOutput("samp_dist_data")
              )
            )
          )
        )
      )
    )
  )
)
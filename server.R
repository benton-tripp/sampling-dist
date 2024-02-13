# Load libraries
suppressWarnings(suppressPackageStartupMessages(library(shiny)))
suppressWarnings(suppressPackageStartupMessages(library(shinyjs)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(library(DT)))

# Set seed
set.seed(19)

# Source modules & utils
purrr::walk(list.files("modules", full.names=T), source)
purrr::walk(list.files("utils", full.names=T), source)

# Define server
server <- function(input, output, session) {

  # Define server environment variable
  server.env <- environment()
  
  # Load modules
  apply.module(input, output, session, server.env)
  update.params.module(input, output, session, server.env)
  samples.module(input, output, session, server.env)
  sampling.dist.module(input, output, session, server.env)
  dist.summary.module(input, output, session, server.env)
  
  # Load initial plots/stats/data
  shinyjs::click("apply")
}
library(shiny)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(chron)
library(padr)
library(gridExtra)
library(forcats)
library(zoo)
library(bdscale)
library(tibble)
library(tidyverse)
library(R.matlab)
library(stringr)
library(purrr)
library(shinyjs)
library(DT)
library(plotly)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(formattable)


source(file.path("functions", "load_mass.R"))
source(file.path("functions", "plots_mass.R"))



ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Log",

    # include the UI for each tab
    source(file.path("ui", "mass_ui.R"), local = TRUE)$value
  )
)

server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server", "mass_server.R"), local = TRUE)$value
}

shinyApp(ui, server)

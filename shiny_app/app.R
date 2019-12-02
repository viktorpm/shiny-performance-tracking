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





source(file.path("functions", "load_data.R"))
source(file.path("functions", "plots_DelayComp.R"))
source(file.path("functions", "plots_SoundCateg.R"))
source(file.path("functions", "plots_AltStat.R"))
source(file.path("functions", "plots_summary.R"))




ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Protocols",

    # include the UI for each tab
    source(file.path("ui", "Summary_ui.R"), local = TRUE)$value,
    source(file.path("ui", "AthenaDelayComp_ui.R"), local = TRUE)$value,
    source(file.path("ui", "SoundCategorization_ui.R"), local = TRUE)$value,
    source(file.path("ui", "AltStat_ui.R"), local = TRUE)$value
  )
)

server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server", "Summary_server.R"), local = TRUE)$value
  source(file.path("server", "AthenaDelayComp_server.R"), local = TRUE)$value
  source(file.path("server", "SoundCategorization_server.R"), local = TRUE)$value
  source(file.path("server", "AltStat_server.R"), local = TRUE)$value
}

shinyApp(ui, server)

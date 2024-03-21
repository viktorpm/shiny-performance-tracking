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
library(parallel)
library(magrittr)
library(readr)


# Source custom functions and UI components
source(file.path("functions", "load_data.R"))
source(file.path("functions", "plots_DelayComp.R"))
source(file.path("functions", "plots_SoundCateg.R"))
source(file.path("functions", "plots_AltStat.R"))
source(file.path("functions", "plots_AltSoundCateg.R"))
source(file.path("functions", "plots_summary.R"))
source(file.path("functions", "plots_AltSoundCategCatch.R"))
source(file.path("functions", "plots_SequenceComp.R"))
source(file.path("functions", "plots_SoundCatContinuous.R"))

# Define the UI for the Shiny app
ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Room",
    # Include the UI for each tab
    source(file.path("ui", "Summary_ui.R"), local = TRUE)$value,
    source(file.path("ui", "AthenaDelayComp_ui.R"), local = TRUE)$value,
    source(file.path("ui", "SoundCategorization_ui.R"), local = TRUE)$value,
    source(file.path("ui", "AltSoundCategorization_ui.R"), local = TRUE)$value,
    source(file.path("ui", "AltStat_ui.R"), local = TRUE)$value,
    source(file.path("ui", "AltSoundCategorizationCatch_ui.R"), local = TRUE)$value,
    source(file.path("ui", "ElenaSequenceComp_ui.R"), local = TRUE)$value,
    source(file.path("ui", "SoundCatContinuous_ui.R"), local = TRUE)$value
  )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server", "Summary_server.R"), local = TRUE)$value
  source(file.path("server", "AthenaDelayComp_server.R"), local = TRUE)$value
  source(file.path("server", "SoundCategorization_server.R"), local = TRUE)$value
  source(file.path("server", "AltSoundCategorization_server.R"), local = TRUE)$value
  source(file.path("server", "AltStat_server.R"), local = TRUE)$value
  source(file.path("server", "AltSoundCategorizationCatch_server.R"), local = TRUE)$value
  source(file.path("server", "ElenaSequenceComp_server.R"), local = TRUE)$value
  source(file.path("server", "SoundCatContinuous_server.R"), local = TRUE)$value
}

# Run the Shiny app
shinyApp(ui, server)

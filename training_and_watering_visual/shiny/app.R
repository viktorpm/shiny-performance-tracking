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
library(odbc)
library(DBI)
library(RMySQL)

# connecting to db
# first open an ssh tunnel: ssh -f vplattner@192.168.238.210 -L 8080:172.24.155.100:3306 -N
# all other ssh tunnels have to be closed 

source(file.path("functions", "load_mass.R"))
source(file.path("functions", "load_water.R"))
source(file.path("functions", "load_rats.R"))
source(file.path("functions", "plots_mass.R"))




ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    "Log",
    position = "fixed-top",
    # tags$style(type="text/css", "body {padding-top: 70px;}"),
    
    

    # include the UI for each tab
    source(file.path("ui", "mass_ui.R"), local = TRUE)$value,
    source(file.path("ui", "water_ui.R"), local = TRUE)$value,
    source(file.path("ui", "record_ui.R"), local = TRUE)$value
  )
)

server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server", "mass_server.R"), local = TRUE)$value
  source(file.path("server", "water_server.R"), local = TRUE)$value
  source(file.path("server", "record_server.R"), local = TRUE)$value
}


onStop(function() {
  lapply(dbListConnections(drv = MySQL()), dbDisconnect)
  #dbDisconnect(conn = akrami_db)
})



shinyApp(ui, server)

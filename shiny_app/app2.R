library(shiny)
library(shinydashboard)
library(shinyjs)
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
library(DT)
library(plotly)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(parallel)
library(magrittr)
library(readr)

source(file.path("functions", "load_data.R"))

source(file.path("functions", "ChoiceDirectionPlot.R"))
source(file.path("functions", "CompletedTrialsPlot.R"))


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Training Dashboard"),
  dashboardSidebar(
    # Protocol selection
    selectInput(
      inputId = "protocol",
      label = "Protocol",
      choices = TRAINING$protocol %>% unique() %>% sort() %>% as.vector(),
      # always selects the protocol with the latest entries
      selected = TRAINING %>% dplyr::arrange(desc(date)) %>% dplyr::slice(1) %>% dplyr::select(protocol) %>% pull()
    ),
    
    # Filters
    radioButtons(
      inputId = "show",
      label = "Show",
      choices = c("All animals", "Experimenter", "Individual animals"),
      selected = "All animals"
    ),
    
    # Experimenter selection
    selectInput(
      inputId = "exp_select",
      label = "Select experimenter",
      choices = NULL
    ),
    
    # Animal selection
    selectInput(
      inputId = "animal_select",
      label = "Select animals to show",
      choices = NULL
    ),
    
    # Date range input
    dateRangeInput(
      inputId = "setdate",
      label = "Dates to show (default: last 3 weeks)",
      start = max(TRAINING$date) - 21,
      end = max(TRAINING$date),
      min = min(TRAINING$date),
      max = max(TRAINING$date)
    ),
    
    # Training stage input
    checkboxGroupInput(
      inputId = "stage",
      label = "Select stages to show",
      choices = TRAINING$stage %>% unique() %>% as.vector(),
      selected = TRAINING$stage %>% unique() %>% as.vector()
    )
  ),
  
  # Define dashboard body
  dashboardBody(
    useShinyjs(),
    fluidRow(
      column(6, plotOutput("plt1")),
      column(6, plotOutput("plt2")),
    ),
    fluidRow(
      column(6, plotOutput("plot_correct_ratio")),
      column(6, plotOutput("plot_stage_tracking")),
    )
  )
)

server <- function(input, output, session) {
  
  # updating "Select experimenter" drop down list based on protocol and date
  update_exp_select <- reactive({
    req(input$protocol, input$setdate)
    TRAINING %>%
      dplyr::filter(
        protocol == input$protocol, 
        date >= input$setdate[1], date <= input$setdate[2]) %>%
      dplyr::select(experimenter) %>%
      unique() %>%
      pull() %>%
      sort() %>% 
      as.vector()
  })
  
  
  
  # updating "Select animals to show" drop down list based on protocol, date and experimenter
  update_animal_select <- reactive({
    req(input$protocol, input$exp_select, input$setdate)
    TRAINING %>%
      dplyr::filter(
        protocol == input$protocol,
        experimenter == input$exp_select,
        date >= input$setdate[1], date <= input$setdate[2]) %>%
      dplyr::select(animal_id) %>%
      unique() %>%
      pull() %>%
      sort() %>% 
      as.vector()
  })
  
  
  # choice direction plot
  create_plt1 <- reactive({
    ChoiceDirectionPlot(
      prtcl = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      show = input$show
    )
  })
  
  output$plt1 <- renderPlot({
    create_plt1()
  })
  

  # No. completed trials plot
  create_plt2 <- reactive({
    CompletedTrialsPlot(
      prtcl = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      show = input$show
    )
  })
  output$plt2 <- renderPlot({
    create_plt2()
  })
  
  
  # # correct ratio plot
  # create_plot_correct <- reactive({
  #   plot_generation(
  #     plottype = "Correct ratio",
  #     protocol = input$protocol,
  #     datelim = input$setdate,
  #     stage_filter = input$stage,
  #     animal_filter = input$animal_select,
  #     exp = input$exp_select,
  #     show = input$show
  #   )
  # })
  # 
  # output$plot_correct_ratio <- renderPlot({
  #   create_plot_correct()
  # })
  # 
  # # stage tracking plot
  # create_plot_stage <- reactive({
  #   plot_generation(
  #     plottype = "Stage tracking",
  #     protocol = input$protocol,
  #     datelim = input$setdate,
  #     stage_filter = input$stage,
  #     animal_filter = input$animal_select,
  #     exp = input$exp_select,
  #     show = input$show
  #   )
  # })
  # 
  # output$plot_stage_tracking <- renderPlot({
  #   create_plot_stage()
  # })
  

  
  observeEvent(c(input$show, input$protocol, input$exp_select), {
    if (input$show == "All animals") {
      shinyjs::disable("animal_select")
      shinyjs::disable("exp_select")
      }
    
    if (input$show == "Experimenter") {
      shinyjs::enable("exp_select")
      shinyjs::disable("animal_select")
      updateSelectInput(session, inputId = "exp_select", choices = update_exp_select())
      }
      
    if (input$show == "Individual animals") {
      shinyjs::enable("exp_select")
      shinyjs::enable("animal_select")
      updateSelectInput(session, inputId = "animal_select", choices = update_animal_select())
      }
  })

  
}

shinyApp(ui, server)
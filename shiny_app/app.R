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
source(file.path("functions", "plot_theme_settings.R"))
source(file.path("functions", "ChoiceDirectionPlot.R"))
source(file.path("functions", "CompletedTrialsPlot.R"))
source(file.path("functions", "CorrectRatioPlot.R"))
source(file.path("functions", "StageTrackingPlot.R"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Training Dashboard"),
  dashboardSidebar(
    br(),
    br(),
    actionButton("reset", "Reset to Defaults"),
    br(),

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
      label = HTML("Dates to show<br>(default: last 3 weeks)"),
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
      column(6, plotOutput("plt1", height = "600px")),
      column(6, plotOutput("plt4", height = "600px")),
    ),
    br(),
    fluidRow(
      column(6, plotOutput("plt2", height = "600px")),
      column(6, plotOutput("plt3", height = "600px")),
    )
  )
)

server <- function(input, output, session) {
  
  # Capture initial state of inputs
  initialState <- reactiveValues(
    protocol = TRAINING %>% dplyr::arrange(desc(date)) %>% dplyr::slice(1) %>% dplyr::select(protocol) %>% pull(),
    show = "All animals",
    exp_select = NULL,
    animal_select = NULL,
    setdate = list(start = max(TRAINING$date) - 21, end = max(TRAINING$date)),
    stage = TRAINING$stage %>% unique() %>% as.vector()
  )
  # Reset inputs to initial state when reset button is clicked
  observeEvent(input$reset, {
    updateSelectInput(session, "protocol", selected = initialState$protocol)
    updateRadioButtons(session, "show", selected = initialState$show)
    updateSelectInput(session, "exp_select", choices = NULL) # Assuming you want to clear this
    updateSelectInput(session, "animal_select", choices = NULL) # Assuming you want to clear this
    updateDateRangeInput(session, "setdate", start = initialState$setdate$start, end = initialState$setdate$end)
    updateCheckboxGroupInput(session, "stage", selected = initialState$stage)
  })
  
  # updating "Select experimenter" drop down list based on protocol and date
  update_exp_select <- reactive({
    req(input$protocol, input$setdate)
    TRAINING %>%
      dplyr::filter(
        protocol == input$protocol,
        date >= input$setdate[1], date <= input$setdate[2]
      ) %>%
      dplyr::select(experimenter) %>%
      unique() %>%
      pull() %>%
      sort() %>%
      as.vector()
  })

  # Initialize a reactiveVal to store the current selection of the experimenter dropdown
  current_exp_select <- reactiveVal()

  # Update the reactiveVal whenever the selection changes
  observe({
    current_exp_select(input$exp_select)
  })

  # updating "Select animals to show" drop down list based on protocol, date and experimenter
  update_animal_select <- reactive({
    req(input$protocol, input$exp_select, input$setdate)
    TRAINING %>%
      dplyr::filter(
        protocol == input$protocol,
        experimenter == input$exp_select,
        date >= input$setdate[1], date <= input$setdate[2]
      ) %>%
      dplyr::select(animal_id) %>%
      unique() %>%
      pull() %>%
      sort() %>%
      as.vector()
  })

  current_animal_select <- reactiveVal()
  observe({
    current_animal_select(input$animal_select)
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

  # correct ratio plot
  create_plt3 <- reactive({
    CorrectRatioPlot(
      prtcl = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      show = input$show
    )
  })

  output$plt3 <- renderPlot({
    create_plt3()
  })

  # stage tracking plot
  create_plt4 <- reactive({
    StageTrackingPlot(
      prtcl = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      show = input$show
    )
  })

  output$plt4 <- renderPlot({
    create_plt4()
  })

  observeEvent(c(input$show, input$protocol, input$exp_select), {
    if (input$show == "All animals") {
      shinyjs::disable("animal_select")
      shinyjs::disable("exp_select")
    }

    if (input$show == "Experimenter") {
      shinyjs::enable("exp_select")
      shinyjs::disable("animal_select")

      # Update the choices and pass the current selection to the selected argument
      updateSelectInput(session, inputId = "exp_select", choices = update_exp_select(), selected = current_exp_select())
    }

    if (input$show == "Individual animals") {
      shinyjs::enable("exp_select")
      shinyjs::enable("animal_select")
      updateSelectInput(session, inputId = "animal_select", choices = update_animal_select(), selected = current_animal_select())
    }
  })
}

shinyApp(ui, server)

library(shiny)
library(shinydashboard)
library(shinyjs)


source(file.path("functions", "load_data.R"))
source(file.path("functions", "plot_generation.R"))


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Training Dashboard"),
  dashboardSidebar(
    # Protocol selection
    selectInput(
      inputId = "protocol",
      label = "Protocol",
      choices = all_protocols # defined in load_data.R (unique values in the protocol column of TRAINING tibble)
    ),
    # Filters
    radioButtons(
      inputId = "f_options",
      label = "Filters",
      choices = c("All animals", "Experimenter", "Individual animals"),
      selected = "All animals"
    ),
    # Experimenter selection
    selectInput(
      inputId = "exp_select",
      label = "Select experimenter",
      choices = TRAINING$experimenter %>% unique() %>% as.vector()
    ),
    # Animal selection
    selectInput(
      inputId = "animal_select",
      label = "Select animals to show",
      choices = TRAINING %>%
        dplyr::filter(protocol == "@SoundCategorization") %>%
        select(animal_id) %>%
        unique() %>%
        pull() %>%
        as.vector()
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
      column(6, plotOutput("plot_choice_direction")),
      column(6, plotOutput("plot_completed_trials")),
    ),
    fluidRow(
      column(6, plotOutput("plot_correct_ratio")),
      column(6, plotOutput("plot_stage_tracking")),
    )
  )
)

server <- function(input, output, session) {
  
  # choice direction plot
  create_plot_choice <- reactive({
    plot_generation(
      plottype = "Choice direction",
      protocol = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      f_options = input$f_options
    )
  })
  
  output$plot_choice_direction <- renderPlot({
    create_plot_choice()
  })
  
  # No. completed trials plot
  create_plot_completed <- reactive({
    plot_generation(
      plottype = "No. completed trials",
      protocol = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      f_options = input$f_options
    )
  })
  output$plot_completed_trials <- renderPlot({
    create_plot_completed()
  })
  
  # correct ratio plot
  create_plot_correct <- reactive({
    plot_generation(
      plottype = "Correct ratio",
      protocol = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      f_options = input$f_options
    )
  })
  
  output$plot_correct_ratio <- renderPlot({
    create_plot_correct()
  })
  
  # stage tracking plot
  create_plot_stage <- reactive({
    plot_generation(
      plottype = "Stage tracking",
      protocol = input$protocol,
      datelim = input$setdate,
      stage_filter = input$stage,
      animal_filter = input$animal_select,
      exp = input$exp_select,
      f_options = input$f_options
    )
  })
  
  output$plot_stage_tracking <- renderPlot({
    create_plot_stage()
  })
  
  
  observe({
    if (input$f_options == "All animals") {
      disable("animal_select")
      disable("exp_select")
    }
    
    if (input$f_options == "Experimenter") {
      enable("exp_select")
      disable("animal_select")
    }
    
    if (input$f_options == "Individual animals") {
      enable("exp_select")
      enable("animal_select")
    }
    
    
    
  })
}

shinyApp(ui, server)
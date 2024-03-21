library(shiny)
library(shinydashboard)



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

ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    
    
    selectInput(
      inputId = "protocol_SC",
      label = "Protocol",
      choices = c(
        "Summary",
        "@AthenaDelayComp",
        "@SoundCategorization",
        "@AltSoundCategorization",
        "@AltStat",
        "@AltSoundCategorizationCatch",
        "@ElenaSequenceComp",
        "@SoundCatContinuous"
      )
    ),
    
    selectInput(
      inputId = "plot_type_SC",
      label = "Select plot type",
      choices = c(
        "CP duration",
        "Choice direction",
        "No. done trials",
        "No. completed trials",
        "No. correct trials",
        "Correct ratio",
        "Stage tracking"
        #"Missing data"
      )
    ),
    
    radioButtons(
      inputId = "f_options_SC",
      label = "Filters",
      choices = c("All animals", "Experimenter", "Individual animals"),
      selected = "All animals"
    ),
    
    
    selectInput(
      inputId = "exp_select_SC",
      label = "Select experimenter",
      choices = TRAINING$experimenter %>% unique() %>% as.vector()
    ),
    
    
    downloadButton("report_SC", "Generate report"),
    
    
    selectInput(
      inputId = "animal_select_SC",
      label = "Select animals to show",
      # choices = TRAINING$animal_id %>% unique() %>% as.vector()
      choices = TRAINING %>%
        dplyr::filter(protocol == "@SoundCategorization") %>%
        select(animal_id) %>%
        unique() %>%
        pull() %>%
        as.vector()
    ),
    
    
    
    dateRangeInput(
      inputId = "setdate_SC",
      label = "Dates to show (default: last 3 weeks)",
      start = max(TRAINING$date) - 21,
      end = max(TRAINING$date),
      min = min(TRAINING$date),
      max = max(TRAINING$date)
    ),
    
    
    
    checkboxGroupInput(
      inputId = "stage_SC",
      label = "Select stages to show",
      choices = TRAINING$stage %>% unique() %>% as.vector(),
      selected = TRAINING$stage %>% unique() %>% as.vector()
    )
    
  
  ),
  
  
  dashboardBody(
    fluidRow(
      splitLayout(
        cellWidths = c("50%", "50%"),
        plotOutput("distPlot1"),
        plotOutput("distPlot2")
      )
    )
  )
)

server <- function(input, output) {
  output$distPlot1 <- renderPlot({
    data <- switch(input$dataset,
                   "mtcars" = mtcars,
                   "iris" = iris,
                   "airquality" = airquality)
    hist(data$mpg, main = "Histogram of MPG", xlab = "Miles per Gallon", col = "lightblue", border = "black")
  })
  
  output$distPlot2 <- renderPlot({
    hist(rnorm(input$obs), main = "Histogram of Random Numbers", xlab = "Value", col = "lightblue", border = "black")
  })
}

shinyApp(ui, server)

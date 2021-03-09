tabPanel(
  "Mass Log",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      
      radioButtons(
        inputId = "f_options",
        label = "Filters",
        choices = c("Experimenter", "Individual animals"),
        selected = "Experimenter"
      ),
      
      
      selectInput(
        inputId = "exp_select",
        label = "Select experimenter",
        choices = mass$exp_id %>% unique() %>% as.vector(),
        selected = "athena"
      ),
      
      
      selectInput(
        inputId = "animal_select",
        label = "Select animals to show",
        choices = TRAINING$animal_id %>% unique() %>% as.vector()
      ),
      
      
      dateRangeInput(
        inputId = "setdate",
        label = "Dates to show (default: last 3 weeks)",
        start = max(TRAINING$date) - 21,
        end = max(TRAINING$date),
        min = min(TRAINING$date),
        max = max(TRAINING$date)
      )
    ),
    
    mainPanel(
      width = 9,
      plotOutput(outputId = "plot", height = 1000),
      formattableOutput(outputId = "mass_table")
    )
  )
)
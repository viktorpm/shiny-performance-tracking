tabPanel(
  "@SoundCategorization",
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
    
    mainPanel(
      width = 9,
      plotOutput(outputId = "plot_SC", height = 1000),
      dataTableOutput(outputId = "perform_SC")
    )
  )
)
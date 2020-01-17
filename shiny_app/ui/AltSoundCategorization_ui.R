tabPanel(
  "@AltSoundCategorization",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = "plot_type_ASC",
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
        inputId = "f_options_ASC",
        label = "Filters",
        choices = c("All animals", "Experimenter", "Individual animals"),
        selected = "All animals"
      ),
      
      
      selectInput(
        inputId = "exp_select_ASC",
        label = "Select experimenter",
        choices = TRAINING$experimenter %>% unique() %>% as.vector()
      ),
      
      
      downloadButton("report_ASC", "Generate report"),
      
      
      selectInput(
        inputId = "animal_select_ASC",
        label = "Select animals to show",
        # choices = TRAINING$animal_id %>% unique() %>% as.vector()
        choices = TRAINING %>%
          dplyr::filter(protocol == "@AltSoundCategorization") %>%
          select(animal_id) %>%
          unique() %>%
          pull() %>%
          as.vector()
      ),
      
      
      
      dateRangeInput(
        inputId = "setdate_ASC",
        label = "Dates to show (default: last 3 weeks)",
        start = max(TRAINING$date) - 21,
        end = max(TRAINING$date),
        min = min(TRAINING$date),
        max = max(TRAINING$date)
      ),
      
      
      
      checkboxGroupInput(
        inputId = "stage_ASC",
        label = "Select stages to show",
        choices = TRAINING$stage %>% unique() %>% as.vector(),
        selected = TRAINING$stage %>% unique() %>% as.vector()
      )
    ),
    
    mainPanel(
      width = 9,
      plotOutput(outputId = "plot_ASC", height = 1000),
      dataTableOutput(outputId = "perform_ASC")
    )
  )
)
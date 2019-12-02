tabPanel(
  "Summary",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = "plot_type_sum",
        label = "Select plot type",
        choices = c(
          "Stage tracking",
          "Missing data"
        )
      ),
      
      
      dateRangeInput(
        inputId = "setdate_sum",
        label = "Dates to show",
        start = max(TRAINING$date) - 56,
        end = max(TRAINING$date),
        min = min(TRAINING$date),
        max = max(TRAINING$date)
      ),
      
      
      dateInput(
        inputId = "date_to_sum",
        label = "Pick a date to summarize",
        value = TRAINING$date %>% unique() %>% max(),
        min = min(TRAINING$date),
        max = max(TRAINING$date)
      )
    ),
    
    mainPanel(
      width = 9,
      plotOutput(outputId = "plot_sum", height = 800),
      dataTableOutput(outputId = "table_sum")
    )
  )
)
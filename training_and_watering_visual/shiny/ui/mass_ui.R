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
        choices = mass$animal_id %>% unique() %>% as.vector()
        # choices = mass %>% dplyr::filter(exp_id == input$exp_select) %>% pull(animal_id) %>% unique()
      ),
      
      
      dateRangeInput(
        inputId = "setdate",
        label = "Dates to show (default: last 3 weeks)",
        start = Sys.Date() - 21,
        end =  Sys.Date(),
        min = min(mass$date),
        max = Sys.Date()
      )
    ),
    
    mainPanel(
      width = 9,
      plotOutput(outputId = "plot", height = 1000),
      formattableOutput(outputId = "mass_table")
    )
  )
)
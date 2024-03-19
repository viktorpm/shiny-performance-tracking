tabPanel(
  "Training and watering",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "position:fixed;width:inherit;",
      
      selectInput(
        inputId = "exp_select_w",
        label = "Select experimenter",
        choices = time_ranges$Experimenter %>% unique() %>% as.vector(),
        selected = "athena"
      ),
      
      selectInput(
        inputId = "animal_select_w",
        label = "Select animals to show",
        choices = time_ranges$Animal %>% unique() %>% as.vector()
      ),
      
  
      
      dateRangeInput(
        inputId = "setdate_w",
        label = "Dates to show (default: last 3 weeks)",
        start = Sys.Date() - 21,
        end = Sys.Date(),
        min = min(time_ranges$Date, na.rm = T),
        max = Sys.Date()
      )
    ),
    
    mainPanel(
      # to keep date window on top of the navbar panel
      tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
      
      column(width = 9,offset = 2,
             DT::dataTableOutput(outputId = "water_table")),
      width = 9
     
    )
  )
)

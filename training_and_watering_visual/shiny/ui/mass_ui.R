tabPanel(
  "Weight",
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", "body {padding-top: 70px;}"), # to keep sidebar panel on top of navbar tabs
      width = 2,
      style = "position:fixed;width:inherit;",


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
        end = Sys.Date(),
        min = min(mass$date, na.rm = T),
        max = Sys.Date()
      )
    ),

    mainPanel(
      column(width = 9,offset = 2,
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "plot", height = 1000)),
        tabPanel("Table", DT::dataTableOutput(outputId = "mass_table")),
        tabPanel(
          "Record weight", 
          DT::dataTableOutput(outputId = "mass_rec_table"),
          actionButton(inputId = "save", label = "Save", width = 200)
          )
      )),
      
      width = 9,
      
    )
  )
)

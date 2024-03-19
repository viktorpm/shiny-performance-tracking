tabPanel(
  "Record weight and watering times",
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", "body {padding-top: 70px;}"), # to keep sidebar panel on top of navbar tabs
      width = 2,
      style = "position:fixed;width:inherit;",
      
      shinyTime::timeInput(inputId = "timestart", value = Sys.time(), label = "Watering time (from)", seconds = F), 
      actionButton(inputId = "timestartadd", label = "Add", width = 120),
      
      
      shinyTime::timeInput(inputId = "timeend", value = Sys.time() + 1200, label = "Watering time (to)", seconds = F),
      actionButton(inputId = "timeendadd", label = "Add", width = 120),
      
      actionButton(inputId = "save", label = "Save", width = 120)
      
      
     
      
    ),
    
    mainPanel(
       column(width = 9,offset = 2,

      
      DT::dataTableOutput(outputId = "mass_rec_table"),
      # actionButton(inputId = "save", label = "Save", width = 200)

      ),
      width = 9,
    )
  )
)

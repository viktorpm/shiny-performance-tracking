tabPanel(
  "Record weight and watering times",
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", "body {padding-top: 70px;}"), # to keep sidebar panel on top of navbar tabs
      width = 2,
      style = "position:fixed;width:inherit;",
      
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

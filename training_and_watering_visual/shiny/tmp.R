library(shiny)
library(DT)
library(dplyr)


shinyApp(
  ui <- fluidPage(DT::dataTableOutput("ruless"),
                  fluidRow(column(4, offset = 1, actionButton("save", "Save", width = 200),))),
  
  server <- function(input, output) {
    
    values <- reactiveValues(data = NULL)
    
    values$data <- as.data.frame(
      cbind(c("a", "d", "b", "c", "e", "f"),
            c(1463, 159, 54, 52, 52, 220),
            c(0.7315, 0.0795, 0.027, 0.026, 0.026, 0.11)
      )
    )
    
    shinyInput = function(FUN, len, id, ...) {
      #validate(need(character(len)>0,message=paste("")))
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
      }
      inputs
    }
    
    dataExpTable <- reactive({
      
      data.frame(delete=shinyInput(textInput,nrow(values$data),"cbox_"), values$data)
      
    })
    
    output$ruless <- DT::renderDataTable({
      datatable(
        dataExpTable(),
        selection="multiple",
        escape = FALSE,
        filter = list(position = 'top', clear = FALSE),
        extensions = list("ColReorder" = NULL, "Buttons" = NULL),
        options = list(
          dom = 'BRrltpi',
          autoWidth=TRUE,
          lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
          ColReorder = TRUE,
          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download',
              selected = TRUE
            )
          )
        )
      )
    })
    
    
    
    observeEvent(
      eventExpr   = input$save,
      handlerExpr = {
        out_col <- character(nrow(values$data))
        
        for (i in seq_len(nrow(values$data))) {
          
          out_col[i] <- input[[paste0("cbox_", i)]]
          
        }
        
        out_df <- cbind(out_col, values$data)
        
        write.csv(out_df, "test.csv", row.names = FALSE)
      }
    )
    # observeEvent(input$save, {
    #   
    #   write.csv2(dataExpTable(), "test.csv", row.names = FALSE)
    #   
    # })
    
  }
)

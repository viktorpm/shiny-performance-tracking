
### defining shinyInput... helper functions to add text input in every row in 3 columns
shinyInputWaterStart = function(FUN, len, id, ...) {
  #validate(need(character(len)>0,message=paste("")))
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(
      FUN(paste0(id, i), 
          label = NULL,  
          width = "50%", 
          #placeholder = "HH:MM",
          value = Sys.time() %>% substring(first = 12, last = 16), ...)
      )
  }
  inputs
}

shinyInputWaterEnd = function(FUN, len, id, ...) {
  #validate(need(character(len)>0,message=paste("")))
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(
      FUN(paste0(id, i), 
          label = NULL,  
          width = "50%", 
          #placeholder = "HH:MM",
          value = (Sys.time() + 1200) %>% substring(first = 12, last = 16), ...)
      )
  }
  inputs
}

shinyInputWeight = function(FUN, len, id, ...) {
  #validate(need(character(len)>0,message=paste("")))
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i), label = NULL, width = "50%",  ...))
  }
  inputs
}


### storing reactive values (observEvent can access it)
### record_weights dataframe is stored as reactive value 
values <- reactiveValues(data = NULL)
values$data <- as.data.frame(record_weights)


### ExportWeights is the reactive with the shinyInput() fields
### constructed as a data frame with a current_weight for adding new values
ExportWeights <- reactive({
  data.frame(
    values$data,
    current_weight = shinyInputWeight(FUN = textInput, len = nrow(values$data), id = "cbox_"),
    water_start = shinyInputWaterStart(FUN = textInput, len = nrow(values$data), id = "cbox_ws"),
    water_end = shinyInputWaterEnd(FUN = textInput, len = nrow(values$data), id = "cbox_we")
  )
})



### if the Save button is pressed 
observeEvent(
  eventExpr = input$save,
  handlerExpr = {
    
    #out_col <- character(nrow(values$data))
    out_col_weight <- list()
    out_col_water_start <- list()
    out_col_water_end <- list()
    
    for (i in seq_len(nrow(values$data)) ) {
      
      out_col_weight[[i]] <- input[[paste0("cbox_", i)]]
      out_col_water_start[[i]] <- input[[paste0("cbox_ws", i)]]
      out_col_water_end[[i]] <- input[[paste0("cbox_we", i)]]
      
    }
    
    out_df <- cbind(
      values$data, ### record_weights reactive
      "Current weight (g)" = out_col_weight %>% unlist(), ### list of entries named as "Current weight (g)" 
      "Water start" = out_col_water_start %>% unlist(),
      "Water end" = out_col_water_end %>% unlist()
      )
    
    
    
    write.csv(out_df, "test.csv", row.names = FALSE)
    
  })


### if the Add button is pressed 

# observeEvent(
#   eventExpr = input$timestartadd,
#   
#   handlerExpr = {
#   x <- input$timestart
#   
#   out_col_water_start <- list()
#   
#   for (i in seq_len(nrow(values$data)) ) {
#     out_col_water_start[[i]] <- input[[paste0("cbox_ws", i)]]
#   }
#   
#   }
# )
# 



output$mass_rec_table <- DT::renderDataTable({
  
  ExportWeights() %>% 
    dplyr::rename(
      "Lab ID" = animal_id, 
      "NRF ID" = nrf_id,
      "PIL holder" = experimenter,
      "Cage" = cage,
      "Last measured weight (g)" = mass,
      "Water start" = water_start,
      "Water end" = water_end,
      "Current weight (g)" = current_weight
      
      ) %>% 
    datatable(
      selection = "multiple",
      escape = FALSE,
      options = list(
        pageLength = -1, ### important to show the whole table otherwise saving gives an error (in the for loop in the observEvent()) 
        dom = 't',
        # autoWidth=TRUE,
        # lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
        # ColReorder = TRUE,s
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
  
  # formattable::formattable()
})

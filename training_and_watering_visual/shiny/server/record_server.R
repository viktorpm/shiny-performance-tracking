
### defining shinyInput helper function
shinyInput = function(FUN, len, id, ...) {
  #validate(need(character(len)>0,message=paste("")))
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
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
    current_weight = shinyInput(FUN = textInput, len = nrow(values$data), id = "cbox_")
  )
})



### if the save buton is pressed 
observeEvent(
  eventExpr = input$save,
  handlerExpr = {
    
    #out_col <- character(nrow(values$data))
    out_col <- list()
    
    for (i in seq_len(nrow(values$data)) ) {
      
      out_col[[i]] <- input[[paste0("cbox_", i)]]
      
    }
    out_df <- cbind(values$data,out_col %>% unlist() )
    
    
    
    write.csv(out_df, "test.csv", row.names = FALSE)
    
  })



output$mass_rec_table <- DT::renderDataTable({
  
  ExportWeights() %>% 
    # dplyr::filter(
    #   experimenter == input$exp_select) %>%
    dplyr::rename(
      "Lab ID" = animal_id, 
      "NRF ID" = nrf_id,
      "PIL holder" = experimenter,
      "Cage" = cage,
      "Last measured weight (g)" = mass,
      "Current weight (g)" = current_weight) %>% 
    datatable(
      selection="multiple",
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

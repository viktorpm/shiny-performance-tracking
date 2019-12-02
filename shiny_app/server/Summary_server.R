### @AthenaDelayComp Summary panel ----


create_plot_sum <- reactive({
  plots_summary(
    plottype_sum = input$plot_type_sum,
    datelim_sum = input$setdate_sum,
    pick_date = input$date_to_sum
  )
})


output$plot_sum <- renderPlot({
  create_plot_sum()
})



output$table_sum <- DT::renderDataTable(
  bind_cols(
    TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        date == input$date_to_sum
      ) %>%
      group_by(protocol, stage) %>%
      summarize("No. animals" = length(animal_id)),
    
    
    tmp <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        date == input$date_to_sum
      ) %>%
      group_by(protocol, stage) %>%
      summarize("Animal names" = paste(animal_id, sep = "", collapse = ", ")) %>%
      pull("Animal names") %>%
      as.tibble() %>%
      rename("Aimal names" = value)
  )
)


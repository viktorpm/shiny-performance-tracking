### mass log plots and tables ----


create_plot <- reactive({
  plots_mass(
    datelim = input$setdate,
    f_options = input$f_options,
    animal_filter = input$animal_select,
    exp = input$exp_select
  )
})


output$plot <- renderPlot({
  create_plot()
})



mass_formatter <-
  formatter("span",
    style = x ~ style(
      font.weight = "bold",
      color = ifelse(
        test = c(NA, diff(as.numeric(x))) > 0, ### NA to shift it by 1 to get the correct colors
        yes = "green",
        no = ifelse(c(NA, diff(as.numeric(x))) < 0, "red", "black")
      )
    ),
    x ~ icontext(ifelse(
      test = c(NA, diff(as.numeric(x))) > 0,
      yes = "arrow-up",
      no = ifelse(c(NA, diff(as.numeric(x))) < 0, "arrow-down", "minus")
    ), x)
  )



### defining shinyInput helper function
shinyInput = function(FUN, len, id, ...) {
  #validate(need(character(len)>0,message=paste("")))
  inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

values <- reactiveValues(data = NULL)

values$data <- as.data.frame(record_weights)


ExportWeights <- reactive({
  data.frame(
    values$data,
    current_weight = shinyInput(textInput, nrow(values$data), "cbox_")
  )
})


observeEvent(
  eventExpr = input$save,
  handlerExpr = {
  
    out_col <- character(nrow(values$data))

    for (i in seq_len(nrow(values$data))) {

      out_col[i] <- input[[paste0("cbox_", i)]]

    }
    out_df <- cbind(values$data,out_col)
    


    write.csv(out_df, "test.csv", row.names = FALSE)
  
})





observe({
  if (input$f_options == "Experimenter") {
    disable("animal_select")

    output$mass_table <- DT::renderDataTable({
      mass %>%
        dplyr::filter(
          # animal_id == input$animal_select,
          exp_id == input$exp_select,
          date >= input$setdate[1],
          date <= input$setdate[2]
        ) %>%
        dplyr::arrange(animal_id, date) %>%
        dplyr::rename("Animal" = animal_id, "Experimenter" = exp_id, "Date" = date, "Mass (g)" = mass) %>%
        formattable::formattable(
          align = c("c", "c", "r", "r"),
          list(
            `Mass (g)` = mass_formatter
          )
        ) %>%
        as.datatable(
          extensions = "Buttons",
          options = list(
            pageLength = 20,
            buttons = c("copy", "csv", "excel"),
            dom = "Bfrtip"
          ),
          class = "display"
        )
    })
  }
  
  output$mass_rec_table <- DT::renderDataTable({
    datatable(
      ExportWeights(),
      selection="multiple",
      escape = FALSE,
      options = list(
        # dom = 'BRrltpi',
        # autoWidth=TRUE,
        # lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
        # ColReorder = TRUE,s
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
      )
  })



  if (input$f_options == "Individual animals") {
    enable("animal_select")

    x <- mass %>%
      dplyr::filter(exp_id == input$exp_select) %>%
      pull(animal_id) %>%
      unique()


    updateSelectInput(session,
      inputId = "animal_select",
      label = "Select animals to show",
      choices = x,
      selected = x[1]
    )


    output$mass_table <- DT::renderDataTable({
      mass %>%
        dplyr::filter(
          animal_id == input$animal_select,
          exp_id == input$exp_select,
          date >= input$setdate[1],
          date <= input$setdate[2]
        ) %>%
        dplyr::arrange(date) %>%
        dplyr::rename("Animal" = animal_id, "Experimenter" = exp_id, "Date" = date, "Mass (g)" = mass) %>%
        formattable::formattable(
          align = c("c", "c", "r", "r"),
          list(
            `Mass (g)` = mass_formatter
          )
        ) %>%
        as.datatable(
          extensions = "Buttons",
          options = list(
            pageLength = 20,
            buttons = c("copy", "csv", "excel"),
            dom = "Bfrtip"
          ),
          class = "display"
        )
    })
  }
})

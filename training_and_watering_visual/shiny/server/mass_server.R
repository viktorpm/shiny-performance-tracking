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

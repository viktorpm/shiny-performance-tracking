### @SoundCategorization plots ----


create_plot_SC <- reactive({
  plots_SoundCateg(
    prot_SC = input$protocol_SC,
    plottype_SC = input$plot_type_SC,
    datelim_SC = input$setdate_SC,
    stage_filter_SC = input$stage_SC,
    animal_filter_SC = input$animal_select_SC,
    exp_SC = input$exp_select_SC,
    f_options_SC = input$f_options_SC
  )
})


output$plot_SC <- renderPlot({
  create_plot_SC()
})


observe({
  if (input$plot_type_SC == "Choice direction") {
    disable("animal_select_SC")
    disable("exp_select_SC")
    disable("f_options_SC")
    disable("report_SC")
    hide("perform_SC")
  } else {
    enable("animal_select_SC")
    enable("exp_select_SC")
    enable("f_options_SC")
    enable("report_SC")

    if (input$f_options_SC == "All animals") {
      disable("animal_select_SC")
      disable("exp_select_SC")
      disable("report_SC")
      hide("perform_SC")
    }

    if (input$f_options_SC == "Experimenter") {
      enable("exp_select_SC")
      disable("animal_select_SC")
      enable("report_SC")
      hide("perform_SC")
    }

    if (input$f_options_SC == "Individual animals") {
      enable("animal_select_SC")
      disable("exp_select_SC")
      show("perform_SC")
      disable("report_SC")

      output$perform_SC <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_SC,
            choice_direction == "right_trials",
            protocol == "@SoundCategorization",
            date >= input$setdate_SC[1],
            date <= input$setdate_SC[2]
          ) %>%
          select(
            date,
            all_trials,
            completed_trials,
            correct_trials,
            error_trials,
            violation_trials,
            timeoout_trials
          ) %>%
          mutate(sum = correct_trials + error_trials + violation_trials + timeoout_trials) %>%
          mutate(difference = all_trials - sum) %>%
          mutate("correct_ratio (correct/completed)" = (correct_trials / completed_trials) %>%
            round(2)) %>%
          mutate("violation_ratio (violation/done)" = (violation_trials / all_trials) %>%
            round(2))
      )
    }
  }
})






### @SoundCategorization generate report ----

output$report_SC <- downloadHandler(
  filename = "weekly_report_SC.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "weekly_report_SC.Rmd") %>%
      normalizePath()
    file.copy(from = "weekly_report_SC.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params_SC <- list(
      exp_SC = input$exp_select_SC,
      stg_SC = input$stage_SC,
      fil_SC = input$f_options_SC,
      ani_SC = input$animal_select_SC,
      dt_SC = input$setdate_SC
    )
    rmarkdown::render(
      input = tempReport,
      output_file = file,
      params = params_SC,
      envir = new.env(parent = globalenv())
    )
  }
)

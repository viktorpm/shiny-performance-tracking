### @AltSoundCategorizationCatch plots ----


create_plot_ASCC <- reactive({
  plots_AltSoundCategCatch(
    plottype_ASCC = input$plot_type_ASCC,
    datelim_ASCC = input$setdate_ASCC,
    stage_filter_ASCC = input$stage_ASCC,
    animal_filter_ASCC = input$animal_select_ASCC,
    exp_ASCC = input$exp_select_ASCC,
    f_options_ASCC = input$f_options_ASCC
  )
})


output$plot_ASCC <- renderPlot({
  create_plot_ASCC()
})


observe({
  if (input$plot_type_ASCC == "Choice direction") {
    disable("animal_select_ASCC")
    disable("exp_select_ASCC")
    disable("f_options_ASCC")
    disable("report_ASCC")
    hide("perform_ASCC")
  } else {
    enable("animal_select_ASCC")
    enable("exp_select_ASCC")
    enable("f_options_ASCC")
    enable("report_ASCC")

    if (input$f_options_ASCC == "All animals") {
      disable("animal_select_ASCC")
      disable("exp_select_ASCC")
      disable("report_ASCC")
      hide("perform_ASCC")
    }

    if (input$f_options_ASCC == "Experimenter") {
      enable("exp_select_ASCC")
      disable("animal_select_ASCC")
      enable("report_ASCC")
      hide("perform_ASCC")
    }

    if (input$f_options_ASCC == "Individual animals") {
      enable("animal_select_ASCC")
      disable("exp_select_ASCC")
      show("perform_ASCC")
      disable("report_ASCC")

      output$perform_ASCC <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_ASCC,
            choice_direction == "right_trials",
            protocol == "@AltSoundCategorizationCatch",
            date >= input$setdate_ASCC[1],
            date <= input$setdate_ASCC[2]
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






### @AltSoundCategorizationCatch generate report ----

output$report_ASCC <- downloadHandler(
  filename = "weekly_report_ASCC.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "weekly_report_ASCCatch.Rmd") %>%
      normalizePath()
    file.copy(from = "weekly_report_ASCCatch.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params_ASCC <- list(
      exp_ASCC = input$exp_select_ASCC,
      stg_ASCC = input$stage_ASCC,
      fil_ASCC = input$f_options_ASCC,
      ani_ASCC = input$animal_select_ASCC,
      dt_ASCC = input$setdate_ASCC
    )
    rmarkdown::render(
      input = tempReport,
      output_file = file,
      params = params_ASCC,
      envir = new.env(parent = globalenv())
    )
  }
)

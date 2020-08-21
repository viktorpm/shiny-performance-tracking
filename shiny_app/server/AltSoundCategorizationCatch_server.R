### @AltSoundCategorizationCatch plots ----


create_plot_ASC <- reactive({
  plots_AltSoundCategCatch(
    plottype_ASC = input$plot_type_ASC,
    datelim_ASC = input$setdate_ASC,
    stage_filter_ASC = input$stage_ASC,
    animal_filter_ASC = input$animal_select_ASC,
    exp_ASC = input$exp_select_ASC,
    f_options_ASC = input$f_options_ASC
  )
})


output$plot_ASC <- renderPlot({
  create_plot_ASC()
})


observe({
  if (input$plot_type_ASC == "Choice direction") {
    disable("animal_select_ASC")
    disable("exp_select_ASC")
    disable("f_options_ASC")
    disable("report_ASC")
    hide("perform_ASC")
  } else {
    enable("animal_select_ASC")
    enable("exp_select_ASC")
    enable("f_options_ASC")
    enable("report_ASC")

    if (input$f_options_ASC == "All animals") {
      disable("animal_select_ASC")
      disable("exp_select_ASC")
      disable("report_ASC")
      hide("perform_ASC")
    }

    if (input$f_options_ASC == "Experimenter") {
      enable("exp_select_ASC")
      disable("animal_select_ASC")
      enable("report_ASC")
      hide("perform_ASC")
    }

    if (input$f_options_ASC == "Individual animals") {
      enable("animal_select_ASC")
      disable("exp_select_ASC")
      show("perform_ASC")
      disable("report_ASC")

      output$perform_ASC <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_ASC,
            choice_direction == "right_trials",
            protocol == "@AltSoundCategorizationCatch",
            date >= input$setdate_ASC[1],
            date <= input$setdate_ASC[2]
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

output$report_ASC <- downloadHandler(
  filename = "weekly_report_ASC.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "weekly_report_ASCCatch.Rmd") %>%
      normalizePath()
    file.copy(from = "weekly_report_ASCCatch.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params_ASC <- list(
      exp_ASC = input$exp_select_ASC,
      stg_ASC = input$stage_ASC,
      fil_ASC = input$f_options_ASC,
      ani_ASC = input$animal_select_ASC,
      dt_ASC = input$setdate_ASC
    )
    rmarkdown::render(
      input = tempReport,
      output_file = file,
      params = params_ASC,
      envir = new.env(parent = globalenv())
    )
  }
)

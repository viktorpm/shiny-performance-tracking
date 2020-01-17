### @AthenaDelayComp plots and tables ----


create_plot_AS <- reactive({
  # isolate({
  plots_AltStat(
    plottype_AS = input$plot_type_AS,
    datelim_AS = input$setdate_AS,
    stage_filter_AS = input$stage_AS,
    f_options_AS = input$f_options_AS,
    animal_filter_AS = input$animal_select_AS,
    exp_AS = input$exp_select_AS
  )
  # })
})


output$plot_AS <- renderPlot({
  create_plot_AS()
})



observe({
  if (input$plot_type_AS == "Choice direction") {
    disable("animal_select_AS")
    disable("exp_select_AS")
    disable("f_options_AS")
    disable("report_AS")
    hide("perform_AS")
  } else {
    enable("animal_select_AS")
    enable("exp_select_AS")
    enable("f_options_AS")
    enable("report_AS")

    if (input$f_options_AS == "All animals") {
      disable("animal_select_AS")
      disable("exp_select_AS")
      disable("report_AS")
      hide("perform_AS")
    }

    if (input$f_options_AS == "Experimenter") {
      enable("exp_select_AS")
      enable("report_AS")
      disable("animal_select_AS")
      hide("perform_AS")
    }

    if (input$f_options_AS == "Individual animals") {
      enable("animal_select_AS")
      disable("exp_select_AS")
      disable("report_AS")
      show("perform_AS")

      output$perform_AS <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_AS,
            choice_direction == "right_trials",
            protocol == "@AltStatDelayComp",
            date >= input$setdate_AS[1],
            date <= input$setdate_AS[2]
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



### @AthenaDelayComp generate report ----

output$report_AS <- downloadHandler(
  filename = "weekly_report_AS.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "weekly_report_AS.Rmd") %>%
      normalizePath()
    file.copy(from = "weekly_report_AS.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params_AS <- list(
      exp_AS = input$exp_select_AS,
      stg_AS = input$stage_AS,
      fil_AS = input$f_options_AS,
      ani_AS = input$animal_select_AS,
      dt_AS = input$setdate_AS
    )
    rmarkdown::render(
      input = tempReport,
      output_file = file,
      params = params_AS,
      envir = new.env(parent = globalenv())
    )
  }
)

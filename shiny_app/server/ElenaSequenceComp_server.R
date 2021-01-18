### @ElenaSequenceComp plots and tables ----


create_plot_ESC <- reactive({
  plots_SequenceComp(
    plottype_ESC = input$plot_type_ESC,
    datelim_ESC = input$setdate_ESC,
    stage_filter_ESC = input$stage_ESC,
    f_options_ESC = input$f_options_ESC,
    animal_filter_ESC = input$animal_select_ESC,
    exp_ESC = input$exp_select_ESC
  )
})


output$plot_ESC <- renderPlot({
  create_plot_ESC()
})


observe({
  if (input$plot_type_ESC == "Choice direction") {
    disable("animal_select_ESC")
    disable("exp_select_ESC")
    disable("f_options_ESC")
    disable("report_ESC")
    hide("perform_ESC")
  } else {
    enable("animal_select_ESC")
    enable("exp_select_ESC")
    enable("f_options_ESC")
    enable("report_ESC")

    if (input$f_options_ESC == "All animals") {
      disable("animal_select_ESC")
      disable("exp_select_ESC")
      disable("report_ESC")
      hide("perform_ESC")
    }

    if (input$f_options_ESC == "Experimenter") {
      enable("exp_select_ESC")
      enable("report_ESC")
      disable("animal_select_ESC")
      hide("perform_ESC")
    }

    if (input$f_options_ESC == "Individual animals") {
      enable("animal_select_ESC")
      disable("exp_select_ESC")
      disable("report_ESC")
      show("perform_ESC")

      output$perform_ESC <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_ESC,
            choice_direction == "right_trials",
            protocol == "@ElenaSequenceComp",
            date >= input$setdate_ESC[1],
            date <= input$setdate_ESC[2]
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


### @ElenaSequenceComp generate report ----

output$report <- downloadHandler(
  filename = "weekly_report.html",
  content = function(file){
    tempReport <- file.path(tempdir(), "weekly_report.Rmd") %>%
      normalizePath()
    file.copy(from = "weekly_report.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params <- list(exp = input$exp_select_ESC,
                   stg = input$stage_ESC,
                   fil = input$f_options_ESC,
                   ani = input$animal_select_ESC,
                   dt = input$setdate_ESC)
    rmarkdown::render(input = tempReport,
                      output_file = file,
                      params = params_ESC,
                      envir = new.env(parent = globalenv())
    )

  }
)

### @SoundCatContinuous plots ----


create_plot_SCCONT <- reactive({
  plots_SoundCateg(
    plottype_SCCONT = input$plot_type_SCCONT,
    datelim_SCCONT = input$setdate_SCCONT,
    stage_filter_SCCONT = input$stage_SCCONT,
    animal_filter_SCCONT = input$animal_select_SCCONT,
    exp_SCCONT = input$exp_select_SCCONT,
    f_options_SCCONT = input$f_options_SCCONT
  )
})


output$plot_SCCONT <- renderPlot({
  create_plot_SCCONT()
})


observe({
  if (input$plot_type_SCCONT == "Choice direction") {
    disable("animal_select_SCCONT")
    disable("exp_select_SCCONT")
    disable("f_options_SCCONT")
    disable("report_SCCONT")
    hide("perform_SCCONT")
  } else {
    enable("animal_select_SCCONT")
    enable("exp_select_SCCONT")
    enable("f_options_SCCONT")
    enable("report_SCCONT")
    
    if (input$f_options_SCCONT == "All animals") {
      disable("animal_select_SCCONT")
      disable("exp_select_SCCONT")
      disable("report_SCCONT")
      hide("perform_SCCONT")
    }
    
    if (input$f_options_SCCONT == "Experimenter") {
      enable("exp_select_SCCONT")
      disable("animal_select_SCCONT")
      enable("report_SCCONT")
      hide("perform_SCCONT")
    }
    
    if (input$f_options_SCCONT == "Individual animals") {
      enable("animal_select_SCCONT")
      disable("exp_select_SCCONT")
      show("perform_SCCONT")
      disable("report_SCCONT")
      
      output$perform_SCCONT <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select_SCCONT,
            choice_direction == "right_trials",
            protocol == "@SoundCatContinuous",
            date >= input$setdate_SCCONT[1],
            date <= input$setdate_SCCONT[2]
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






### @SoundCatContinuous generate report ----

output$report_SCCONT <- downloadHandler(
  filename = "weekly_report_SCCONT.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "weekly_report_SCCONT.Rmd") %>%
      normalizePath()
    file.copy(from = "weekly_report_SCCONT.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params_SCCONT <- list(
      exp_SCCONT = input$exp_select_SCCONT,
      stg_SCCONT = input$stage_SCCONT,
      fil_SCCONT = input$f_options_SCCONT,
      ani_SCCONT = input$animal_select_SCCONT,
      dt_SCCONT = input$setdate_SCCONT
    )
    rmarkdown::render(
      input = tempReport,
      output_file = file,
      params = params_SCCONT,
      envir = new.env(parent = globalenv())
    )
  }
)

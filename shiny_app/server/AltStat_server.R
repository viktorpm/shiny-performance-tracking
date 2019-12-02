### @AthenaDelayComp plots and tables ----


create_plot <- reactive({
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
  create_plot()
})



observe({
  if (input$plot_type == "Choice direction") {
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
    
    if (input$f_options == "All animals") {
      disable("animal_select_AS")
      disable("exp_select_AS")
      disable("report_AS")
      hide("perform_AS")
    }
    
    if (input$f_options == "Experimenter") {
      enable("exp_select_AS")
      enable("report_AS")
      disable("animal_select_AS")
      hide("perform_AS")
    }
    
    if (input$f_options == "Individual animals") {
      enable("animal_select_AS")
      disable("exp_select_AS")
      disable("report_AS")
      show("perform_AS")
      
      output$perform <- DT::renderDataTable(
        TRAINING %>%
          dplyr::filter(
            animal_id == input$animal_select,
            choice_direction == "right_trials",
            protocol == "@AthenaDelayComp",
            date >= input$setdate[1],
            date <= input$setdate[2]
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

output$report <- downloadHandler(
  filename = "weekly_report.html",
  content = function(file){
    tempReport <- file.path(tempdir(), "weekly_report.Rmd") %>% 
      normalizePath()
    file.copy(from = "weekly_report.Rmd", to = tempReport, overwrite = T)
    library(rmarkdown)
    params <- list(exp = input$exp_select,
                   stg = input$stage,
                   fil = input$f_options,
                   ani = input$animal_select,
                   dt = input$setdate)
    rmarkdown::render(input = tempReport,
                      output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
    
  }
)


### @AthenaDelayComp plots and tables ----


create_plot <- reactive({
  # isolate({
  plots_DelayComp(
    plottype = input$plot_type,
    datelim = input$setdate,
    stage_filter = input$stage,
    f_options = input$f_options,
    animal_filter = input$animal_select,
    exp = input$exp_select
  )
  # })
})


output$plot <- renderPlot({
  create_plot()
})



observe({
  if (input$plot_type == "Choice direction") {
    disable("animal_select")
    disable("exp_select")
    disable("f_options")
    disable("report")
    hide("perform")
  } else {
    enable("animal_select")
    enable("exp_select")
    enable("f_options")
    enable("report")
    
    if (input$f_options == "All animals") {
      disable("animal_select")
      disable("exp_select")
      disable("report")
      hide("perform")
    }
    
    if (input$f_options == "Experimenter") {
      enable("exp_select")
      enable("report")
      disable("animal_select")
      hide("perform")
    }
    
    if (input$f_options == "Individual animals") {
      enable("animal_select")
      disable("exp_select")
      disable("report")
      show("perform")
      
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


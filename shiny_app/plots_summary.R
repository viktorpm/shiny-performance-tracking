plots_summary <- function(plottype_sum,
                          datelim_sum,
                          pick_date) {

  #  browser()

  #############################
  ### Filtering data table ----
  #############################


  TRAINING <- TRAINING %>%
    dplyr::filter(
      choice_direction == "right_trials",
      # stage %in% stage_filter_sum,
      date >= datelim_sum[1], date <= datelim_sum[2]
    )

  col_by <- "animal_id"
  col_lab_name <- "Animals"
  lines <- geom_line(aes(col = eval(parse(text = col_by))),
    linetype = "dashed",
    alpha = 0.4
  )




  #############################
  ### PLOT: stage tracking ----
  #############################


  if (plottype_sum == "Stage tracking") {
    stage_plot <- ggplot(
      data = TRAINING,
      mapping = aes(x = date, y = animal_id)
    ) +
      geom_point(aes(col = stage), size = 6) +

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_sum[1]), as.Date(datelim_sum[2]))
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      xlab("Date [day]") +
      ylab("Animals") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(date == pick_date),
        mapping = aes(label = animal_id),
        direction = "y",
        hjust = -0.5
      ) +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(date == pick_date),
        mapping = aes(label = protocol),
        direction = "y",
        hjust = 1.3,
        vjust = 1
      ) +


      labs(col = "Stage")
    
    #ggplotly(stage_plot)

     plot(stage_plot)
  }



  ############################
  ### PLOT: Missing data ----
  ############################

  if (plottype_sum == "Missing data") {
    recording_dates <- TRAINING %>%
      group_by(animal_id) %>%
      mutate(trained = T) %>%
      pad(start_val = TRAINING$date %>% min(), end_val = TRAINING$date %>% max()) %>%
      mutate(day_name = weekdays(date)) %>%
      mutate(weekend = is.weekend(date)) %>%
      select(date, day_name, trained, rig) %>%
      mutate(trained = replace(trained, is.na(trained), F)) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(rig = which(rigs_sessions == animal_id, arr.ind = T)[1]) %>%
      mutate(session = which(rigs_sessions == animal_id, arr.ind = T)[2]) %>%
      ungroup()

    missing_plot <- ggplot(
      data = recording_dates,
      mapping = aes(x = date, y = fct_reorder(animal_id, rig))
    ) +
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_sum[1]), as.Date(datelim_sum[2]))
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      xlab("Date [day]") +
      ylab("Animals") +
      geom_point(aes(size = trained, col = trained)) +
      geom_label_repel(
        data = recording_dates %>%
          dplyr::filter(date == pick_date),
        mapping = aes(label = rig, fill = as.character(rig)),
        direction = "y",
        hjust = -1
      ) +
      labs(fill = "Rig") +
      labs(col = "Training status") +
      labs(size = "Training status")

    plot(missing_plot)
  }
}

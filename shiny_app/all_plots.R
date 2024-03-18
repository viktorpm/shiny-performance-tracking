# Function to generate summary plots based on specified parameters
# This function filters the TRAINING dataset based on user-specified criteria
# and generates either a stage tracking plot or a missing data plot.
summary_plots <- function(plottype_sum,
                          datelim_sum,
                          stage_filter_sum,
                          animal_filter_sum,
                          all_animals_sum) {
  
  # Filtering data table based on parameters
  # If all animals are plotted, filter by choice_direction, stage, and date range
  if (all_animals_sum == T) {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_sum,
        date >= datelim_sum[1], date <= datelim_sum[2]
      )
    
    # Set variables for plotting
    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
                       linetype = "dashed",
                       alpha = 0.4
    )
  } else {
    # If only one selected animal is plotted, add animal_id to the filter
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_sum,
        date >= datelim_sum[1], date <= datelim_sum[2],
        animal_id == animal_filter_sum
      )
    
    # Set variables for plotting
    col_by <- "stage"
    col_lab_name <- "Stages"
    lines <- geom_line(linetype = "dashed", alpha = 0.4)
  }
  
  # Stage tracking plot
  if (plottype_sum == "Stage tracking") {
    stage_plot <- ggplot(
      data = TRAINING,
      mapping = aes(x = date, y = animal_id)
    ) +
      geom_point(aes(col = stage), size = 6) +
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
          dplyr::filter(date == max(date)),
        mapping = aes(label = animal_id),
        direction = "y",
        hjust = -0.5
      ) +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(date == max(date) - 1),
        mapping = aes(label = protocol),
        direction = "y",
        hjust = 1.3,
        vjust = 1
      ) +
      labs(col = "Stage")
    
    plot(stage_plot)
  }
  
  # Missing data plot
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
        limits = c(as.Date(datelim[1]), as.Date(datelim[2]))
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
          dplyr::filter(date == max(date)),
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

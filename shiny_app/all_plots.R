all_plots <- function(plottype, datelim, stage_filter, animal_filter, all_animals) {

  #  browser()

  #############################
  ### Filtering data table ----
  #############################
  
  if (all_animals == T) {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter,
        date >= datelim[1], date <= datelim[2]
      )
  } else {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter,
        date >= datelim[1], date <= datelim[2],
        animal_id == animal_filter
      )
  }




  ##########################
  ### PLOT: CP duration ----
  ##########################

  if (plottype == "CP duration") {
    cp_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        col = animal_id,
        x = date,
        y = total_CP # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      geom_line(linetype = "dashed", alpha = 0.4) +
      geom_point(size = 3) +

      ### scales, labels, themes
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
      ylab(paste0("CP duration [s] ", stage_filter %>% paste(collapse = ", "))) +
      xlab("Date [day]") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(
            date == max(date)
          ),

        mapping = aes(label = animal_id, col = animal_id),

        hjust = -0.9,
        direction = "y"
      )

    plot(cp_plot)
  }

  
  
  
  ##########################
  ### PLOT: done trials ----
  ##########################

  if (plottype == "No. done trials") {
    trial_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = done_trials / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      geom_line(
        mapping = aes(col = animal_id),
        linetype = "dashed",
        alpha = 0.4
      ) +
      geom_point(mapping = aes(col = animal_id), size = 3) +

      ### scales, labels, themes
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
      ylab(paste0("Norm. No. done trials ", stage_filter %>% paste(collapse = ", "))) +
      xlab("Date [day]") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(
            date == max(date)
          ),
        mapping = aes(label = animal_id, col = animal_id),

        hjust = -0.5,
        direction = "y"
      )

    plot(trial_plot)
  }

  
  
  
  #############################
  ### PLOT: stage tracking ----
  #############################
  
  
  
  
  scale_fill_viktor <- function(...) {
    ggplot2:::manual_scale(
      "col",
      values = setNames(c("#F8766D", "#00BFC4", "#7CAE00"), 
                        c("0_side_poke_on", "1_center_poke_on", "2_intord_stim")), 
      ...
    )
  }
  

  if (plottype == "Stage tracking") {
    stage_plot <- ggplot(
      data = TRAINING,
      mapping = aes(x = date, y = animal_id, col = stage)
    ) +
      geom_point(aes(col = stage), size = 6) +
      
      ### scales, labels, themes
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
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(date == max(date)),
        mapping = aes(label = animal_id),
        direction = "y",
        hjust = -0.5
      ) +
      scale_fill_viktor()

    plot(stage_plot)
  }

  
  
  
  ############################
  ### PLOT: Missing data ----
  ############################
  
  if (plottype == "Missing data") {
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
      labs(fill = "Rig")

    plot(missing_plot)
  }
}

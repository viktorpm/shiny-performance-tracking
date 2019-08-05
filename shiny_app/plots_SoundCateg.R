plots_SoundCateg <- function(plottype_SC,
                             datelim_SC,
                             stage_filter_SC,
                             animal_filter_SC,
                             exp_SC,
                             f_options_SC) {


  #############################
  ### Filtering data table ----
  #############################


  if (f_options_SC == "All animals") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_SC,
        date >= datelim_SC[1], date <= datelim_SC[2],
        protocol == "@SoundCategorization"
      )

    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }

  if (f_options_SC == "Experimenter") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_SC,
        date >= datelim_SC[1], date <= datelim_SC[2],
        protocol == "@SoundCategorization",
        experimenter == exp_SC
      )

    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }


  if (f_options_SC == "Individual animals") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_SC,
        date >= datelim_SC[1], date <= datelim_SC[2],
        protocol == "@SoundCategorization",
        animal_id == animal_filter_SC
      )

    col_by <- "stage"
    col_lab_name <- "Stages"
    lines <- geom_line(linetype = "dashed", alpha = 0.4)
  }





  ##########################
  ### PLOT: CP duration ----
  ##########################

  if (plottype_SC == "CP duration") {
    cp_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = total_CP # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      lines +
      geom_point(aes(col = eval(parse(text = col_by))),
        size = 3
      ) +

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_SC[1]), as.Date(datelim_SC[2]))
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab(paste0("CP duration [s] ", stage_filter_SC %>% paste(collapse = ", "))) +
      xlab("Date [day]") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(
            date == max(date)
          ),

        mapping = aes(label = animal_id, col = eval(parse(text = col_by))),

        hjust = -0.9,
        direction = "y"
      ) +
      labs(col = eval(parse(text = "col_lab_name")))

    plot(cp_plot)
  }




  ##########################
  ### PLOT: done trials ----
  ##########################

  if (plottype_SC == "No. done trials") {
    trial_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = done_trials # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      lines +
      geom_point(
        mapping = aes(col = eval(parse(text = col_by))),
        size = 3
      ) +

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_SC[1]), as.Date(datelim_SC[2]))
      ) +
      ylim(0, max(TRAINING$done_trials) + 10) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab(paste0("No. done trials ", stage_filter_SC %>% paste(collapse = ", "))) +
      xlab("Date [day]") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(
            date == max(date)
          ),
        mapping = aes(label = animal_id, col = eval(parse(text = col_by))),

        hjust = -0.5,
        direction = "y"
      ) +
      labs(col = eval(parse(text = "col_lab_name")))

    plot(trial_plot)
  }


  ###############################
  ### PLOT: completed trials ----
  ###############################

  if (plottype_SC == "No. completed trials") {
    trial_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = completed_trials # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      lines +
      geom_point(
        mapping = aes(col = eval(parse(text = col_by))),
        size = 3
      ) +

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_SC[1]), as.Date(datelim_SC[2]))
      ) +
      ylim(0, max(TRAINING$done_trials) + 10) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab(paste0("No. completed trials", stage_filter_SC %>% paste(collapse = ", "))) +
      xlab("Date [day]") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(
            date == max(date)
          ),
        mapping = aes(label = animal_id, col = eval(parse(text = col_by))),

        hjust = -0.5,
        direction = "y"
      ) +
      labs(col = eval(parse(text = "col_lab_name")))

    plot(trial_plot)
  }




  #############################
  ### PLOT: correct trials ----
  #############################

  if (plottype_SC == "No. correct trials") {
    trial_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = correct_trials # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      lines +
      geom_point(
        mapping = aes(col = eval(parse(text = col_by))),
        size = 3
      ) +

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_SC[1]), as.Date(datelim_SC[2]))
      ) +
      ylim(0, max(TRAINING$done_trials) + 10) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab(paste0("No. correct trials", stage_filter_SC %>% paste(collapse = ", "))) +
      xlab("Date [day]") +
      geom_label_repel(
        data = TRAINING %>%
          dplyr::filter(
            date == max(date)
          ),
        mapping = aes(label = animal_id, col = eval(parse(text = col_by))),

        hjust = -0.5,
        direction = "y"
      ) +
      labs(col = eval(parse(text = "col_lab_name")))

    plot(trial_plot)
  }





  #############################
  ### PLOT: stage tracking ----
  #############################

  # scale_fill_viktor <- function(...) {
  #   ggplot2:::manual_scale(
  #     "col",
  #     values = setNames(
  #       c("#F8766D", "#00BFC4", "#7CAE00"),
  #       c("0_side_poke_on", "1_center_poke_on", "2_intord_stim")
  #     ),
  #     ...
  #   )
  # }


  if (plottype_SC == "Stage tracking") {
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
        limits = c(as.Date(datelim_SC[1]), as.Date(datelim_SC[2]))
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
      # geom_label_repel(
      #   data = TRAINING %>%
      #     dplyr::filter(date == max(date) - 1),
      #   mapping = aes(label = protocol),
      #   direction = "y",
      #   hjust = 1.3,
      #   vjust = 1
      # ) +

      # scale_fill_viktor() +
      labs(col = "Stage")

    plot(stage_plot)
  }



  ############################
  ### PLOT: Missing data ----
  ############################

  if (plottype_SC == "Missing data") {
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
        limits = c(as.Date(datelim_SC[1]), as.Date(datelim_SC[2]))
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

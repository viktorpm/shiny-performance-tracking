plots_SequenceComp <- function(
    plottype_ESC,
    datelim_ESC,
    stage_filter_ESC,
    animal_filter_ESC,
    exp_ESC, 
    f_options_ESC) {



  # browser()
  #############################
  ### Filtering data table ----
  #############################

  TRAINING_original <- TRAINING
  
  
  if(missing(datelim_ESC)){
    datelim_ESC = c(max(TRAINING$date)-9,max(TRAINING$date))
  }
  
  

  if (f_options_ESC == "All animals") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_ESC,
        date >= datelim_ESC[1], date <= datelim_ESC[2],
        protocol == "@ElenaSequenceComp"
      )

    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }

  if (f_options_ESC == "Experimenter") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_ESC,
        date >= datelim_ESC[1], date <= datelim_ESC[2],
        protocol == "@ElenaSequenceComp",
        experimenter == exp_ESC
      )

    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }


  if (f_options_ESC == "Individual animals") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter_ESC,
        date >= datelim_ESC[1], date <= datelim_ESC[2],
        protocol == "@ElenaSequenceComp",
        animal_id == animal_filter_ESC
      )

    col_by <- "stage"
    col_lab_name <- "Stages"
    lines <- geom_line(linetype = "dashed", alpha = 0.4)
  }



  # browser()

  ##########################
  ### PLOT: CP duration ----
  ##########################

  if (plottype_ESC == "CP duration") {
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
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab("CP duration [s]") +
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




  ################################
  ### PLOT: left/right trials ----
  ################################

  if (plottype_ESC == "Choice direction") {
    direction_plot <- ggplot(
      data = TRAINING_original %>%
        dplyr::filter(
          stage %in% stage_filter_ESC,
          date >= datelim_ESC[1], date <= datelim_ESC[2],
          protocol == "@ElenaSequenceComp"
        ),
      mapping = aes(
        x = animal_id,
        y = No_pokes # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +
      
      ### boxplots, points
      geom_boxplot(aes(fill = choice_direction), alpha = 0.3) +
      geom_point(aes(group = choice_direction, col = choice_direction),
        position = position_dodge(width = 0.75),
        size = 2
      ) +
      
      ### scales, labels, themes
      ylab("No. pokes") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12, hjust = 0.05 ),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      
      ### statistics
      stat_compare_means(aes(group = choice_direction ),
                         label = "p.signif",
                         hide.ns = T)
      

    plot(direction_plot)
  }




  ##########################
  ### PLOT: done trials ----
  ##########################

  if (plottype_ESC == "No. done trials") {
    trial_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = all_trials # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      lines +
      geom_point(
        mapping = aes(col = eval(parse(text = col_by))),
        size = 3
      ) +
      
      geom_hline(yintercept = 20, col = "gray") + 
      annotate("text", x = datelim_ESC[1], y = 23, label = "Threshold - 20 trials", col = "gray") + 

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
      ) +
      ylim(0, max(TRAINING$all_trials) + 10) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab("No. done trials") +
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

  if (plottype_ESC == "No. completed trials") {
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
      
      geom_hline(yintercept = 20, col = "gray") + 
      annotate("text", x = datelim_ESC[1], y = 23, label = "Threshold - 20 trials", col = "gray") +

      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
      ) +
      ylim(0, max(TRAINING$all_trials) + 10) + # max(all_trials): comparable to the done trial plot
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab("No. completed trials") +
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
  ### PLOT: correct trials ----
  ###############################

  if (plottype_ESC == "No. correct trials") {
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
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
      ) +
      ylim(0, max(TRAINING$all_trials) + 10) + # max(all_trials): comparable to the done trial plot
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab("No. correct trials") +
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



  ############################
  ### PLOT: correct ratio ----
  ############################

  if (plottype_ESC == "Correct ratio") {
    trial_plot <- ggplot(
      data = TRAINING,
      mapping = aes(
        x = date,
        y = correct_trials / completed_trials # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
      )
    ) +

      ### lines and points
      lines +
      geom_point(
        mapping = aes(col = eval(parse(text = col_by))),
        size = 3
      ) +
      geom_hline(yintercept = 0.50, col = "gray") +
      ### scales, labels, themes
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%b %d",
        minor_breaks = "1 day",
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
      ) +
      ylim(0, 1) + # max(all_trials): comparable to the done trial plot
      theme(
        axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ylab("Correct ratio [No. correct / No. completed trials]") +
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
      annotate("text", x = datelim_ESC[1], y = 0.51, label = "Chance level", col = "gray") +
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


  if (plottype_ESC == "Stage tracking") {
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
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
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

  if (plottype_ESC == "Missing data") {
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
        limits = c(as.Date(datelim_ESC[1]), as.Date(datelim_ESC[2]))
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

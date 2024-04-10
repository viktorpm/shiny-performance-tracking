plot_generation <- function(
    plottype,
    protocol,
    datelim,
    stage_filter,
    animal_filter,
    exp,
    show) {
  
  
  #############################
  ### Filtering data table ----
  #############################

  TRAINING_original <- TRAINING


  if (missing(datelim)) {
    datelim <- c(max(TRAINING$date) - 9, max(TRAINING$date))
  }



  if (show == "All animals") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter,
        date >= datelim[1], date <= datelim[2],
        protocol == protocol
      )

    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }

  if (show == "Experimenter") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter,
        date >= datelim[1], date <= datelim[2],
        protocol == protocol,
        experimenter == exp
      )

    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }


  if (show == "Individual animals") {
    TRAINING <- TRAINING %>%
      dplyr::filter(
        choice_direction == "right_trials",
        stage %in% stage_filter,
        date >= datelim[1], date <= datelim[2],
        protocol == protocol,
        animal_id == animal_filter
      )

    col_by <- "stage"
    col_lab_name <- "Stages"
    lines <- geom_line(linetype = "dashed", alpha = 0.4)
  }

  
  ################################
  ### PLOT: left/right trials ----
  ################################
  direction_plot <- ggplot(
    data = TRAINING_original %>%
      dplyr::filter(
        stage %in% stage_filter,
        date >= datelim[1], date <= datelim[2],
        protocol == protocol
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
      axis.text.x = element_text(angle = 90, vjust = -0.001, size = 12, hjust = 0.05),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold")
    ) +

    ### statistics
    stat_compare_means(aes(group = choice_direction),
      label = "p.signif",
      hide.ns = T
    )

  if (plottype == "Choice direction") {
    plot(direction_plot)
  }


  ###############################
  ### PLOT: completed trials ----
  ###############################
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
    annotate("text", x = datelim[1], y = 23, label = "Threshold - 20 trials", col = "gray") +

    ### scales, labels, themes
    scale_x_date(
      date_breaks = "1 day",
      date_labels = "%b %d",
      minor_breaks = "1 day",
      limits = c(as.Date(datelim[1]), as.Date(datelim[2]))
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

  if (plottype == "No. completed trials") {
    plot(trial_plot)
  }


  ############################
  ### PLOT: correct ratio ----
  ############################
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
      limits = c(as.Date(datelim[1]), as.Date(datelim[2]))
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
    annotate("text", x = datelim[1], y = 0.51, label = "Chance level", col = "gray") +
    labs(col = eval(parse(text = "col_lab_name")))

  if (plottype == "Correct ratio") {
    plot(trial_plot)
  }


  #############################
  ### PLOT: stage tracking ----
  #############################
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


  if (plottype == "Stage tracking") {
    plot(stage_plot)
  }
}

CompletedTrialsPlot <- function(
    prtcl,
    datelim,
    stage_filter,
    animal_filter,
    exp,
    show) {
  if (missing(datelim)) {
    datelim <- c(max(TRAINING$date) - 9, max(TRAINING$date))
  }


  # Default: when all animals are shown
  if (show == "All animals") {
    # list of animals under the selected protocol
    animal_filter <- TRAINING %>%
      dplyr::filter(protocol == prtcl) %>%
      dplyr::select(animal_id) %>%
      unique() %>%
      pull() %>%
      as.vector()

    # list of experimenters under the selected protocol
    exp <- TRAINING %>%
      dplyr::filter(protocol == prtcl) %>%
      dplyr::select(experimenter) %>%
      unique() %>%
      pull() %>%
      as.vector()

    # Defining plot colors and line types based on filters
    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }

  # when an experimenter is selected
  if (show == "Experimenter") {
    animal_filter <- TRAINING %>%
      dplyr::filter(protocol == prtcl) %>%
      dplyr::select(animal_id) %>%
      unique() %>%
      pull() %>%
      as.vector()

    # experimenter is selected by user
    exp <- exp

    # Defining plot colors and line types based on filters
    col_by <- "animal_id"
    col_lab_name <- "Animals"
    lines <- geom_line(aes(col = eval(parse(text = col_by))),
      linetype = "dashed",
      alpha = 0.4
    )
  }

  if (show == "Individual animals") {
    animal_filter <- animal_filter
    exp <- exp

    # Defining plot colors and line types based on filters
    col_by <- "stage"
    col_lab_name <- "Stages"
    lines <- geom_line(linetype = "dashed", alpha = 0.4)
  }

  TRAINING <- TRAINING %>%
    dplyr::filter(
      date >= datelim[1], date <= datelim[2],
      protocol == prtcl,
      stage %in% stage_filter,
      animal_id %in% animal_filter,
      experimenter %in% exp,
      choice_direction == "right_trials"
    )


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
    annotate("text", x = datelim[1], y = 23, label = "Threshold - 20 trials", col = "gray", vjust = 2, hjust = -0.5) +

    ### scales, labels, themes
    scale_x_date(
      date_breaks = "1 day",
      date_labels = "%b %d",
      minor_breaks = "1 day",
      limits = c(as.Date(datelim[1]), as.Date(datelim[2]))
    ) +
    ylim(0, max(TRAINING$all_trials) + 10) + # max(all_trials): comparable to the done trial plot
    ylab("No. completed trials") +
    xlab("Date [day]") +
    ggtitle("Completed trials") +
    geom_label_repel(
      data = TRAINING %>%
        dplyr::filter(
          date == max(date)
        ),
      mapping = aes(label = animal_id, col = eval(parse(text = col_by))),
      hjust = -0.5,
      direction = "y"
    ) +
    labs(col = eval(parse(text = "col_lab_name"))) +
    plot_theme_settings()

  plot(trial_plot)
}

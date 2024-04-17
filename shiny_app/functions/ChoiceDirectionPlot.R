ChoiceDirectionPlot <- function(
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
  if (show == "All animals" ) {
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
  }
  
  # when an experimenter is selected 
  if (show == "Experimenter") {
    # list of animals under the selected protocol
    animal_filter <- TRAINING %>%
      dplyr::filter(protocol == prtcl) %>%
      dplyr::select(animal_id) %>%
      unique() %>%
      pull() %>%
      as.vector()
    
    # experimenter is selected by user
    exp <- exp
  }
  
  
  TRAINING <- TRAINING %>%
    dplyr::filter(
      date >= datelim[1], date <= datelim[2],
      protocol == prtcl,
      stage %in% stage_filter,
      animal_id %in% animal_filter,
      experimenter %in% exp
    )



  direction_plot <- ggplot(
    data = TRAINING,
    mapping = aes(
      x = animal_id,
      y = No_pokes # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
    )
  ) +

    ### box plots, points
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


  plot(direction_plot)
}

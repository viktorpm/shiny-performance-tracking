plots_mass <- function(animal_filter, exp, datelim, f_options) {
  if (f_options == "Experimenter") {
    mass %>%
      dplyr::filter(
        exp_id == exp,
        date >= datelim[1],
        date <= datelim[2]
        # animal_id == animal_filter
      ) %>%
      ggplot(mapping = aes(x = date, y = mass %>% as.numeric())) +
      geom_line(aes(color = animal_id %>% as.factor())) +
      ylim(250, 900) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      scale_color_discrete(name = "Animals") +
      ylab("Weight [g]") +
      xlab("Date")
    
    
  } else {
    mass %>%
      dplyr::filter(
        exp_id == exp,
        date >= datelim[1],
        date <= datelim[2],
        animal_id == animal_filter
      ) %>%
      ggplot(mapping = aes(x = date, y = mass %>% as.numeric())) +
      geom_line(aes(color = animal_id)) +
      ylim(250, 900) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      scale_color_discrete(name = "Animals") +
      ylab("Weight [g]") +
      xlab("Date")
  }
}

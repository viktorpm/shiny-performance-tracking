plots_mass <- function(animal_filter,
                       exp,
                       datelim,
                       f_options) {

  if (f_options == "Experimenter") {
    mass %>%
      dplyr::filter(
        exp_id == exp,
        date >= datelim[1],
        date <= datelim[2],
        animal_id == animal_filter
      ) %>%
      ggplot(mapping = aes(x = date, y = mass %>% as.numeric())) +
      geom_line(aes(color = animal_id)) +
      ylim(200,900)
  }
  
  if (f_options == "Individual animals") {
    mass %>%
      dplyr::filter(
        exp_id == exp,
        date >= datelim[1],
        date <= datelim[2],
        animal_id == animal_filter
      ) %>%
      ggplot(mapping = aes(x = date, y = mass %>% as.numeric())) +
      geom_line(aes(color = animal_id)) +
      ylim(200,900)
      
  }
  

}

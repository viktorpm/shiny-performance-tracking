plots_mass <- function (animal_filter,
                        exp,
                        datelim,
                        f_options) {
  mass %>% 
    dplyr::filter(
      exp_id == exp,
      date <= Sys.Date(),
      date >= datelim[2],
    ) %>% 
    ggplot(mapping = aes(x = date, y = mass %>% as.numeric())) + 
    geom_line(aes(color = animal_id))
}


joystick <- read_csv(file.path("D:", "joystick_protocol", "Bonsai", "Summary_statistics.csv"))


names(joystick) <- joystick %>% names() %>% tolower()
joystick <- joystick %>%
  dplyr::mutate(index = row_number()) %>% 
  tidyr::extract(
    col = session_id, 
    into = c("test", "animal", "phase", "day", "month", "year", "session"), 
    regex = "([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)")





joystick %>%
  ggplot(mapping = aes(x = index, y = trials)) +
  geom_point()


joystick %>%
  ggplot(mapping = aes(x = index, y = good_pushes/bad_pushes)) +
  geom_point()


joystick %>%
  ggplot(mapping = aes(x = index, y = final_holdtime)) +
  geom_point()

library(ggplot2)
library(ggrepel)
library(chron)

### Reading csv to tibble ----
TRAINING <- read_csv(file.path("D:", "_R_WD", "git_projects", "rat_wm_training", "output_data", "TRAINING.csv"))
TRAINING <- TRAINING %>%
  mutate(date = date %>% as.Date(format = c("%d-%b-%Y"))) %>%
  mutate(animal_id = animal_id %>% toupper()) %>%
  mutate(session_length = save_time %>% chron(times. = .) - start_time %>% chron(times. = .)) %>% 
  mutate(settings_file = settings_file %>% substr(start = nchar(.) - 10, stop = nchar(.) - 4)) %>%
  gather(right_trials, left_trials, key = "choice_direction", value = "No_pokes")


TRAINING$session_length %>% hist(breaks = 30)



### PLOTTING ----


### PLOT: No. left and right pokes ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 1),
  mapping = aes(
    x = animal_id,
    y = No_pokes
  )
) +
  geom_boxplot(aes(fill = choice_direction)) +
  geom_point(aes(group = choice_direction),
    position = position_dodge(width = 0.75)
  )


### PLOT: done trials/animals ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 1),

  mapping = aes(
    x = animal_id,
    y = done_trials
  )
) +
  geom_boxplot()



### PLOT: CP duration ----
ggplot(
  data = TRAINING %>%
    dplyr::filter(stage == 1),

  mapping = aes(
    x = date,
    y = total_CP, col = animal_id
  )
) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))




### PLOT: No. done trials vs date ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 0),
  mapping = aes(x = date, y = done_trials)
) +
  geom_line(mapping = aes(col = animal_id)) +
  geom_point(mapping = aes(col = animal_id)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


annotate("label_repel",
  x = TRAINING %>%
    dplyr::filter(
      stage == 0,
      date == "2019-05-28",
      done_trials > 0
    ) %>%
    select(date) %>%
    pull() %>% `[`(1:(length(.) / 2)),
  y = TRAINING %>%
    dplyr::filter(
      stage == 0,
      date == "2019-05-28",
      done_trials > 0
    ) %>%
    select(done_trials) %>%
    pull() %>% `[`(1:(length(.) / 2)),
  label = TRAINING %>%
    dplyr::filter(
      stage == 0,
      date == "2019-05-28",
      done_trials > 0
    ) %>%
    select(animal_id) %>%
    pull() %>%
    unique()
)



geom_label_repel(
  mapping = aes(label = animal_id, col = animal_id),
  direction = "y",
  hjust = -0.5,
  segment.size = 0.5
)

geom_label(aes(label = animal_id, col = animal_id))


### PLOT: CP duration vs No. done trials ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 1),
  mapping = aes(x = total_CP, y = done_trials)
) +
  geom_point(mapping = aes(col = animal_id))
geom_line(mapping = aes(col = animal_id))





setdiff(
  rds_list %>%
    unlist() %>%
    substr(start = 1, stop = 41),
  paste0(TRAINING$file, ".rds") %>%
    substr(start = 1, stop = 40)
)


setdiff(
  paste0(TRAINING$file, ".rds") %>%
    substr(start = 1, stop = 40),
  rds_list %>%
    unlist() %>%
    substr(start = 1, stop = 41)
)


setdiff(c(1, 2, 3, 4, 5, 6), c(4, 5, 6, 7, 8, 9))
setdiff(c(4, 5, 6, 7, 8, 9), c(1, 2, 3, 4, 5, 6))




intersect(c(1, 2, 3, 4, 5, 6), c(4, 5, 6, 7, 8, 9))

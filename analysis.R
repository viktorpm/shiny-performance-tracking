library(ggplot2)
library(ggrepel)
library(chron)
library(padr)
library(gridExtra)

### Reading csv to tibble ----
TRAINING <- read_csv(file.path("D:", "_R_WD", "git_projects", "rat_wm_training", "output_data", "TRAINING.csv"))
TRAINING <- TRAINING %>%
  mutate(date = date %>% as.Date(format = c("%d-%b-%Y"))) %>%
  mutate(animal_id = animal_id %>% toupper()) %>%
  mutate(session_length = save_time %>% chron(times. = .) - start_time %>% chron(times. = .)) %>%
  mutate(settings_file = settings_file %>% substr(start = nchar(.) - 10, stop = nchar(.) - 4)) %>%
  mutate(stage = replace(stage, A2_time > 0 & A2_time < 0.5 ,2)) %>% 
  gather(right_trials, left_trials, key = "choice_direction", value = "No_pokes")


TRAINING$session_length %>% hist(breaks = 50)



### PLOTTING ----


### PLOT: session length over time ----
ggplot(
  data = TRAINING,
  mapping = aes(
    col = animal_id,
    x = date,
    y = (session_length * 60 * 24) %>% as.numeric()
  )
) +
  geom_line(linetype = "dashed", alpha = 0.4) +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))



### PLOT: No. left and right pokes ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 1),
  mapping = aes(
    x = animal_id,
    y = No_pokes
  )
) +
  geom_boxplot(aes(fill = choice_direction), alpha = 0.4) +
  geom_point(aes(group = choice_direction),
    position = position_dodge(width = 0.75)
  )



### PLOT: done trials/animals ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 1),

  mapping = aes(
    x = animal_id,
    y = done_trials # / ((session_length * 60 * 24) %>% as.numeric())
  )
) +
  geom_boxplot()



### PLOT: CP duration ----
ggplot(
  data = TRAINING %>%
    dplyr::filter(stage == 2, date > "2019-06-10"),

  mapping = aes(
    col = animal_id,
    x = date,
    y = total_CP # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
  )
) +

  ### lines and points
  geom_line(linetype = "dashed", alpha = 0.4) +
  geom_point() +

  ### scales, labels, themes
  scale_x_date(
    date_breaks = "1 day",
    date_labels = "%b %d",
    minor_breaks = "1 day",
    limits = c(as.Date("2019-06-18"), as.Date("2019-06-21"))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("CP duration (stage 1) [s]") +
  xlab("Date [day]") +
  geom_label_repel(
    data = TRAINING %>%
      dplyr::filter(stage == 2,
                    date == "2019-06-19"|date == "2019-06-20"|date == "2019-06-21",
                    choice_direction == "right_trials", 
                    total_CP > 6), # %>%
    # arrange(total_CP) %>%
    # slice(1:6),
    mapping = aes(label = animal_id, col = animal_id),
    # nudge_x = 2,
    hjust = -0.9,
    direction = "y"
    # nudge_y = 2.5,
    # check_overlap = F # geom_text parameter
  ) +
  geom_hline(yintercept = 6)



### PLOT: No. done trials vs date ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 2, date > "2019-06-01"),
  mapping = aes(
    x = date,
    y = done_trials / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
  )
) +

  ### lines and points
  geom_line(
    mapping = aes(col = animal_id),
    linetype = "dashed",
    alpha = 0.4
  ) +
  geom_point(mapping = aes(col = animal_id)) +

  ### scales, labels, themes
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("Norm. No. done trials (stage 1)") +
  xlab("Date") +
  geom_label_repel(
    data = TRAINING %>%
      dplyr::filter(stage == 2, 
                    date == "2019-06-19"|date == "2019-06-20"|date == "2019-06-21", 
                    choice_direction == "right_trials"),
    mapping = aes(label = animal_id, col = animal_id),
    # nudge_x = 0.5,
    hjust = -0.5,
    direction = "y"
    # nudge_y = 2.5,
    # check_overlap = F # geom_text parameter
  ) 


### PLOT: CP duration vs No. done trials ----
ggplot(
  data = TRAINING %>% dplyr::filter(stage == 1),
  mapping = aes(x = done_trials, y = total_CP)
) +
  geom_point(mapping = aes(col = animal_id))
geom_line(mapping = aes(col = animal_id))





recording_dates <- TRAINING %>% dplyr::filter(choice_direction == "left_trials") %>% 
  group_by(animal_id) %>% 
  mutate(trained = T) %>%
  pad(start_val = as.Date("2019-04-25"), end_val = as.Date("2019-06-21")) %>% 
  mutate(day_name = weekdays(date)) %>%
  mutate(weekend = is.weekend(date)) %>%
  select(date,day_name,trained) %>% 
  mutate(trained = replace(trained, is.na(trained), F))
  
ggplot(data = recording_dates,
       mapping = aes(x = date, y = animal_id)) + 
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day")+
  theme(axis.text.x = element_text(angle = 90, vjust = -0.001)) + 
  geom_point(aes(col = trained, size = trained))
  

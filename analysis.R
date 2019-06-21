library(ggplot2)
library(ggrepel)
library(chron)
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



library(chron)

date_range <- seq.Date(from = as.Date("2019-04-25"), to = as.Date("2019-06-21"), by = 1)

recording_dates <- TRAINING %>% dplyr::filter(choice_direction == "left_trials") %>% 
  group_by(animal_id) %>%
  mutate(day_diff = c(NA,diff(date))) %>% 
  mutate(day_name = weekdays(date)) %>%
  mutate(trained = T) %>% 
  select(date,day_name, day_diff, trained) %>% 
  group_map(~ print(.$date))
names(recording_dates) <- TRAINING$animal_id %>% unique()



library(padr)



pad(recording_dates) %>% View()




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
  
  
TRAINING %>% dplyr::filter(choice_direction == "left_trials") %>% 
  mutate(trained = T) %>%
  pad(group = TRAINING$animal_id)   




training_days <- tibble(
  animal_id = TRAINING$animal_id %>% unique() %>% rep(each = length(date_range)),
  date = date_range %>% rep(TRAINING$animal_id %>% unique() %>% length()),
  weekend = is.weekend(date),
  trained = NA
)

full_join(recording_dates %>% select(animal_id,date, trained),
           training_days %>% select(animal_id, date, trained)) %>% View()



training_days %>%
  dplyr::filter(animal_id == training_days$animal_id %>%
                  unique() %>%
                  `[`(1)) %>%
  mutate(trained = replace(
    trained,
    date_range %in% recording_dates[[training_days$animal_id %>%
                                       unique() %>%
                                       `[`(1) ]],
    T
  ))




identical(
setdiff(date_range, recording_dates[[2]]) %>% as.Date(origin="1970-01-01"), 

date_range[
  !date_range %in% recording_dates$AA01
] 
)

TRAINING$animal_id %>% unique()





training_days$trained

training_days <- for (i in 1:30){
  training_days %>%
    dplyr::filter(animal_id == training_days$animal_id %>%
      unique() %>%
      `[`(i)) %>%
    mutate(trained = replace(
      trained,
      date_range %in% recording_dates[[training_days$animal_id %>%
        unique() %>%
        `[`(i) ]],
      T
    ))
}




recording_dates$AA01
recording_dates[[training_days$animal_id %>% unique() %>% `[`(1) ]]




ifelse(date_range %in% recording_dates$AA01, yes = trained = T, no = trained = F )




recording_dates$AA01








TRAINING$animal_id %>% unique() %>% rep(each = length(date_range))



TRAINING %>% dplyr::filter(choice_direction == "left_trials") %>% 
  group_by(animal_id) %>%
  mutate(day_diff = c(NA,diff(date))) %>% 
  mutate(day_name = weekdays(date)) %>% 
  select(date,day_name, day_diff)
  
  

right_join(tibble(date = seq.Date(from = as.Date("2019-04-25"), to = as.Date("2019-06-20"), by = 1))) 
  






tibble(date = seq.Date(from = as.Date("2019-04-25"), to = as.Date("2019-06-19"), by = 1))


TRAINING %>% dplyr::filter(choice_direction == "left_trials", animal_id == "AA01") %>% 
  select(date) %>% 
  right_join(tibble(date = seq.Date(from = as.Date("2019-04-25"), to = as.Date("2019-06-19"), by = 1))
)
 


base::intersect(x = seq.Date(from = as.Date("2019-04-25"), to = as.Date("2019-06-19"), by = 1),
        y = seq.Date(from = as.Date("2019-04-28"), to = as.Date("2019-06-25"), by = 1)) %>% as.Date(origin="1970-01-01")





mutate(day_diff = c(NA,diff(date))) %>% select(day_diff)




ggplot(TRAINING %>% dplyr::filter(choice_direction == "left_trials") %>% 
         group_by(animal_id) %>% 
         mutate(day_diff = c(NA,diff(date))),
       mapping = aes(x = day_diff)) + 
  geom_bar(aes(col = animal_id))















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

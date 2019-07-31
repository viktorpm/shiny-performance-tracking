library(ggplot2)
library(ggrepel)
library(chron)
library(padr)
library(gridExtra)
library(forcats)
library(zoo)
library(bdscale)
library(ggalluvial)
library(magrittr)
library(purrr)


##############################
### Reading csv to tibble ----
##############################

TRAINING <- read_csv(file.path("shiny_app", "TRAINING.csv"))



################################
### Creating session matrix ----
################################

rigs_sessions <- matrix(data = c("AA01", "AA02", "DO01", "DO02", "SC01", "SC02","VP01", "VP02", "AA03", "AA04", "DO03", "DO04","SC03", "SC04", "VP03", "VP04", "AA05", "AA06","DO05", "DO06", "SC05", "SC06", "VP05", "VP06","AA07", "AA08", "DO07", "DO08", "VP07", "VP08"),
                        nrow = 6,
                        ncol = 5)
rownames(rigs_sessions) <- c("rig_1","rig_2","rig_3","rig_4","rig_5","rig_6")
colnames(rigs_sessions) <- c("session_1","session_2","session_3","session_4","session_5")




##############################################
### filtering and tidying up original csv ----
##############################################

TRAINING <- TRAINING %>%
  mutate(date = date %>% as.Date(format = c("%d-%b-%Y"))) %>%
  mutate(animal_id = animal_id %>% toupper()) %>%
  mutate(session_length = difftime(save_time, start_time, units = "mins")) %>% 
  mutate(settings_file = ifelse(settings_file == "empty_field_in_mat_file", 
                                yes = "empty_field_in_mat_file",
                                no = settings_file %>% substr(start = nchar(.) - 10, stop = nchar(.) - 4)
                                )
         ) %>% 
  mutate(protocol = file %>% substr(
                           start = file %>% gregexpr(pattern = "@") %>% unlist(),
                           stop =  file %>% 
                             gregexpr(pattern = "_") %>% 
                             map(~ .x[[2]]) %>% 
  #https://community.rstudio.com/t/extract-single-list-element-as-part-of-a-pipeline/1095/5
                             unlist() %>%  `-` (1))
         ) %>% 
  mutate(stage = replace(stage, stage == 0, "0_side_poke_on")) %>%
  mutate(stage = replace(stage, stage == 1, "1_center_poke_on")) %>%
  mutate(stage = replace(stage, 
                         A2_time > 0 & A2_time < 0.5 & reward_type == "Always", 
                         "2_intord_stim")) %>%
  mutate(stage = replace(stage, reward_type == "NoReward", "3_NoReward")) %>% 
  rowwise() %>% 
  mutate(rig = which(rigs_sessions == animal_id, arr.ind = T)[1] ) %>% 
  mutate(session = which(rigs_sessions == animal_id, arr.ind = T)[2]) %>% 
  ungroup() %>%   
  gather(right_trials, left_trials, key = "choice_direction", value = "No_pokes")



TRAINING %>% 
  dplyr::filter(animal_id == "AA03", date == max(date)-1, choice_direction == "right_trials") %>% 
  select(done_trials, violation_trials, hit_trials, timeoout_trials) 


TRAINING %>% names

TRAINING %>% 
  dplyr::filter(stage != "0_side_poke_on") %>% 
  mutate(donetrials2 = right_trials + left_trials) %>% 
  mutate(difftrials = done_trials-donetrials2) %>% 
  dplyr::filter(difftrials != 0) %>% View()




### Session length distribution
ggplot(data = TRAINING %>% 
         dplyr::filter(session_length > 0) %>% 
         select(session_length),
       mapping = aes(x = session_length)) + 
  geom_histogram(bins = 70) + 
  scale_x_continuous(breaks = seq(from = 0, to = 300, by = 25), minor_breaks = F)




### PLOTTING ----

#######################################
### PLOT: session length over time ----
#######################################

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




#######################################
### PLOT: No. left and right pokes ----
#######################################

### "stage" values:
### "0_side_poke_on"
### "1_center_poke_on"
### "2_intord_stim"

ggplot(
  data = TRAINING %>% dplyr::filter(stage == "1_center_poke_on"),
  mapping = aes(
    x = animal_id,
    y = No_pokes
  )
) +
  geom_boxplot(aes(fill = choice_direction), alpha = 0.4) +
  geom_point(aes(group = choice_direction),
    position = position_dodge(width = 0.75)
  )




##################################
### PLOT: done trials/animals ----
##################################

ggplot(
  data = TRAINING %>% dplyr::filter(stage == "1_center_poke_on"),

  mapping = aes(
    x = animal_id,
    y = done_trials # / ((session_length * 60 * 24) %>% as.numeric())
  )
) +
  geom_boxplot()




##########################
### PLOT: CP duration ----
##########################

ggplot(
  data = TRAINING %>%
    dplyr::filter(animal_id == "AA02"),

  mapping = aes(
    #col = animal_id,
    x = date,
    y = total_CP # / ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
  )
) +

  ### lines and points
  geom_line(linetype = "dashed", alpha = 0.4) +
  geom_point(aes(col = stage), size = 3) +

  ### scales, labels, themes
  scale_x_date(
    date_breaks = "1 day",
    date_labels = "%b %d",
    minor_breaks = "1 day"
    # limits = c(as.Date("2019-06-18"), as.Date("2019-06-21"))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("CP duration (stage 1) [s]") +
  xlab("Date [day]") +
  geom_label_repel(
    data = TRAINING %>%
      dplyr::filter(
        animal_id == "AA01",
        date == max(date),
        # max(date),
        # "2019-06-19" | date == "2019-06-20" | ,
        choice_direction == "right_trials"
        # total_CP > 6
      ), # %>%
    # arrange(total_CP) %>%
    # slice(1:6),
    mapping = aes(label = animal_id),
    # nudge_x = 2,
    hjust = -0.9,
    direction = "y"
    # nudge_y = 2.5,
    # check_overlap = F # geom_text parameter
  ) +
  geom_hline(yintercept = 6)




######################################
### PLOT: No. done trials vs date ----
######################################

ggplot(
  data = TRAINING %>% dplyr::filter(stage == "1_center_poke_on"),
  mapping = aes(
    x = date,
    y = done_trials #/ ((session_length * 60 * 24) %>% as.numeric()) # normalized to session length
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
      dplyr::filter(
        stage == "1_center_poke_on",
        date == max(date),
        choice_direction == "right_trials"
      ),
    mapping = aes(label = animal_id, col = animal_id),
    # nudge_x = 0.5,
    hjust = -0.5,
    direction = "y"
    # nudge_y = 2.5,
    # check_overlap = F # geom_text parameter
  )



# RAINING$file[str_detect(TRAINING$file,pattern = regex("", ignore_case = T))]

# TRAINING$file %>% str_detect(pattern = regex("delay", ignore_case = T))

#############################################
### PLOT: CP duration vs No. done trials ----
#############################################

ggplot(
  data = TRAINING %>% dplyr::filter(stage == "1_center_poke_on"),
  mapping = aes(y = done_trials, x = total_CP)
) +
  geom_point(mapping = aes(col = animal_id))





#####################################
### PLOT: stage transition track ----
#####################################

ggplot(
  data = TRAINING %>% dplyr::filter(experimenter == "athena") ,
  mapping = aes(x = date, y = animal_id)
) +
  geom_point(aes(col = as.character(stage)), size = 6) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.001)) +
  geom_label_repel(
    data = TRAINING %>%
      dplyr::filter(date == max(date), choice_direction == "left_trials"),
    mapping = aes(label = animal_id),
    direction = "y",
    hjust = -0.5
  ) +
  geom_label_repel(
    data = TRAINING %>%
      dplyr::filter(date == max(date) - 1, choice_direction == "left_trials"),
    mapping = aes(label = protocol),
    direction = "y",
    hjust = 1.3,
    vjust = 1
  )





########################################
### PLOT: missing data points track ----
########################################

recording_dates <- TRAINING %>%
  dplyr::filter(choice_direction == "left_trials") %>%
  group_by(animal_id) %>%
  mutate(trained = T) %>%
  pad(start_val = TRAINING$date %>% min(), end_val = TRAINING$date %>% max()) %>%
  mutate(day_name = weekdays(date)) %>%
  mutate(weekend = is.weekend(date)) %>%
  select(date, day_name, trained, rig) %>%
  mutate(trained = replace(trained, is.na(trained), F)) %>%
  ungroup() %>%
  rowwise() %>% 
  mutate(rig = which(rigs_sessions == animal_id, arr.ind = T)[1]) %>% 
  mutate(session = which(rigs_sessions == animal_id, arr.ind = T)[2]) %>% 
  ungroup()

ggplot(
  data = recording_dates,
    #mutate(animal_id = fct_reorder(animal_id, rig)),
  mapping = aes(x = date, y = fct_reorder(animal_id, rig))
) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.001)) +
  # geom_raster(aes(fill = trained))
  geom_point(aes(size = trained, col = trained)) +
  geom_label_repel(
    data = recording_dates %>% 
      dplyr::filter(date == max(date)),
    mapping = aes(label = rig, fill = as.character(rig)),
    direction = "y",
    hjust = -1
  ) +
  labs(fill = "Rig")


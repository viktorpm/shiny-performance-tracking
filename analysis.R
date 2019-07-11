library(ggplot2)
library(ggrepel)
library(chron)
library(padr)
library(gridExtra)
library(forcats)
library(zoo)
library(bdscale)
library(ggalluvial)


##############################
### Reading csv to tibble ----
##############################

TRAINING <- read_csv(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "output_data", "TRAINING.csv"))




##############################################
### filtering and tidying up original csv ----
##############################################

TRAINING <- TRAINING %>%
  mutate(date = date %>% as.Date(format = c("%d-%b-%Y"))) %>%
  mutate(animal_id = animal_id %>% toupper()) %>%
  dplyr::filter(animal_id != "RATNAME", animal_id != "SOUNDRAT", animal_id != "TEST01") %>%
  mutate(session_length = save_time %>% chron(times. = .) - start_time %>% chron(times. = .)) %>%
  mutate(settings_file = settings_file %>% substr(start = nchar(.) - 10, stop = nchar(.) - 4)) %>%
  mutate(stage = replace(stage, stage == 0, "0_side_poke_on")) %>%
  mutate(stage = replace(stage, stage == 1, "1_center_poke_on")) %>%
  mutate(stage = replace(stage, A2_time > 0 & A2_time < 0.5, "2_intord_stim")) %>%
  mutate(rig = ifelse(animal_id %in% c("AA01", "VP01", "SC03", "DO05", "AA07"), yes = 1,
    ifelse(animal_id %in% c("AA02", "VP02", "SC04", "DO06", "AA08"), yes = 2,
      ifelse(animal_id %in% c("DO01", "AA03", "VP03", "SC05", "DO07"), yes = 3,
        ifelse(animal_id %in% c("DO02", "AA04", "VP04", "SC06", "DO08"), yes = 4,
          ifelse(animal_id %in% c("SC01", "DO03", "AA05", "VP05", "VP07"), yes = 5,
            ifelse(animal_id %in% c("SC02", "DO04", "AA06", "VP06", "VP08"), yes = 6, no = NA)
          )
        )
      )
    )
  )) %>%
  mutate(session = ifelse(animal_id %in% c("AA01", "AA02", "DO01", "DO02", "SC01", "SC02"), yes = "session_1",
    ifelse(animal_id %in% c("VP01", "VP02", "AA03", "AA04", "DO03", "DO04"), yes = "session_2",
      ifelse(animal_id %in% c("SC03", "SC04", "VP03", "VP04", "AA05", "AA06"), yes = "session_3",
        ifelse(animal_id %in% c("DO05", "DO06", "SC05", "SC06", "VP05", "VP06"), yes = "session_4",
          ifelse(animal_id %in% c("AA07", "AA08", "DO07", "DO08", "VP07", "VP08"), yes = "session_5", no = NA)
        )
      )
    )
  )) %>%
  gather(right_trials, left_trials, key = "choice_direction", value = "No_pokes")




### Session length distribution
TRAINING$session_length %>% hist(breaks = 70)



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
    dplyr::filter(stage == "1_center_poke_on"),

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
    minor_breaks = "1 day"
    # limits = c(as.Date("2019-06-18"), as.Date("2019-06-21"))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ylab("CP duration (stage 1) [s]") +
  xlab("Date [day]") +
  geom_label_repel(
    data = TRAINING %>%
      dplyr::filter(
        stage == "1_center_poke_on",
        date == max(date),
        # max(date),
        # "2019-06-19" | date == "2019-06-20" | ,
        choice_direction == "right_trials"
        # total_CP > 6
      ), # %>%
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




######################################
### PLOT: No. done trials vs date ----
######################################

ggplot(
  data = TRAINING %>% dplyr::filter(stage == "1_center_poke_on"),
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
  data = TRAINING,
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
  )



# pos = position_jitter(width = 0,seed = 1)
# ggplot(data = TRAINING %>% group_by(stage, animal_id),
#        mapping = aes(x = stage, axis1 = animal_id, axis2 = animal_id)) +
#   geom_alluvium(aes(fill = length(animal_id)))+
#   geom_stratum(width = 1/12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", label.strata = TRUE)
#
#
#   geom_point(aes(shape = as.character(stage), col = animal_id),
#              position = pos,
#              alpha = 0.7) +
#   scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day") +
#   theme(axis.text.x = element_text(angle = 90, vjust = -0.001)) +
#   geom_label_repel(data = TRAINING %>%
#                      dplyr::filter(date == max(date), choice_direction == "left_trials"),
#                    mapping = aes(label = animal_id),
#                    direction = "y",
#                    hjust = -0.5,
#                    position = pos)
#
# TRAINING %>%
#   pad(start_val = min(.$date), end_val = max(.$date)) %>%
#   group_by(stage,date) %>%
#   summarise(length(animal_id)) %>%
#   arrange(date) %>%  View()




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
  mutate(rig = ifelse(animal_id %in% c("AA01", "VP01", "SC03", "DO05", "AA07"), yes = 1,
    ifelse(animal_id %in% c("AA02", "VP02", "SC04", "DO06", "AA08"), yes = 2,
      ifelse(animal_id %in% c("DO01", "AA03", "VP03", "SC05", "DO07"), yes = 3,
        ifelse(animal_id %in% c("DO02", "AA04", "VP04", "SC06", "DO08"), yes = 4,
          ifelse(animal_id %in% c("SC01", "DO03", "AA05", "VP05", "VP07"), yes = 5,
            ifelse(animal_id %in% c("SC02", "DO04", "AA06", "VP06", "VP08"), yes = 6, no = NA)
          )
        )
      )
    )
  )) %>%
  mutate(session = ifelse(animal_id %in% c("AA01", "AA02", "DO01", "DO02", "SC01", "SC02"), yes = "session_1",
    ifelse(animal_id %in% c("VP01", "VP02", "AA03", "AA04", "DO03", "DO04"), yes = "session_2",
      ifelse(animal_id %in% c("SC03", "SC04", "VP03", "VP04", "AA05", "AA06"), yes = "session_3",
        ifelse(animal_id %in% c("DO05", "DO06", "SC05", "SC06", "VP05", "VP06"), yes = "session_4",
          ifelse(animal_id %in% c("AA07", "AA08", "DO07", "DO08", "VP07", "VP08"), yes = "session_5", no = NA)
        )
      )
    )
  ))




########################################
### PLOT: missing data points track ----
########################################

ggplot(
  data = recording_dates %>%
    mutate(animal_id = fct_reorder(animal_id, as.numeric(rig))),
  mapping = aes(x = date, y = animal_id)
) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d", minor_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.001)) +
  # geom_raster(aes(fill = trained))
  geom_point(aes(size = trained, col = trained)) +
  geom_label_repel(
    data = recording_dates %>% dplyr::filter(date == max(date)),
    mapping = aes(label = rig, fill = as.character(rig)),
    direction = "y",
    hjust = -1
  ) +
  labs(fill = "Rig")


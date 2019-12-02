TRAINING <- read_csv(file.path("TRAINING.csv"))



################################
### Creating session matrix ----
################################

# rigs_sessions <- matrix(
#   data = c("AA01", "AA02", "DO01", "DO02", "SC01", "SC02", "VP01", "VP02", "AA03", "AA04", "DO03", "DO04", "SC03", "SC04", "VP03", "VP04", "AA05", "AA06", "DO05", "DO06", "SC05", "SC06", "VP05", "VP06", "AA07", "AA08", "DO07", "DO08", "VP07", "VP08"),
#   nrow = 6,
#   ncol = 5
# )
# rownames(rigs_sessions) <- c("rig_1", "rig_2", "rig_3", "rig_4", "rig_5", "rig_6")
# colnames(rigs_sessions) <- c("session_1", "session_2", "session_3", "session_4", "session_5")




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
  )) %>%
  # mutate(protocol = file %>% substr(
  #   start = file %>% gregexpr(pattern = "@") %>% unlist(),
  #   stop = file %>%
  #     gregexpr(pattern = "_") %>%
  #     map(~ .x[[2]]) %>%
  #     # https://community.rstudio.com/t/extract-single-list-element-as-part-of-a-pipeline/1095/5
  #     unlist() %>% `-`(1)
  # )) %>%
  mutate(stage = replace(stage, stage == 0, "0_side_poke_on")) %>%
  mutate(stage = replace(stage, stage == 1, "1_center_poke_on")) %>%
  mutate(stage = replace(
    stage,
    A2_time > 0 & A2_time < 0.5 & reward_type == "Always",
    "2_intord_stim"
  )) %>%
  mutate(stage = replace(stage, reward_type == "DelayedReward", "3_DelayedReward")) %>%
  mutate(stage = replace(stage, reward_type == "NoReward", "3_NoReward")) %>%
  rowwise() %>%
  # mutate(rig = which(rigs_sessions == animal_id, arr.ind = T)[1]) %>%
  # mutate(session = which(rigs_sessions == animal_id, arr.ind = T)[2]) %>%
  ungroup() %>%
  gather(right_trials, left_trials, key = "choice_direction", value = "No_pokes")


### Session length distribution
ggplot(
  data = TRAINING %>%
    dplyr::filter(session_length > 0) %>%
    select(session_length),
  mapping = aes(x = session_length)
) +
  geom_histogram(bins = 70) +
  scale_x_continuous(breaks = seq(from = 0, to = 300, by = 25), minor_breaks = F)

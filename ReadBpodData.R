ReadBpodData <- function(rds_file, data_source, rat_data) {

  ### initializes TRAINING variables

  # file <- NA
  settings_file <- NA
  # protocol <- NA
  # data_source <- data_source
  experimenter <- NA
  animal_id <- NA
  rig_id <- NA
  # date <- NA
  start_time <- NA
  save_time <- NA
  stage <- NA
  right_trials <- NA
  left_trials <- NA
  right_hit_frac <- NA
  left_hit_frac <- NA
  all_trials <- NA
  completed_trials <- NA
  correct_trials <- NA
  error_trials <- NA
  violation_trials <- NA
  timeoout_trials <- NA
  init_CP <- NA
  total_CP <- NA
  A1_time <- NA
  A2_time <- NA
  reward_type <- NA




  TRAINING <- list(
    file = rds_file,
    settings_file,
    protocol = substr(rds_file,
      start = gregexpr(
        pattern = "_",
        rds_file
      ) %>%
        unlist() %>% `[`(1) + 1,
      stop = gregexpr(
        pattern = "_",
        rds_file
      ) %>%
        unlist() %>% `[`(3) - 1
    ),
    data_source = data_source,
    experimenter,
    animal_id = substr(rds_file,
      start = 1,
      stop = gregexpr(
        pattern = "_",
        rds_file
      ) %>%
        unlist() %>% `[`(1) - 1
    ),
    rig_id,
    date = rat_data$SessionData[,,]$Info[,,]$SessionDate %>% as.character(),
    start_time = rat_data$SessionData[, , ]$Info[, , ]$SessionStartTime.UTC %>% as.character(),
    save_time,
    stage,
    right_trials,
    left_trials,
    right_hit_frac,
    left_hit_frac,
    all_trials,
    completed_trials,
    correct_trials,
    error_trials,
    violation_trials,
    timeoout_trials,
    init_CP,
    total_CP,
    A1_time,
    A2_time,
    reward_type
  )
  return(TRAINING)
}

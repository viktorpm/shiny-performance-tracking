# Function to read and process Bpod data from an RDS file
ReadBpodData <- function(rds_file, data_source, rat_data) {
  # Initialize TRAINING variables with NA
  settings_file <- NA
  experimenter <- NA
  animal_id <- NA
  rig_id <- NA
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
  
  # Extract protocol from rds_file name
  protocol <- substr(rds_file,
                     start = gregexpr(pattern = "_", rds_file) %>% unlist() %>% `[`(1) + 1,
                     stop = gregexpr(pattern = "_", rds_file) %>% unlist() %>% `[`(3) - 1
  )
  
  # Extract animal_id from rds_file name
  animal_id <- substr(rds_file,
                      start = 1,
                      stop = gregexpr(pattern = "_", rds_file) %>% unlist() %>% `[`(1) - 1
  )
  
  # Extract date and start_time from rat_data
  date <- rat_data$SessionData[,,]$Info[,,]$SessionDate %>% as.character()
  start_time <- rat_data$SessionData[, , ]$Info[, , ]$SessionStartTime.UTC %>% as.character()
  
  # Initialize TRAINING list with extracted and initialized variables
  TRAINING <- list(
    file = rds_file,
    settings_file = settings_file,
    protocol = protocol,
    data_source = data_source,
    experimenter = experimenter,
    animal_id = animal_id,
    rig_id = rig_id,
    date = date,
    start_time = start_time,
    save_time = save_time,
    stage = stage,
    right_trials = right_trials,
    left_trials = left_trials,
    right_hit_frac = right_hit_frac,
    left_hit_frac = left_hit_frac,
    all_trials = all_trials,
    completed_trials = completed_trials,
    correct_trials = correct_trials,
    error_trials = error_trials,
    violation_trials = violation_trials,
    timeoout_trials = timeoout_trials,
    init_CP = init_CP,
    total_CP = total_CP,
    A1_time = A1_time,
    A2_time = A2_time,
    reward_type = reward_type
  )
  
  return(TRAINING)
}

# Documentation:
# Variables:
# - rds_file: The name of the RDS file to process.
# - data_source: The source of the data (e.g., "experiment", "simulation").
# - rat_data: The data structure containing the saved data from the RDS file.
# - settings_file: The settings file associated with the experiment.
# - experimenter: The name of the experimenter.
# - animal_id: The ID of the animal.
# - rig_id: The ID of the rig.
# - start_time: The start time of the experiment.
# - save_time: The save time of the experiment.
# - stage: The training stage.
# - right_trials: The number of right trials.
# - left_trials: The number of left trials.
# - right_hit_frac: The fraction of right hits.
# - left_hit_frac: The fraction of left hits.
# - all_trials: The total number of trials.
# - completed_trials: The number of completed trials.
# - correct_trials: The number of correct trials.
# - error_trials: The number of error trials.
# - violation_trials: The number of violation trials.
# - timeoout_trials: The number of timeout trials.
# - init_CP: The initial CP duration.
# - total_CP: The total CP duration.
# - A1_time: The A1 time.
# - A2_time: The A2 time.
# - reward_type: The type of reward.
# - TRAINING: A list containing processed data from the RDS file.

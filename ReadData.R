ReadData <- function(rds_file) {
  library(R.matlab)
  library(tidyverse)
  library(purrr)

  # TRAINING <- tibble(
  #   date = character(),
  #   time = character(),
  #   animal_id = character(),
  #   experimenter = character(),
  #   right_trials = numeric(),
  #   left_trials = numeric()
  # )

  # browser()


  # filename_pos <- gregexpr(file,
  #                          pattern = "/"
  # ) %>%
  #   unlist() %>%
  #   `[[`(length(.)) + 1
  #
  # filename <- substr(
  #   file,
  #   start = filename_pos,
  #   stop = nchar(file)
  # )
  #
  #
  # file_path <- substr(
  #   file,
  #   start = 1,
  #   stop = filename_pos - 1
  # )

  rat_data <- readRDS(paste0(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files"), "/", rds_file))

  filename_pos <- gregexpr(rat_data$saved[, , ]$SavingSection.data.file %>% as.character(),
    pattern = "\\\\"
  ) %>%
    unlist() %>%
    `[[`(length(.)) + 1
  
  
  settings_filename_pos <- gregexpr(rat_data$saved[, , ]$SavingSection.settings.file %>% as.character(),
                           pattern = "\\\\"
  ) %>%
    unlist() %>%
    `[[`(length(.)) + 1



  TRAINING <- list(
    # file = rat_data$saved[, , ]$SavingSection.data.file %>% as.character() %>%
    #   substr(start = filename_pos, stop = nchar(.)),
    
    file = rds_file,
    
    settings_file = rat_data$saved[, , ]$SavingSection.settings.file %>% as.character() %>%
      substr(start = settings_filename_pos, stop = nchar(.)), 
    
    experimenter = rat_data$saved[, , ]$SavingSection.experimenter %>%
      as.character(),
    
    animal_id = rat_data$saved[, , ]$SavingSection.ratname %>%
      as.character(),
    
    date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
      as.character() %>%
      substr(1, 11), # %>%
    # strptime(format = "%d-%b-%Y") %>%
    # as.POSIXct(),
    
    # MATLAB TIME FORMAT TO R: https://stackoverflow.com/questions/30072063/how-to-extract-the-time-using-r-from-a-matlab-serial-date-number
    start_time = rat_data$saved[, , ]$SavingSection.settings.file.load.time %>% 
      as.numeric() %>% `-` (719529) %>% `*`(86400) %>% 
      as.POSIXct( origin = "1970-01-01", tz = "UTC") %>% 
      as.character() %>%
      substr(12, 20),
    
    
    save_time = rat_data$saved[, , ]$SavingSection.SaveTime %>%
      as.character() %>%
      substr(13, 20),
    
    right_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass1 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass2 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass3 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass4 %>% as.double(),
    
    left_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass5 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass6 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass7 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass8 %>% as.double(),
    
    stage = rat_data$saved[, , ]$SideSection.training.stage %>% as.numeric(),
    
    inti_CP = rat_data$saved[, , ]$SideSection.init.CP.duration %>% as.numeric(),
    
    total_CP = rat_data$saved[, , ]$SideSection.Total.CP.duration %>% as.numeric(),
    
    done_trials = rat_data$saved[, , ]$ProtocolsSection.n.done.trials %>% as.numeric(),
    
    A2_time = rat_data$saved[, , ]$SideSection.A2.time %>% as.numeric(),
    
    reward_type = rat_data$saved[, , ]$SideSection.reward.type %>% as.character() 
  )
  
  # browser()
  
  
  ### converts empty elements (character(0), num(0)) to text otherwise in TRAININGtoCSV as_tibble function won't save those rows  
  
  TRAINING <- lapply(TRAINING, function(x) ifelse(is_empty(x), yes = "empty_field_in_mat_file", no = x)) 

  
  # if (identical(TRAINING$settings_file, character(0))){
  #   TRAINING$settings_file <- "empty_field_in_mat_file"
  # }
  # 
  # if (identical(TRAINING$file, character(0))){
  #    TRAINING$file <- "empty_field_in_mat_file"
  # }
   
  
  
  #browser()




  # rat_data$saved[, , ]$SideSection TrainingStage, init CP duration, total CP duration
  # rat_data$saved[, , ]$ProtocolSection n done trials



  # # adding rows of data to tibble
  # TRAINING <- add_row(TRAINING,
  #   # data id
  #   experimenter = rat_data$saved[, , ]$SavingSection.experimenter,
  #   animal_id = rat_data$saved[, , ]$SavingSection.ratname,
  #   date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
  #     as.character() %>%
  #     substr(1, 11), # %>%
  #   # strptime(format = "%d-%b-%Y") %>%
  #   # as.POSIXct(),
  #   time = rat_data$saved[, , ]$SavingSection.SaveTime %>%
  #     as.character() %>%
  #     substr(13, 20),
  #   right_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass1 +
  #     rat_data$saved[, , ]$StimulusSection.nTrialsClass2 +
  #     rat_data$saved[, , ]$StimulusSection.nTrialsClass3 +
  #     rat_data$saved[, , ]$StimulusSection.nTrialsClass4,
  #   left_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass5 +
  #     rat_data$saved[, , ]$StimulusSection.nTrialsClass6 +
  #     rat_data$saved[, , ]$StimulusSection.nTrialsClass7 +
  #     rat_data$saved[, , ]$StimulusSection.nTrialsClass8
  # )
  #
  return(TRAINING)
}

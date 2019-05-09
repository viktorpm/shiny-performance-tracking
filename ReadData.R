ReadData <- function(filename) {
  library(R.matlab)
  library(tidyverse)

  # TRAINING <- tibble(
  #   date = character(),
  #   time = character(),
  #   animal_id = character(),
  #   experimenter = character(),
  #   right_trials = numeric(),
  #   left_trials = numeric()
  # )


  rat_data <- readMat(file.path("data", filename))
  
  
  
  TRAINING <- list(
    file = rat_data$saved[, , ]$SavingSection.data.file %>% as.character() %>%
      substr(start = (nchar(.)-18), stop = nchar(.)),
    experimenter = rat_data$saved[, , ]$SavingSection.experimenter %>% 
      as.character(),
    animal_id = rat_data$saved[, , ]$SavingSection.ratname %>% 
      as.character(),
    date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
      as.character() %>%
      substr(1, 11), # %>%
    # strptime(format = "%d-%b-%Y") %>%
    # as.POSIXct(),
    time = rat_data$saved[, , ]$SavingSection.SaveTime %>%
      as.character() %>%
      substr(13, 20),
    right_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass1 %>% as.double() +
      rat_data$saved[, , ]$StimulusSection.nTrialsClass2 %>% as.double()+
      rat_data$saved[, , ]$StimulusSection.nTrialsClass3 %>% as.double()+
      rat_data$saved[, , ]$StimulusSection.nTrialsClass4%>% as.double(),
    left_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass5 %>% as.double()+
      rat_data$saved[, , ]$StimulusSection.nTrialsClass6 %>% as.double()+
      rat_data$saved[, , ]$StimulusSection.nTrialsClass7 %>% as.double()+
      rat_data$saved[, , ]$StimulusSection.nTrialsClass8 %>% as.double()
  )


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

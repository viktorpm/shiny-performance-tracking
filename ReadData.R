ReadData <- function(rds_file, trialData = F) {
  library(R.matlab)
  library(tidyverse)
  library(purrr)

  if (missing(trialData)){
    trialData = F
  }
  
  # browser()
  path <- file.path(
    "shiny_app"
  )
  data_source <- NA
  
  ### Default mode, no trial data ----
  if (trialData == F) {

    ### cheks if CSV file exists
    csv_exists <- file.exists(file.path(path, "TRAINING.csv"))

    ### cheks if the rds_file has been processed (can be found in the CSV file)
    if (csv_exists == T) {
      read_TRAINING <- suppressMessages(
        suppressWarnings(
          read_csv(file.path(path, "TRAINING.csv"))
        )
      )
      file_processed_test <- str_detect(
        read_TRAINING$file,
        regex(paste0(rds_file))
      )
    } else {
      file_processed_test <- F
    }


    if (all(file_processed_test == F)) {
      rat_data <- readRDS(paste0(file.path("data", "rds_files"), "/", rds_file))

      ### checks the source of the data
      if (rat_data %>% names() %in% "SessionData" %>% any()) {
        data_source <- "bpod"
        source("ReadBpodData.R")
        TRAINING <- ReadBpodData(
          rds_file = rds_file,
          data_source = data_source,
          rat_data = rat_data
        )
        return(TRAINING)
      } else {
        if (rat_data %>% names() %in% "saved" %>% any()) {
          data_source <- "bcontrol"
          source("ReadBcontrolData.R")
          TRAINING <- ReadBcontrolData(
            rds_file = rds_file,
            rat_data = rat_data,
            data_source = data_source
          )
          return(TRAINING)
        }
      }
    } else {
      warning(paste0(
        rds_file,
        # ".mat: ",
        " file has already been processed"
      ))
      return(NULL)
    }
  } 
  
  ### With trial data ----
  if(trialData == T) {


    ### cheks if CSV file exists
    csv_exists <- file.exists(file.path(path, "TrialByTrial.csv"))

    ### cheks if the rds_file has been processed (can be found in the CSV file)
    if (csv_exists == T) {
      read_TRIAL <- suppressMessages(
        suppressWarnings(
          read_csv(file.path(path, "TrialByTrial.csv"))
        )
      )
      file_processed_test <- str_detect(
        read_TRIAL$file,
        regex(paste0(rds_file))
      )
    } else {
      file_processed_test <- F
    }


    if (all(file_processed_test == F)) {
      rat_data <- readRDS(paste0(file.path("data", "rds_files"), "/", rds_file))

      ### checks the source of the data
      if (rat_data %>% names() %in% "SessionData" %>% any()) {
        # data_source <- "bpod"
        # source("ReadBpodData.R")
        # TRIAL <- ReadBpodData(
        #   rds_file = rds_file,
        #   data_source = data_source,
        #   rat_data = rat_data
        # )
        return(NULL)
      } else {
        if (rat_data %>% names() %in% "saved" %>% any()) {
          data_source <- "bcontrol"
          source("ReadTrialData.R")
          TRIAL <- ReadTrialData(
            rds_file = rds_file,
            rat_data = rat_data,
            data_source = data_source
          )
          return(TRIAL)
        }
      }
    } else {
      warning(paste0(
        rds_file,
        # ".mat: ",
        " file has already been processed"
      ))
      return(NULL)
    }
  }
}

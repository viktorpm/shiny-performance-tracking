ReadData <- function(rds_file) {
  library(R.matlab)
  library(tidyverse)
  library(purrr)

  # browser()

  path <- file.path(
    "output_data"
  )

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
    )} else {
      file_processed_test <- F
    }


    ### pulls data from rds file if it has not been processed before and returns a list with the data
    ### (else i treturns NULL)
    if (all(file_processed_test == F)) {
      rat_data <- readRDS(paste0(file.path("data", "rds_files"), "/", rds_file))

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


      ### pulls data from rds file
      TRAINING <- list(
        file = rds_file,

        settings_file = rat_data$saved[, , ]$SavingSection.settings.file %>% as.character() %>%
          substr(start = settings_filename_pos, stop = nchar(.)),
        
        protocol = substr(rds_file,
                          start = rds_file %>% gregexpr(pattern = "@") %>% unlist(),
                          stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2)-1 
                          ),

        experimenter = rat_data$saved[, , ]$SavingSection.experimenter %>%
          as.character(),

        animal_id = rat_data$saved[, , ]$SavingSection.ratname %>%
          as.character(),

        date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
          as.character() %>%
          substr(1, 11),

        ### MATLAB TIME FORMAT TO R:
        ### https://stackoverflow.com/questions/30072063/how-to-extract-the-time-using-r-from-a-matlab-serial-date-number
        start_time = rat_data$saved[, , ]$SavingSection.settings.file.load.time %>%
          as.numeric() %>% `-`(719529) %>% `*`(86400) %>%
          as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
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

        init_CP = rat_data$saved[, , ]$SideSection.init.CP.duration %>% as.numeric(),

        total_CP = rat_data$saved[, , ]$SideSection.Total.CP.duration %>% as.numeric(),

        done_trials = rat_data$saved[, , ]$ProtocolsSection.n.done.trials %>% as.numeric(),
        
        A1_time = rat_data$saved[, , ]$SideSection.A1.time %>% as.numeric(),
        
        A2_time = rat_data$saved[, , ]$SideSection.A2.time %>% as.numeric(),

        reward_type = rat_data$saved[, , ]$SideSection.reward.type %>% as.character()
      )


      ### converts empty elements (character(0), num(0)) to text
      ### otherwise in TRAININGtoCSV as_tibble function won't save those rows
      TRAINING <- lapply(TRAINING, function(x) ifelse(is_empty(x), yes = "empty_field_in_mat_file", no = x))
      return(TRAINING)
    } else {
      warning(paste0(
        rds_file,
        # ".mat: ",
        " file has already been processed"
      ))
      return(NULL)
    }
  
}

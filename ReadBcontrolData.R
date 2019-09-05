ReadBcontrolData <- function(rds_file, rat_data, data_source) {

  # browser()
  section_name <- rds_file %>% substr(
    start = rds_file %>% gregexpr(pattern = "@") %>% unlist() %>% `+`(1),
    stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
  )




  ### pulls data from rds file if it has not been processed before and returns a list with the data
  ### (else it treturns NULL)

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

    #####################################
    ### files, animals, experimenters ---
    #####################################
    file = rds_file,

    settings_file = rat_data$saved[, , ]$SavingSection.settings.file %>% as.character() %>%
      substr(start = settings_filename_pos, stop = nchar(.)),

    protocol = substr(rds_file,
      start = rds_file %>% gregexpr(pattern = "@") %>% unlist(),
      stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
    ),

    data_source = data_source,

    experimenter = rat_data$saved[, , ]$SavingSection.experimenter %>%
      as.character(),

    animal_id = rat_data$saved[, , ]$SavingSection.ratname %>%
      as.character(),

    rig_id = rat_data$saved[, , ]$WaterValvesSection.RigID %>%
      as.character(),



    #########################
    ### date, time, stage ---
    #########################

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

    stage = rat_data$saved[, , ]$SideSection.training.stage %>% as.numeric(),




    ##############
    ### Trials ---
    ##############

    right_trials = rat_data$saved[, , ]$SideSection.previous.sides %>%
      intToUtf8(multiple = T) %>%
      `[`(. == "r") %>%
      length(),

    left_trials = rat_data$saved[, , ]$SideSection.previous.sides %>%
      intToUtf8(multiple = T) %>%
      `[`(. == "l") %>%
      length(),

    right_hit_frac = rat_data$saved[, , ]$OverallPerformanceSection.Right.hit.frac,

    left_hit_frac = rat_data$saved[, , ]$OverallPerformanceSection.Left.hit.frac,


    # right_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass1 %>% as.double() +
    #   rat_data$saved[, , ]$StimulusSection.nTrialsClass2 %>% as.double() +
    #   rat_data$saved[, , ]$StimulusSection.nTrialsClass3 %>% as.double() +
    #   rat_data$saved[, , ]$StimulusSection.nTrialsClass4 %>% as.double(),
    #
    # left_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass5 %>% as.double() +
    #   rat_data$saved[, , ]$StimulusSection.nTrialsClass6 %>% as.double() +
    #   rat_data$saved[, , ]$StimulusSection.nTrialsClass7 %>% as.double() +
    #   rat_data$saved[, , ]$StimulusSection.nTrialsClass8 %>% as.double(),
    #

    ### all the initiated trials, complete (error, correct) and incomplete (timeout, violation)
    ### all_trials = correct + error + timeout + violation
    all_trials = rat_data$saved[, , ]$ProtocolsSection.n.done.trials %>% as.numeric(),

    ### hit history: not the correct trials but all the initiated trials
    ### (error: 0, correct: 1, violation + timeout: NaN)
    ### complete_trials = correct + error
    completed_trials = get(
      paste(section_name, ".hit.history", sep = ""),
      rat_data$saved[, , ]
    ) %>%
      `[`(!is.na(.)) %>% length(),

    correct_trials = get(
      paste(section_name, ".hit.history", sep = ""),
      rat_data$saved[, , ]
    ) %>%
      `[`(get(
        paste(section_name, ".hit.history", sep = ""),
        rat_data$saved[, , ]
      ) == 1) %>% na.omit() %>% sum(),

    error_trials = get(
      paste(section_name, ".hit.history", sep = ""),
      rat_data$saved[, , ]
    ) %>%
      `[`(get(
        paste(section_name, ".hit.history", sep = ""),
        rat_data$saved[, , ]
      ) == 0) %>% na.omit() %>% length(),


    violation_trials = get(
      paste(section_name, ".violation.history", sep = ""),
      rat_data$saved[, , ]
    ) %>%
      as.numeric() %>%
      sum(na.rm = T),

    timeoout_trials = get(
      paste(section_name, ".timeout.history", sep = ""),
      rat_data$saved[, , ]
    ) %>%
      as.numeric() %>%
      sum(na.rm = T),




    init_CP = rat_data$saved[, , ]$SideSection.init.CP.duration %>% as.numeric(),

    total_CP = rat_data$saved[, , ]$SideSection.Total.CP.duration %>% as.numeric(),

    A1_time = rat_data$saved[, , ]$SideSection.A1.time %>% as.numeric(),

    A2_time = rat_data$saved[, , ]$SideSection.A2.time %>% as.numeric(),

    reward_type = rat_data$saved[, , ]$SideSection.reward.type %>% as.character()
  )


  ### converts empty elements (character(0), num(0)) to text
  ### otherwise in TRAININGtoCSV as_tibble function won't save those rows
  TRAINING <- lapply(TRAINING, function(x) ifelse(is_empty(x), yes = "empty_field_in_mat_file", no = x))
  return(TRAINING)
}

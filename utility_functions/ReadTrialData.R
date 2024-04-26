# Function to read and process trial data from an RDS file
#' @param rds_file The name of the RDS file to process.
#' @param rat_data The data structure with the saved data from the RDS file.
#' @param data_source The source of the data (e.g., "bpod", "bcontrol").
#' @return A list containing processed trial data from the RDS file.

ReadTrialData <- function(rds_file, rat_data, data_source) {
  # Extract section name from rds_file name
  section_name <- rds_file %>% substr(
    start = rds_file %>% gregexpr(pattern = "@") %>% unlist() %>% `+`(1),
    stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
  )

  # Determine pair number based on section_name
  pair_number <- ifelse(
    get(
      paste(section_name, ".pair.history", sep = ""),
      rat_data$saved[, , ]
    ) %>% is_empty(),
    yes = "empty_field_in_mat_file",
    no = get(
      paste(section_name, ".pair.history", sep = ""),
      rat_data$saved[, , ]
    ) %>% as.vector()
  )

  # Extract pair values based on pair_number
  if (pair_number == "empty_field_in_mat_file") {
    pair_value_s1 <- NA
    pair_value_s2 <- NA
  } else {
    pair_value_s1 <- rbind(
      rat_data$saved[, , ]$StimulusSection.pairs.d,
      rat_data$saved[, , ]$StimulusSection.pairs.u
    )[pair_number, 1]
    pair_value_s2 <- rbind(
      rat_data$saved[, , ]$StimulusSection.pairs.d,
      rat_data$saved[, , ]$StimulusSection.pairs.u
    )[pair_number, 2]
  }

  # Prepare trial_by_trial data list
  trial_by_trial <- list(
    animal_id = rat_data$saved[, , ]$SavingSection.ratname %>% as.character(),
    file = rds_file,
    protocol = substr(rds_file,
      start = rds_file %>% gregexpr(pattern = "@") %>% unlist(),
      stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
    ),
    date = rat_data$saved[, , ]$SavingSection.SaveTime %>% as.character() %>%
      substr(1, 11),
    data_source = data_source,
    stage = rat_data$saved[, , ]$SideSection.training.stage %>% as.numeric(),
    A2_time = ifelse(rat_data$saved[, , ]$SideSection.A2.time %>% is_empty(),
      yes = "empty_field_in_mat_file",
      no = rat_data$saved[, , ]$SideSection.A2.time %>% as.numeric()
    ),
    reward_type = rat_data$saved[, , ]$SideSection.reward.type %>%
      as.character(),
    pair_number = pair_number,
    pair_value_s1 = pair_value_s1,
    pair_value_s2 = pair_value_s2,
    hit = ifelse(
      get(
        paste(
          section_name, ".hit.history",
          sep = ""
        ),
        rat_data$saved[, , ]
      ) %>% is_empty(),
      yes = "empty_field_in_mat_file",
      no = get(
        paste(
          section_name, ".hit.history",
          sep = ""
        ),
        rat_data$saved[, , ]
      ) %>% as.vector()
    ),
    choice = ifelse(
      rat_data$saved[, , ]$SideSection.previous.sides %>%
        intToUtf8(multiple = T) %>% is_empty(),
      yes = "empty_field_in_mat_file",
      no = rat_data$saved[, , ]$SideSection.previous.sides %>%
        intToUtf8(multiple = T)
    ),
    trial_per_session = seq(
      from = 1,
      to = get(
        paste(section_name, ".hit.history", sep = ""),
        rat_data$saved[, , ]
      ) %>% length()
    )
  )

  return(trial_by_trial)
}

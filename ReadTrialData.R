ReadTrialData = function(rds_file, rat_data, data_source){
  
  section_name <- rds_file %>% substr(
    start = rds_file %>% gregexpr(pattern = "@") %>% unlist() %>% `+`(1),
    stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
  )
  
  
  
  trial_by_trial = list(
    animal_id = rat_data$saved[, , ]$SavingSection.ratname %>%
      as.character(),
    
    file = rds_file,
    
    protocol = substr(rds_file,
                      start = rds_file %>% gregexpr(pattern = "@") %>% unlist(),
                      stop = rds_file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
    ),
    
    date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
      as.character() %>%
      substr(1, 11),
    
    data_source = data_source,
    
    stage = rat_data$saved[, , ]$SideSection.training.stage %>% as.numeric(),
    
    A2_time = ifelse(rat_data$saved[, , ]$SideSection.A2.time %>% is_empty(),
                     yes = "empty_field_in_mat_file",
                     no = rat_data$saved[, , ]$SideSection.A2.time %>% as.numeric()),
    
    reward_type = rat_data$saved[, , ]$SideSection.reward.type %>% as.character(),
    
    hit = ifelse(
      get(paste(section_name,  ".hit.history", sep = ""), rat_data$saved[, , ]) %>% is_empty(),
      yes =  "empty_field_in_mat_file",
      no = get(paste(section_name,  ".hit.history", sep = ""), rat_data$saved[, , ]) %>%
        as.vector()),
      
      #rat_data$saved[, , ]$AthenaDelayComp.hit.history %>% as.vector(),
    
    choice = ifelse(
      rat_data$saved[, , ]$SideSection.previous.sides %>%
        intToUtf8(multiple = T) %>% is_empty(),
      yes = "empty_field_in_mat_file",
      no = rat_data$saved[, , ]$SideSection.previous.sides %>%
        intToUtf8(multiple = T)
      ),
    
    trial_per_session = seq(from = 1, 
                            to = get(
                              paste(section_name,  ".hit.history", sep = ""), 
                              rat_data$saved[, , ]) %>% 
                              length()
    )
  )
  

  
  
}
library(ggplot2)

TRAINING <- read_csv(file.path("D:", "_R_WD", "git_projects", "rat_wm_training", "output_data", "TRAINING.csv"))
TRAINING <- TRAINING %>%
  mutate(date = date %>% as.Date(format = c("%d-%b-%Y"))) %>% 
  gather(right_trials, left_trials, key = "choice_direction", value = "No_pokes" )



ggplot(data = TRAINING %>% dplyr::filter(stage == 0),
       mapping = aes(x = animal_id,
                     y = No_pokes)) +
  geom_boxplot(aes(fill = choice_direction)) +
  geom_point(aes(group = choice_direction), 
             position = position_dodge(width = 0.75))



ggplot(data = TRAINING %>% dplyr::filter(stage == 1),
       
       mapping = aes(x = animal_id,
                     y = done_trials)) +
  geom_boxplot() 


ggplot(data = TRAINING %>% dplyr::filter(stage == 1,animal_id != "ratname"),
       
       mapping = aes(x = date,
                     y = total_CP,col = animal_id)) +
  geom_line() 

  

ggplot(data = TRAINING %>% dplyr::filter(stage == 0, animal_id != "ratname"),
       mapping = aes(x = date,
                     y = done_trials,
                     col = animal_id)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) 
  

  





setdiff(
  rds_list %>% 
    unlist() %>% 
    substr(start = 1, stop = 41),
  paste0(TRAINING$file, ".rds") %>% 
    substr(start = 1, stop = 40)
)


setdiff(
  paste0(TRAINING$file, ".rds") %>% 
    substr(start = 1, stop = 40),
  rds_list %>% 
    unlist() %>% 
    substr(start = 1, stop = 41)
)


setdiff(c(1,2,3,4,5,6), c(4,5,6,7,8,9))
setdiff(c(4,5,6,7,8,9), c(1,2,3,4,5,6))




intersect(c(1,2,3,4,5,6), c(4,5,6,7,8,9))



library(R.matlab)
library(rmatio)

library(rstudioapi)
library(tidyverse)
library(data.tree)
library(listviewer)
library(stringr)
library(purrr)

### exploring data -------------
rat_data <- readMat(file.path("data","v1.mat"))



names <- rat_data$saved %>%
  dimnames() %>%
  `[[`(1) %>%
  as_tibble()

# finding first "." (full stop)
names %>%
  select(value) %>%
  slice(1) %>%
  str_locate(pattern = "\\.")
# names %>% select(value) %>% lapply(str_locate, pattern = "\\.") -> tmp

to_first_stop <- names %>%
  select(value) %>%
  pull() %>%
  str_locate(pattern = "\\.") %>%
  as.tibble() %>%
  select(start) %>%
  pull() %>%
  `-`(1) # finding the first full stop (start, end)

# unique entry (section) names of the rat_data$saved list
section_names <- names %>%
  select(value) %>%
  pull() %>%
  strtrim(to_first_stop) %>%
  unique()

#creating empty tibble
TRAINING <- tibble(
  date = character(),
  time = character(),
  animal_id = character(),
  experimenter = character(),
  right_trials = numeric(),
  left_trials = numeric()
)

#adding rows of data to tibble
TRAINING <- add_row(TRAINING,
  # data id
  experimenter = rat_data$saved[, , ]$SavingSection.experimenter,
  animal_id = rat_data$saved[, , ]$SavingSection.ratname,
  date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
    as.character() %>%
    substr(1, 11), #%>%
    #strptime(format = "%d-%b-%Y") %>%
    #as.POSIXct(),
  time = rat_data$saved[, , ]$SavingSection.SaveTime %>%
    as.character() %>%
    substr(13, 20),
  right_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass1 +
    rat_data$saved[, , ]$StimulusSection.nTrialsClass2 +
    rat_data$saved[, , ]$StimulusSection.nTrialsClass3 +
    rat_data$saved[, , ]$StimulusSection.nTrialsClass4,
  left_trials = rat_data$saved[, , ]$StimulusSection.nTrialsClass5 +
    rat_data$saved[, , ]$StimulusSection.nTrialsClass6 +
    rat_data$saved[, , ]$StimulusSection.nTrialsClass7 +
    rat_data$saved[, , ]$StimulusSection.nTrialsClass8
)




### read and extract data automatically
# CHECK ALL SUBDIRECTORIES
file_list <- list.files(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data"),recursive = T) %>% as.list()
walk(file_list, ~ReadData(file = .x) %>% TRAININGtoCSV())

list.dirs(file.path("D:","_Rig_data","SoloData","Data"), full.names = TRUE, recursive = TRUE)





data_path <- file.path("D:", "_Rig_data","SoloData","Data")
file_list <- list.files(data_path,recursive = T) %>% as.list()
walk(file_list, ~ReadData(file = .x) %>% TRAININGtoCSV())




rat_data = readRDS(file.path("D:", "_R_WD", "git_projects", 
                             "r_codes_rat_wm", "data",
                             "rds_files",
                             "data_@AthenaDelayComp_viktor_VP08_190710a.mat.rds")) 

names <- rat_data$saved[,,] %>% names()
names %>% str_detect(pattern = "rew")
names[str_detect(names,pattern = regex("box", ignore_case = T))]



file <- "data_@AthenaDelayComp_viktor_VP08_190710a.mat.rds"


file %>% substr(start = file %>% gregexpr(pattern = "@") %>% unlist(),
                stop = file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2)-1 )





rat_data$saved[, , ]$SavingSection.data.file

rat_data$saved[, , ]$SavingSection.hostname
rat_data$saved[, , ]$SideSection.reward.type

rat_data$saved[, , ]$SideSection.A1.time
rat_data$saved[, , ]$SideSection.Total.CP.duration


rat_data$saved[, , ]$ProtocolsSection.n.done.trials
rat_data$saved[, , ]$WaterValvesSection.RigID



rat_data <- readRDS(paste0(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files"), "/", "data_@AthenaDelayComp_dammy_DO07_190522a.mat.rds"))


ReadData("data_@AthenaDelayComp_dammy_DO07_190522a.mat.rds")

rat_data$saved[, , ]$SavingSection.settings.file


# termId <- rstudioapi::terminalExecute("ubuntu")
# rstudioapi::terminalExecute("ssh vplattner@172.24.155.100")


# TRAINING <- ReadData(filename = "data_@AthenaDelayComp_viktor_VP01_190425a.mat") %>% as_tibble()
# TRAINING
# 
# 
# ReadData(filename = "v2.mat") %>% TRAININGtoCSV()
# 
# 
# 
# tmp <- ReadData(filename = "v1.mat") %>% as_tibble()
# 
# bind_rows(tmp, ReadData(filename = "v2.mat") %>% as_tibble())
# 
# 
# 
# 
# 
# write_TRAINING <- as.tibble(ReadMatFile_output$file_info)
# 
# 
# 
# # rat_data$saved[,,]$SavingSection.title
# 
# 
# # right trials
# rat_data$saved[, , ]$StimulusSection.nTrialsClass1 +
#   rat_data$saved[, , ]$StimulusSection.nTrialsClass2 +
#   rat_data$saved[, , ]$StimulusSection.nTrialsClass3 +
#   rat_data$saved[, , ]$StimulusSection.nTrialsClass4
# 
# 
# 
# # left trials
# rat_data$saved[, , ]$StimulusSection.nTrialsClass5 +
#   rat_data$saved[, , ]$StimulusSection.nTrialsClass6 +
#   rat_data$saved[, , ]$StimulusSection.nTrialsClass7 +
#   rat_data$saved[, , ]$StimulusSection.nTrialsClass8
# 
# 
# rat_data$saved %>% length()
# 
# rat_data %>% length()
# rat_data$saved[[4]][, , ]
# 
# 
# 
# 
# rat_data$saved[, , ]$SavingSection.data.file %>% as.character() %>%
#   substr(start = (nchar(.)-18), stop = nchar(.))
# 
# rat_data$saved[, , ]$ProtocolsSection.latest.parsed.events[, , ]$pokes[, , ]$ending.state
# 
# 
# rat_data$saved[, , ]$
# 
#   # ProtocolsSection
#   # AthenaDelayComp
#   # SavinSection
#   # AthenaDelayComp
# 
#   # SavinSection
#   rat_data$saved[, , ]$SavingSection.data.file
# 
# 
# 
# 
# rowwise()
# 
# 
# 
# 
# 
# 
# 
# 
# #---------------------
# 
# dtree <- FromListSimple(rat_data$saved)
# 
# 
# install.packages("devtools")
# devtools::install_github("ropenscilabs/roomba")
# library(roomba)
# 
# rat_data %>%
#   roomba(cols = c("saved", "saved.history", "saved.autoset", "fig.position"), keep = any)
# 
# devtools::install_github("timelyportfolio/reactR")
# listviewer::reactjson(rat_data)

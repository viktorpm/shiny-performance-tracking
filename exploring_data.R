library(R.matlab)
library(rmatio)

library(rstudioapi)
library(tidyverse)
library(data.tree)
library(listviewer)
library(stringr)
library(purrr)

### exploring data -------------
rat_data <- readMat(file.path("data", "v1.mat"))



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

# creating empty tibble
TRAINING <- tibble(
  date = character(),
  time = character(),
  animal_id = character(),
  experimenter = character(),
  right_trials = numeric(),
  left_trials = numeric()
)

# adding rows of data to tibble
TRAINING <- add_row(TRAINING,
  # data id
  experimenter = rat_data$saved[, , ]$SavingSection.experimenter,
  animal_id = rat_data$saved[, , ]$SavingSection.ratname,
  date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
    as.character() %>%
    substr(1, 11), # %>%
  # strptime(format = "%d-%b-%Y") %>%
  # as.POSIXct(),
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
file_list <- list.files(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data"), recursive = T) %>% as.list()
walk(file_list, ~ ReadData(file = .x) %>% TRAININGtoCSV())

list.dirs(file.path("D:", "_Rig_data", "SoloData", "Data"), full.names = TRUE, recursive = TRUE)





data_path <- file.path("D:", "_Rig_data", "SoloData", "Data")
file_list <-l ist.files(data_path, recursive = T) %>% as.list()
walk(file_list, ~ ReadData(file = .x) %>% TRAININGtoCSV())




rat_data <- readRDS(file.path(
  "D:", "_R_WD", "git_projects",
  "r_codes_rat_wm", "data",
  "rds_files",
  "data_@AthenaDelayComp_sharbat_SC03_200116a.mat.rds"
))

rat_data %>% names()

rat_data$saved[, , ]$SavingSection.settings.file.load.time %>%
  as.numeric() %>% `-`(719529) %>% `*`(86400) %>%
  as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
  as.character() %>%
  substr(12, 20)


rat_data$saved[, , ] %>% 
  names() %>% `[`(1) %>% 
  as.character() %>% 
  substr(start = 1, stop = gregexpr(., pattern = "\\."))
  

mat_data <- tibble(names = rat_data$saved[, , ] %>% names(),
                   value = rat_data$saved[, , ],
                   class = lapply(rat_data$saved[, , ], class),
                   dimension = lapply(rat_data$saved[, , ], dim),
                   type = lapply(rat_data$saved[, , ], typeof)
                   
)


names = rat_data$saved[, , ] %>% names()
names %>% str_detect(pattern = "rew")
names[str_detect(names, pattern = regex("history", ignore_case = T))]

rat_data$saved[, , ]$AthenaDelayComp.timeout.history %>% sum()
rat_data$saved[, , ]$AthenaDelayComp.violation.history %>% sum()
rat_data$saved[, , ]$AthenaDelayComp.hit.history %>%
  is.na() %>%
  sum()


### trial by trial choice 

tibble(
  animal_id = rat_data$saved[, , ]$SavingSection.ratname %>%
    as.character(),
  date = rat_data$saved[, , ]$SavingSection.SaveTime %>%
    as.character() %>%
    substr(1, 11),
  stage = rat_data$saved[, , ]$SideSection.training.stage %>% as.numeric(),
  A2_time = rat_data$saved[, , ]$SideSection.A2.time %>% as.numeric(),
  
  reward_type = rat_data$saved[, , ]$SideSection.reward.type %>% as.character(),
  hit = rat_data$saved[, , ]$AthenaDelayComp.hit.history %>% as.vector(),
  choice = rat_data$saved[, , ]$SideSection.previous.sides %>%
    intToUtf8(multiple = T),
  trial_per_session = seq(from = 1, 
                          to = rat_data$saved[, , ]$AthenaDelayComp.hit.history %>% 
                            length()
                          )
 )









rat_data$saved[, , ]$OverallPerformanceSection.Right.hit.frac
rat_data$saved[, , ]$OverallPerformanceSection.Left.hit.frac

rat_data$saved[, , ]$AthenaDelayComp.violation.history

rat_data$saved[, , ]$SideSection.previous.sides %>% 
  intToUtf8(multiple = T) %>% 
  `[` (.=="r") %>% 
  length()

rat_data$saved[, , ]$SideSection.previous.sides %>% 
  intToUtf8(multiple = T) %>% 
  `[` (.=="l") %>% 
  length()


rat_data$saved[, , ]$SideSection.RewardCollection.duration


rat_data$saved[, , ]$SideSection.previous.parameters


rat_data$saved[, , ]$StimulusSection.nTrialsClass1 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass2 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass3 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass4 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass5 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass6 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass7 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass8
  



# left trials
rat_data$saved[, , ]$StimulusSection.nTrialsClass5 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass6 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass7 +
  rat_data$saved[, , ]$StimulusSection.nTrialsClass8



SideSection.previous.sides %>% intToUtf8()

### completed trials
rat_data$saved[, , ]$AthenaDelayComp.hit.history %>%
  `[`(!is.na(.)) %>%
  length()

### error trials
rat_data$saved[, , ]$AthenaDelayComp.hit.history %>%
  `[`(rat_data$saved[, , ]$AthenaDelayComp.hit.history == 0) %>%
  na.omit() %>%
  length()

### correct trials
rat_data$saved[, , ]$AthenaDelayComp.hit.history %>%
  `[`(rat_data$saved[, , ]$AthenaDelayComp.hit.history == 1) %>%
  na.omit() %>%
  sum()




rat_data$saved[, , ]$ProtocolsSection.n.done.trials
rat_data$saved[, , ]$ProtocolsSection.n.completed.trials


rat_data$saved[, , ]$OverallPerformanceSection.violation.rate


# file <- "data_@SoundCategorization_sharbat_SC04_190724a.mat.rds"
file <- "data_@SoundCategorization_viktor_VP08_190729a.mat.rds"


section_name <- file %>% substr(
  start = file %>% gregexpr(pattern = "@") %>% unlist() %>% `+`(1),
  stop = file %>% gregexpr(pattern = "_") %>% unlist() %>% `[`(2) - 1
)



get(paste(section_name, ".violation.history", sep = ""), rat_data$saved[, , ]) %>% sum()




gregexpr(file, pattern = "_") %>%
  unlist() %>%
  `[`(2) - 1

rat_data$saved[, , ]$SavingSection.SaveTime %>%
  as.character() %>%
  substr(1, 11)

rat_data$saved[, , ]$WaterValvesSection

rat_data$saved[, , ]$SavingSection.hostname
rat_data$saved[, , ]$SideSection.reward.type

rat_data$saved[, , ]$SideSection.A1.time
rat_data$saved[, , ]$SideSection.Total.CP.duration


rat_data$saved[, , ]$ProtocolsSection.n.done.trials
rat_data$saved[, , ]$WaterValvesSection.RigID



rat_data <- readRDS(paste0(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files"), "/", "data_@AthenaDelayComp_dammy_DO07_190522a.mat.rds"))


ReadData("data_@AthenaDelayComp_dammy_DO07_190522a.mat.rds")

rat_data$saved[, , ]$SavingSection.settings.file






#### Bpod data -----------------


rat_data <- readRDS(file.path(
  "D:", "_R_WD", "git_projects",
  "r_codes_rat_wm", "data",
  "rds_files",
  #"DefaultSettings.mat.rds",
  "VP04_Gap_Detection_20190822_095255.mat.rds"
))

# rat_data <- readRDS(file.path(
#   "D:", "_R_WD", "git_projects",
#   "r_codes_rat_wm", "data",
#   "rds_files",
#   "data_@AthenaDelayComp_athena_AA01_190516a.mat.rds"
# ))
# 


rat_data %>% names() %in% "saved" %>% any()


rat_data$SessionData[,,]$RawData[,,]$OriginalStateData
rat_data$SessionData[,,]$OnlinePlotParams[,,]$PlotColour
rat_data$SessionData[,,]$RawEvents[,,]$Trial[[1]]

rat_data$SessionData[,,]$Info[,,]$SessionStartTime.MATLAB%>%
  as.numeric() %>% `-`(719529) %>% `*`(86400) %>% 
  as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
  as.character()  %>%
  substr(12, 21)

rat_data$SessionData[,,]$Info[,,]$SessionStartTime.MATLAB

rat_data$SessionData[,,]$Info[,,]$SessionDate %>% as.character() 



rat_data$SessionData[,,]$RawEvents[,,]$Trial[[1]][[1]][,,]$States[,,] %>% 
  names() %>% paste(collapse = ", ")



rat_data$SessionData[,,]$RawEvents[,,]$Trial[[6]][[1]][,,]$States[,,] %>% data.frame(id = names(.)) %>% View()



rat_data$SessionData %>% length()


lev_counter <- rat_data %>% length()

ListLevels <- function(obj) {
  obj %>% is.array()
  obj %>% length()
  level_0 <- obj %>% as.character()
  
  #lev_counter
  #sublev_counter
  
  for (i in length(obj)){
    assign(paste0("level_", as.character(i)), 
           obj[i] %>% names()
           )
  }
  
  
}




if (lev_counter == 1){
  assign(paste0("level_", as.character(i),".",as.character()), rat_data[i] %>% names())
}

  
for (i in 1:lev_counter) {
  assign(paste0("level_", as.character(i),".",as.character()), rat_data[i] %>% names())
}


for (i in 1:lev_counter) {
  assign(paste0("level_1.", as.character(i)), rat_data$SessionData[,,][i] %>% names())
}


for (i in 1:length(rat_data)) {
  rat_data[i] %>% names() %>% paste(collapse = ", ")
  for (j in 1:length(rat_data[[i]])) {
    rat_data[[i]][[j]] %>% names() %>% paste(collapse = ", ")
  }
}


for (i in 1:length(rat_data[[1]])){
  rat_data[i] %>% names() %>% paste(collapse = ", ")
}

rat_data[[1]] %>% length()


rat_data$saved[, , ]$SavingSection.SaveTime %>%
  as.character() %>%
  substr(1, 11)



substr("VP05_Gap_Detection_20190828_113737.mat",
       start = 1,
       stop = gregexpr(pattern = "_",
                       "VP05_Gap_Detection_20190828_113737.mat") %>% unlist %>% `[` (1) - 1)






if (rat_data %>% names() %in% "SessionData" %>% any()) {
  data_source = "bpod"
}


if(rat_data %>% names() %in% "saved" %>% any()) {
  data_source = "bcontrol"
}






in_path <- file.path("D:",
                     "_Rig_data",
                     "SoloData",
                     "Data",
                     "emmett")
file_list <- list.files(file.path("D:",
                                  "_Rig_data",
                                  "SoloData",
                                  "Data",
                                  "emmett"),
                        pattern = "\\.mat$", ### only mat files 
                        recursive = T) %>%
  as.list() 
full_path <- paste0(in_path, "/", file_list)




tmp <- readBin(
  file.path("W:","swc","akrami","neuropixels_recordings","do02","09_09_2020","2020-09-09_14-13-09","recording_slot2_1.npx2"),
  what = "integer",
  endian = "little",
  # size = 2,
  n = 384 * 30000)

tmp <- tmp * 0.195 


tmp <- tmp %>% matrix(nrow = 384, byrow = F)
tmp %>% dim()

tmp_df <-  tmp %>% t() %>% as.tibble()

tmp_df <- tmp_df %>% mutate(time = row_number())
library(reshape2)
ggplot(data = tmp_df %>% melt(id.vars="time"), 
       mapping = aes(x= time, y = value)) +
  geom_line() +
  facet_wrap(~variable)



tmp[65,] %>% plot(type = "l")

# %>% 
#   plot(type = "l")
  


sBinFileName <-  file.path("W:","swc","akrami","neuropixels_recordings","do02","08_09_2020","2020-09-08_14-49-30","recording_slot2_1.npx2")

sBinFileName <-   file.path("W:","swc","akrami","neuropixels_recordings","do02","09_09_2020","2020-09-09_14-13-09","recording_slot2_1.npx2")

conBinFile <- file(description = sBinFileName, open = "rb")
### # loop reading the chunks
nLoopIdx <- 384*30000
while ( length(vecDataChunk <- readBin(con = conBinFile, what = "integer", n = nLoopIdx)) > 0 ) {
  cat("Line: ", nLoopIdx, " : ")
  print(vecDataChunk)
  ### # here we can do more computations on vecDataChunk
  vecDataChunk <- vecDataChunk %>% matrix(nrow = 384, byrow = F)
  vecDataChunk[65,] %>% plot(type = "l")
  ### # increment loop index
  nLoopIdx <- nLoopIdx + 1
}


# database

install.packages("DBI")
install.packages("odbc")
install.packages("ssh")
install.packages("RMariaDB")
install.packages("RMySQL")

library(odbc)
library(DBI)
library(ssh)
library(RMariaDB)
library(RMySQL)
sort(unique(odbcListDrivers()[[1]]))




gateway <- ssh_connect("vplattner@192.168.238.210")
print(gateway)

ssh_tunnel(session = ssh_connect("vplattner@192.168.238.210"),
           port = 8080,
           target = "172.24.155.100:80" )

ssh_tunnel()
ssh_disconnect(gateway)



#### first open an ssh tunnel: ssh -f vplattner@192.168.238.210 -L 8080:172.24.155.100:3306 -N
con <- dbConnect(MySQL(), 
                 user = "akrami", 
                 password = "Akrami2019!",
                 dbname = 'akrami_db',
                 host = "localhost",
                 port = 8080)



dbListTables(con)
dplyr::tbl(con, sql("SELECT * FROM mass_log"))

mass <- dplyr::tbl(con, "mass_log") %>%
  as_tibble()

sessions <- dplyr::tbl(con, "sessions") %>% 
  as_tibble()

water <- dplyr::tbl(con, "water_log") %>%
  as_tibble()





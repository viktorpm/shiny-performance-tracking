library(R.matlab)
library(rmatio)

library(rstudioapi)
library(tidyverse)
library(data.tree)
library(listviewer)
library(stringr)
library(purrr)

### loading functions ----
source("ConvertToRDS.R") ### f. to convert mat files to rds
source("ReadData.R") ### f. to read rds files to a tibble
source("TRAININGtoCSV.R") ### f. to save tibble to csv



### determining the full path of each file ----
in_path <- file.path("D:", "_Rig_data", "SoloData", "Data")
file_list <- list.files(file.path("D:", "_Rig_data", "SoloData", "Data"), recursive = T) %>% as.list()
full_path <- paste0(in_path, "/", file_list) %>% as.list()

### converting mat files to rds ----
walk(full_path, ~ ConvertToRDS(file = .x))



### reading the converted rds filenames to a list
### reading rds files to a tibble and saving them to a csv file ----
rds_list <- list.files(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files")) %>% as.list()
walk(rds_list, ~ ReadData(rds_file = .x) %>% TRAININGtoCSV())






# ConvertToRDS("D:/_Rig_data/SoloData/Data/viktor/VP05/data_@AthenaDelayComp_viktor_VP05_190626a.mat")
# ReadData("data_@AthenaDelayComp_athena_AA01_190612b.mat.rds") %>% TRAININGtoCSV()
# ReadData("data_@AthenaDelayComp_dammy_DO07_190625a.mat.rds") %>% TRAININGtoCSV()
# ReadData("data_@AthenaDelayComp_athena_AA01_190508a.mat.rds") %>% TRAININGtoCSV()
 
 
# ReadData("data_@SoundCategorization_experimenter_ratname_190620_ASV.mat.rds") %>% TRAININGtoCSV()

setdiff(rds_list %>% unlist(), TRAINING$file %>% unlist()) 

system2("D:", invisible = F)
system2("cd _Rig_data")
system2("bash")
system2("ssh -f vplattner@192.168.238.210 -L 8080:172.24.155.100:80 -N")
system2("cmd.exe")


shell(cmd = "c: & dir & mkdir proba", intern = T)





shell("bash; cd /mnt/d/_Rig_data/; svn update",intern = T)



shell("bash; ssh -f vplattner@192.168.238.210 -L 8080:172.24.155.100:80 -N" ,intern = T, wait = F)

shell("ssh -f vplattner@192.168.238.210 -L 8080:172.24.155.100:80 -N" ,intern = T, shell = "c:/windows/system32/bash.exe")


shell("ls", intern = T, shell = "bash")




library(R.matlab)
library(tidyverse)
library(data.tree)
library(listviewer)


rat_data <- readMat(file.path('X:','akrami','ratter','SoloData','Data','viktor','VP01','data_@AthenaDelayComp_viktor_VP01_190501a.mat')) 

rat_data$saved %>% length()

rat_data %>% length()
rat_data$saved[[4]][,,]

names <- rat_data$saved %>% dimnames() %>% `[[`(1) %>% as.matrix()




rat_data$saved[,,]$ProtocolsSection.raw.events

rat_data$saved[,,]$ProtocolsSection.latest.parsed.events[,,]$pokes[,,]$ending.state

rat_data$saved[,,]$















#---------------------

dtree <- FromListSimple(rat_data$saved)


install.packages("devtools")
devtools::install_github("ropenscilabs/roomba")
library(roomba)

rat_data %>% 
  roomba(cols = c("saved","saved.history","saved.autoset","fig.position"), keep = any)

devtools::install_github('timelyportfolio/reactR')
listviewer::reactjson(rat_data)

.First <- function() {
  # Source all utility functions
  source(file.path("utility_functions", "ConvertToRDS.R"))
  source(file.path("utility_functions", "ReadData.R"))
  source(file.path("utility_functions", "TRAININGtoCSV.R"))
  source(file.path("utility_functions", "ReadBpodData.R"))
  source(file.path("utility_functions", "ReadBcontrolData.R"))
  source(file.path("utility_functions", "ReadTrialData.R"))
  # to be completed: source(file.path("utility_functions", "ReadBonsaiData")) 
}



# Set paths to frequently used directories
path_to_mat_files <- file.path("/mnt", "ceph","_raw_data", "rat_training_172", "SoloData", "Data")
path_to_rds_files <- file.path("/mnt", "ceph","_raw_data", "rat_training_172", "rds_files")

# Load commonly used libraries
library(shiny)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(chron)
library(padr)
library(gridExtra)
library(forcats)
library(zoo)
library(bdscale)
library(tibble)
library(tidyverse)
library(R.matlab)
library(stringr)
library(purrr)
library(shinyjs)
library(DT)
library(plotly)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(parallel)
library(magrittr)

packages_to_install <- c("shiny",
                         "ggplot2",
                         "ggrepel",
                         "chron",
                         "padr",
                         "gridExtra",
                         "forcats",
                         "zoo",
                         "bdscale",
                         "magrittr",
                         "tibble",
                         "tidyverse",
                         "R.matlab",
                         "stringr",
                         "purrr",
                         "shinyjs",
                         "DT",
                         "plotly")


not_installed <- which(packages_to_install %in% rownames(installed.packages()) == F)

library(utils)
if (not_installed %>% length() != 0) {
  install.packages(packages_to_install[not_installed])
}



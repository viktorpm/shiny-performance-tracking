# List of required packages
required_packages <- c(
  "shiny", "ggplot2", "ggpubr", "ggrepel", 
  "chron", "padr", "gridExtra", "forcats", 
  "zoo", "bdscale", "tibble", "tidyverse",
  "R.matlab", "stringr", "purrr", "shinyjs", 
  "DT", "plotly", "knitr", "kableExtra",
  "rmarkdown", "parallel", "magrittr"
  )

# Function to check and install packages
check_and_install_packages <- function(pkg_list) {
  for (pkg in pkg_list) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Check and install required packages
check_and_install_packages(required_packages)

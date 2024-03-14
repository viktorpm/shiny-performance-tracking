#packages_to_install <- c("shiny",
                #         "ggplot2",
               #          "ggrepel",
              #           "chron",
             #            "padr",
            #             "gridExtra",
           #              "forcats",
          #               "zoo",
         #                "bdscale",
        #                 "magrittr",
       #                  "tibble",
      #                   "tidyverse",
     #                    "R.matlab",
    #                     "stringr",
   #                      "purrr",
  #                       "shinyjs",
 #                        "DT",
#                         "plotly")


# not_installed <- which(packages_to_install %in% rownames(installed.packages()) == F)

# library(utils)
# if (not_installed %>% length() != 0) {
#   install.packages(packages_to_install[not_installed])
# }

# Define packages to install
packages_to_install <- c("shiny", "ggplot2", "ggrepel", "chron", "padr", "gridExtra", "forcats", "zoo", "bdscale", "magrittr", "tibble", "tidyverse", "R.matlab", "stringr", "purrr", "shinyjs", "DT", "plotly")

# Check which packages are not installed
not_installed <- setdiff(packages_to_install, rownames(installed.packages()))

# Install packages that are not installed
if (length(not_installed) > 0) {
 install.packages(not_installed)
}

# Load all installed packages
lapply(packages_to_install, library, character.only = TRUE)


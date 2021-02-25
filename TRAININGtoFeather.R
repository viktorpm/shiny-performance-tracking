TRAININGtoFeather <- function(data, filename) {
  library(tidyverse)
  library(feather)
  
  if (missing(filename)) {
    filename = "TRAINING.feather"
  }
  
  path <- file.path(
    "shiny_app"
  )
  # browser()
  
  if (is.null(data)) {
    warning(
      "File has already been processed"
    )
  } else {
    feather_exists <- file.exists(file.path(path, filename))
    
    write_TRAINING <- as_tibble(data, .name_repair = "minimal")
    write_feather(
      write_TRAINING,
      file.path(
        path,
        filename
      )
    )
  }
}



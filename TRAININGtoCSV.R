TRAININGtoCSV <- function(data) {
  library(tidyverse)
  path <- file.path(
    "output_data"
  )

  # browser()
   



  csv_exists <- file.exists(file.path(path, "TRAINING.csv"))


  if (csv_exists == T) {
    read_TRAINING <- suppressMessages(suppressWarnings(
      read_csv(file.path(path, "TRAINING.csv")))
      )
    file_processed_test <- str_detect(
      read_TRAINING$file,
      regex(paste0(data$file))
    )
  } else {
    read_TRAINING <- data.frame()
    file_processed_test <- F
  }





  if (all(file_processed_test == F)) {
    write_TRAINING <- as_tibble(data, .name_repair = "minimal")
    write_csv(
      write_TRAINING,
      file.path(
        path,
        "TRAINING.csv"
      ),
      append = T,
      col_names = !csv_exists
    )
  } else {
    warning(paste0(
      data$file,
      # ".mat: ",
      "File has already been processed"
    ))
  }
}

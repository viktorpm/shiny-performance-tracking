TRAININGtoCSV <- function(data) {
  library(tidyverse)
  path <- file.path(
    "shiny_app"
  )
  # browser()

  if (is.null(data)) {
    warning(
      "File has already been processed"
    )
  } else {
    csv_exists <- file.exists(file.path(path, "TRAINING.csv"))

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
  }
}

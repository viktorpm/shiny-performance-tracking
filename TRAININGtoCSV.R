TRAININGtoCSV <- function(data, filename) {
  
  if (missing(filename)) {
    filename = "TRAINING.csv"
  }
  
  path <- file.path(getwd(),"shiny_app")
  # browser()

  if (is.null(data)) {
    warning(
      "File has already been processed"
    )
  } else {
    csv_exists <- file.exists(file.path(path, filename))

    write_TRAINING <- as_tibble(data, .name_repair = "minimal")
    write_csv(
      write_TRAINING,
      file.path(
        path,
        filename
      ),
      append = T,
      col_names = !csv_exists
    )
  }
}

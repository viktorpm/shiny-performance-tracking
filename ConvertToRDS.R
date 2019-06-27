ConvertToRDS <- function(file, save_path) {
  # browser()
  library(R.matlab)

  ### tests if save_path was defined
  if (missing(save_path) == T) {
    save_path <- file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files")
  }


  ### finds the index of the last "/", idetifies the start of the filename
  filename_pos <- gregexpr(file,
    pattern = "/"
  ) %>%
    unlist() %>%
    `[[`(length(.)) + 1

  ### saves the filename
  filename <- substr(
    file,
    start = filename_pos,
    stop = nchar(file)
  )


  ### saves the file path
  file_path <- substr(
    file,
    start = 1,
    stop = filename_pos - 1
  )



  ### gets the list of files from save_path
  file_list <- list.files(save_path) %>% as.list()

  filename_test <- paste0(filename, ".rds")

  if (any(str_detect(string = file_list, pattern = filename_test)) == T) {
    warning(paste0(
      filename,
      " ",
      "File has already been processed"
    ))
  } else {
    rat_data <- readMat(paste0(file_path, filename))

    saveRDS(rat_data,
      file = paste0(
        save_path,
        "/",
        filename,
        ".rds"
      )
    )
  }
}

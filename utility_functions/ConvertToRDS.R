# Function to convert .mat files to .rds format
ConvertToRDS <- function(file, save_path) {
  
  # Check if save_path is provided, if not, set a default path
  if (missing(save_path)) {
    save_path <- file.path(path_to_rds_files)
  }
  
  # Find the position of the last "/" to identify the start of the filename
  filename_pos <- gregexpr(file, pattern = "/") %>%
    unlist() %>%
    `[[`(length(.)) + 1
  
  # Extract the filename from the full path
  filename <- substr(file, start = filename_pos, stop = nchar(file))
  
  # Extract the file path
  file_path <- substr(file, start = 1, stop = filename_pos - 1)
  
  # Read the .mat file using R.matlab's readMat function
  rat_data <- R.matlab::readMat(paste0(file_path, filename))
  
  # Save the data as an .rds file in the specified save_path
  saveRDS(rat_data, file = paste0(save_path, "/", filename, ".rds"))
}

# Documentation:
# Variables:
# - file: The full path of the .mat file to be converted.
# - save_path: The path where the converted .rds file will be saved. If not provided, a default path is used.
# - filename_pos: The position of the last "/" in the file path, used to extract the filename.
# - filename: The name of the .mat file to be converted.
# - file_path: The directory path of the .mat file.
# - rat_data: The data read from the .mat file using R.matlab's readMat function.

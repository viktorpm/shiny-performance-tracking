# Load ggplot2 package for plotting
library(ggplot2)

# Read joystick data from CSV file
#' @param file_path The path to the CSV file containing joystick data.
joystick <- read_csv(file.path("D:", "joystick_protocol", "Bonsai", "Summary_statistics.csv"))

# Convert column names to lowercase for consistency
#' @param df The data frame whose column names are to be converted to lowercase.
names(joystick) <- tolower(names(joystick))

# Add an index column to the data frame
#' @param df The data frame to which an index column will be added.
joystick <- joystick %>%
  dplyr::mutate(index = row_number())

# Extract session details from session_id column
#' @param df The data frame from which session details will be extracted.
#' @param col_name The name of the column containing session IDs.
#' @param into A vector of names for the extracted columns.
#' @param regex A regular expression pattern for extracting session details.
joystick <- joystick %>%
  tidyr::extract(
    col = session_id, 
    into = c("test", "animal", "phase", "day", "month", "year", "session"), 
    regex = "([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)")

# Plot trials against index
#' @param df The data frame containing the data to be plotted.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
joystick %>%
  ggplot(mapping = aes(x = index, y = trials)) +
  geom_point()

# Plot ratio of good pushes to bad pushes against index
joystick %>%
  ggplot(mapping = aes(x = index, y = good_pushes/bad_pushes)) +
  geom_point()

# Plot final hold time against index
joystick %>%
  ggplot(mapping = aes(x = index, y = final_holdtime)) +
  geom_point()

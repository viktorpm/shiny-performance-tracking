mass <- read.csv(file.path("../", "Mass Log.csv"), na.strings = c("", "NA")) %>%
  as_tibble() %>%
  dplyr::select(-X, -X.1) %>%
  na.omit() %>%
  unite(col = "dm", date, mass, sep = "_") %>%
  unite(col = "dm1", date.1, mass.1, sep = "_") %>%
  unite(col = "dm2", date.2, mass.2, sep = "_") %>%
  unite(col = "dm3", date.3, mass.3, sep = "_") %>%
  unite(col = "dm4", date.4, mass.4, sep = "_") %>%
  unite(col = "dm5", date.5, mass.5, sep = "_") %>%
  unite(col = "dm6", date.6, mass.6, sep = "_") %>%
  unite(col = "dm7", date.7, mass.7, sep = "_") %>%
  unite(col = "dm8", date.8, mass.8, sep = "_") %>%
  unite(col = "dm9", date.9, mass.9, sep = "_") %>%
  unite(col = "dm10", date.10, mass.10, sep = "_") %>%
  gather(
    key = "dm_cols", value = "dm_values",
    dm, dm1, dm2, dm3, dm4, dm5, dm6, dm7, dm8, dm9, dm10
  ) %>%
  tidyr::extract(
    col = dm_values,
    into = c("date", "mass"),
    regex = "([[:alnum:][:punct:] ]+)_([[:alnum:]]+)"
  ) %>%
  dplyr::rename("animal_id" = 1, "exp_id" = 2) %>%
  dplyr::select(-dm_cols) %>%
  dplyr::mutate(date = as.Date(date, tryFormats = c("%d/%m/%Y"))) %>%
  dplyr::mutate(exp_id = tolower(exp_id)) 


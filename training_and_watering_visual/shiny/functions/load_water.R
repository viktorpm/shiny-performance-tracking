akrami_db <- DBI::dbConnect(MySQL(),
                       user = "akrami",
                       password = "Akrami2019!",
                       dbname = "akrami_db",
                       host = "localhost",
                       port = 8080
)



sessions <- dplyr::tbl(
  src = akrami_db,
  sql("SELECT ratname, experimenter, sessiondate, endtime, starttime FROM sessions")
) %>%
  as_tibble() %>%
  dplyr::rename(
    animal_id = ratname,
    date = sessiondate,
    exp_id = experimenter
  ) %>%
  dplyr::mutate(
    date = as.Date(date),
    endtime = as.POSIXct(endtime, format = "%H:%M:%S") %>% format(format = "%H:%M"),
    starttime = as.POSIXct(starttime, format = "%H:%M:%S") %>% format(format = "%H:%M")
  )


water <- read.csv(file.path("../","Water Log.csv")) %>%
  as_tibble()

water <- water %>%
  dplyr::mutate(
    date = as.Date(date, tryFormats = c("%d/%m/%Y")),
    water.start = as.POSIXct(water.start, format = "%H:%M") %>% format(format = "%H:%M"),
    water.end = as.POSIXct(water.end, format = "%H:%M") %>% format(format = "%H:%M")
  ) %>%
  dplyr::select(1:5) %>%
  dplyr::rename("animal_id" = 1, "exp_id" = 2) %>%
  dplyr::mutate(exp_id = tolower(exp_id))


time_ranges <- left_join(water, sessions) %>%
  dplyr::filter(animal_id != "") %>%
  dplyr::arrange(animal_id) %>%
  dplyr::rename(
    "Animal" = animal_id,
    "Experimenter" = exp_id,
    "Date" = date,
    "Training ON" = starttime,
    "Training OFF" = endtime,
    "Water ON" = water.start,
    "Water OFF" = water.end
  ) %>%
  dplyr::mutate(
    `Training (min)` = difftime(
      `Training OFF` %>% as.POSIXct(format = "%H:%M"),
      `Training ON` %>% as.POSIXct(format = "%H:%M"),
      units = "min"
    ) %>% as.numeric() %>% replace_na(""),
    `Training (min)` = ifelse(`Training (min)` <= 5, "", `Training (min)`),
    `Watering (min)` = difftime(
      `Water OFF` %>% as.POSIXct(format = "%H:%M"),
      `Water ON` %>% as.POSIXct(format = "%H:%M"),
      units = "min"
    ) %>% as.numeric() %>% replace_na("")
  ) %>%
  dplyr::relocate(`Training ON`, .before = `Training OFF`)

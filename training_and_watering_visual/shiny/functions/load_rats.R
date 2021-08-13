akrami_db <- DBI::dbConnect(MySQL(),
                            user = "akrami",
                            password = "Akrami2019!",
                            dbname = "akrami_db",
                            host = "localhost",
                            port = 8080
)


### rats that are scheduled to train
### to filter the ones that are on water restriction and need to be weighted
scheduler <- dplyr::tbl(
  src = akrami_db,
  sql("SELECT * FROM scheduler")) %>%
  as_tibble() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::filter(
    date == max(date),
    experimenter != "")


### all the rats
### mass: loaded separately, contains all the weights 
rats <- dplyr::tbl(
  src = akrami_db,
  sql("SELECT * FROM rats")
) %>% 
  as_tibble()

record_weights <- rats %>% 
  dplyr::select(ratname, nrf_id, cage, experimenter) %>% 
  dplyr::rename("animal_id" = ratname)

record_weights <- left_join(
  record_weights,
  mass %>% 
    dplyr::filter(date == max(date, na.rm = T)) %>% 
    dplyr::select(animal_id, mass)
) %>% 
  dplyr::filter(animal_id %in% scheduler$ratname)


### rats that are scheduled to train
### to filter the ones that are on water restriction and need to be weighted
scheduler <- dplyr::tbl(
  src = akrami_db,
  sql("SELECT * FROM scheduler")) %>%
  as_tibble() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::filter(
    date == max(date),
    experimenter != "")
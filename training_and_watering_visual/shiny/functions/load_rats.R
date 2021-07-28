akrami_db <- DBI::dbConnect(MySQL(),
                            user = "akrami",
                            password = "Akrami2019!",
                            dbname = "akrami_db",
                            host = "localhost",
                            port = 8080
)

rats <- dplyr::tbl(
  src = akrami_db,
  sql("SELECT * FROM rats")
) %>% 
  as_tibble()

record_weights <- rats %>% 
  dplyr::select(ratname, nrf_id, cage) %>% 
  dplyr::rename("animal_id" = ratname)

record_weights <- left_join(
  record_weights,
  mass %>% 
    dplyr::filter(date == max(date, na.rm = T)) %>% 
    dplyr::select(animal_id, mass)
) 
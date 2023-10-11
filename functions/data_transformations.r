#this file contains a function that allows us to put the data that 
#we initially have in the form of a list into a data-frame format


transform_metadata_to_df <- function(stations_metadata) {
  stations_metadata[[1]] |> 
    map(as_tibble) |> 
    list_rbind() |> 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) |> 
    mutate(latestData = as_datetime(latestData, tz = "UTC"))  |> 
    unnest_wider(location) |> 
    unnest_wider(latLon)
}


to_iso8601 <- function(date_time_var, offset_in_days) {
  modified_date <- lubridate::as_datetime(date_time_var) + lubridate::days(offset_in_days)
  iso8601_date <- paste(format(modified_date, tz="UTC"), "Z", sep = "")
  return(iso8601_date)
}


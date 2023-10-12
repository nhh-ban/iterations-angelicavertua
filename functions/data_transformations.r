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

#function that should return the date time variable in ISO8601 format, 
#with the offset added. There should be a letter "Z" appended to the end 
#of the date string, to indicate the the time zone is UTC

to_iso8601 <- function(dt, offset) {
  dt <- as_datetime(dt) + days(offset)
  dt <- format(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  return(dt)
}


transform_volumes <- function(json_response) {
  # Extract the relevant information from the JSON response
  volume_data <- json_response$trafficData$volume$byHour$edges
  
  # Create an empty data frame to store the results
  result_df <- data.frame()
  
  # Iterate over each data point in volume_data
  for (edge in volume_data) {
    node <- edge$node
    from <- node$from
    to <- node$to
    volume <- node$total$volumeNumbers$volume
    
    # Create a data frame for the current data point
    data_point <- data.frame(from, to, volume)
    
    # Append the data point to the result data frame
    result_df <- rbind(result_df, data_point)
  }
  
  # Convert 'from' and 'to' columns to POSIXct format with UTC timezone
  result_df$from <- as.POSIXct(result_df$from, tz = "UTC")
  result_df$to <- as.POSIXct(result_df$to, tz = "UTC")
  
  return(result_df)
}


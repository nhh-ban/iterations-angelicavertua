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

to_iso8601 <- function(date_time_var, offset_in_days) {
  modified_date <- lubridate::as_datetime(date_time_var) + lubridate::days(offset_in_days)
  iso8601_date <- paste(format(modified_date, tz="UTC"), "Z", sep = "")
  return(iso8601_date)
}

# Create a function to generate volume queries in Vegvesen format

generate_vegvesen_volume_query <- function(station_id, from_date, to_date) {
  query <- glue::glue("
    {{
      trafficData(trafficRegistrationPointId: '{station_id}') {{
        volume {{
          byHour(from: '{from_date}', to: '{to_date}') {{
            edges {{
              node {{
                from
                to
                total {{
                  volumeNumbers {{
                    volume
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}
  ")
  
  return(query)
}



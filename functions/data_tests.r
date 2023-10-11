# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

#this function it's designed to check whether a given data frame df 
#has the expected column names ("id", "name", "latestData", "lat", "lon") 
#if the columns do not match, the function provides a "FAIL" message, signaling 
#that the data may not be in the expected format. If they do match, it provides
#a "PASS" message, indicating that the correct column names have been specified

test_stations_metadata_colnames <-
  function(df) {
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }


#this function it's designed to check the number of rows in a given data frame df 
#and determine whether it falls within a specified range of expected row counts
#It defines min_expected_rows and max_expected_rows as the minimum and maximum 
#expected row counts and then compares the number of rows to the expected range

test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

#this function it's designed to check whether the data types (column types) of 
#the columns in a given data frame df match the expected data types, in this 
#case they should be ("character", "character", "double", "double", "double")

test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }

#this function it's designed to check the number of missing (NA) values in a 
#given data frame df and determine whether the amount of missing data is within 
#an acceptable limit

test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

#this function it's designed to check the time zone associated with the 
#latestData column in a given data frame df and determine whether it matches 
#the expected time zone, which is "UTC"

test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

#this function it's designed to be a composite test function that calls 
#the previous test functions to assess the quality and correctness of a 
#data frame df. The purpose of this function is to perform a comprehensive 
#quality check on the data frame by running a series of individual tests 
#and reporting the results.

test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }






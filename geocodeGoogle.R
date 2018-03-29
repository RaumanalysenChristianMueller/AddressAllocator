# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script uses the geocoding service of google maps to geocode addresses
# It was mainly inspired by this post: https://www.r-bloggers.com/batch-geocoding-with-r-and-google-maps/

geocodeGoogle <- function(data, street, houseNumber, houseNumberAppendix, zipCode, placeName){
  
  
  #define a function that will process googles server responses for us.
  getGeoDetails <- function(address){   
    
    #use the gecode function to query google servers
    geo_reply = geocode(address, output = 'all', messaging = T, override_limit = T)
    
    #now extract the bits that we need from the returned list
    answer <- data.frame(lat = NA, long = NA, accuracy = NA,
                         formatted_address = NA, address_type = NA, status = NA)
    answer$status <- geo_reply$status
    
    #if we are over the query limit - want to pause for an hour
    while(geo_reply$status == "OVER_QUERY_LIMIT"){
      cat("OVER QUERY LIMIT - Pausing for 10 secondes at:") 
      time <- Sys.time()
      cat(as.character(time))
      Sys.sleep(10)
      geo_reply = geocode(address, output = 'all', messaging = T, override_limit = T)
      answer$status <- geo_reply$status
    }
    
    #return Na's if we didn't get a match:
    if (geo_reply$status != "OK"){
      return(answer)
    }
    
    #else, extract what we need from the Google server reply into a dataframe:
    answer$lat <- geo_reply$results[[1]]$geometry$location$lat
    answer$long <- geo_reply$results[[1]]$geometry$location$lng   
    if (length(geo_reply$results[[1]]$types) > 0){
      answer$accuracy <- geo_reply$results[[1]]$types[[1]]
    }
    answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
    answer$formatted_address <- geo_reply$results[[1]]$formatted_address
    
    return(answer)
  }
  
  
  
  # combine addresses to vector
  addresses <- paste0(data[, street], " ", data[, houseNumber], data[, houseNumberAppendix], ", ", data[, placeName])
  
  # remove NAs
  addresses <- gsub(x = addresses, pattern = "NA", replacement = "")
  
  # prepare output container
  out <- cbind.data.frame(addressID = data[,"addressID"], searchAddress = addresses,
                          xcoords = "", ycoords = "", stringsAsFactors = F)
  
  # get unique addresses
  uaddresses <- unique(addresses)
  
  
  # Start the geocoding process - address by address. geocode() function takes care of query speed limit.
  for (a in 1:length(uaddresses)){
    
    # report progress
    cat(paste("Working on index", a, "of", length(addresses)))
    
    #query the google geocoder - this will pause here if we are over the limit.
    result = getGeoDetails(uaddresses[a]) 
    cat(result$status)     
    
    #append the answer to the results object
    aPos <- which(out[,"searchAddress"] == uaddresses[a])
    out[a,"xcoords"] <- result$long
    out[a,"ycoords"] <- result$lat
    
  }
  
  # return function output
  return(out)
  
}
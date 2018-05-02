# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script uses the geocoding service of OpenStreetMap - Nominatim to geocode addresses
# It was mainly inspired by this post: https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/

geocodeOSM <- function(data, street, houseNumber, houseNumberAppendix, zipCode, placeName){

  # define function for calling Nominatim
  nominatim_osm <- function(address = NULL){
    if(suppressWarnings(is.null(address)))
      
      return(data.frame())
    
    tryCatch(
    
        d <- jsonlite::fromJSON( 
      
            gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
                 'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
            ), error = function(c) return(data.frame())
        
    )
    
    if (("d" %in% ls()) == F) return(data.frame())
    
    if (length(d) == 0) return(data.frame())
    
    
    return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
    
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
  
  # iterate over each address
  for (a in 1:length(uaddresses)){
    
    # report progress
    pb_val <- 30 + (a/length(uaddresses) * 15)
    try(tkconfigure(tk_pb, value = pb_val, maximum = 100), silent = T)
    
    # get coordinates from Nominatim
    api_output <- nominatim_osm(uaddresses[a])
    
    # write information to container if information was received from Nominatim's API
    if (length(api_output) > 0){
      
      aPos <- which(out[,"searchAddress"] == uaddresses[a])
      out[aPos, "xcoords"] <- api_output[1]
      out[aPos, "ycoords"] <- api_output[2]
      
    }
    
    # wait for one second due to Nominatim's usage policy
    Sys.sleep(1.01)
    
  }

  return(out)
  
}
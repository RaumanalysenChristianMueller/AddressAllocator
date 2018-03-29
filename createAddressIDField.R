# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script creates an address-ID-field from street, housenumber, housenumber appendix, zip code and place name

createAddressIDField <- function(data, streetCol, houseNrCol, houseNrApCol, zipCol, placeCol, ref_data = ""){
  
  
  # remove previous addressID field
  if ("addressID" %in% colnames(data)) data <- data[,-which(colnames(data) == "addressID")]
  
  
  # prepare individual address fields
  street <- data[,streetCol]
  houseNumber <- data[,houseNrCol]
  houseNumberAppendix <- data[,houseNrApCol]
  zipCode <- data[,zipCol]
  placeName <- data[,placeCol]
  
  
  # create addressID field
  out_data <- cbind(data, addressID = paste0(street, houseNumber, houseNumberAppendix, "_", zipCode, placeName))
  
  # return appended data
  return(out_data)
  
}


# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script adds information on address locations to the existing address data table and condenses the data:
# transfer data, rename columns, add data source, delete redundant columns


addAddressLocationsToDataTable <- function(existingData, newData, newDataSourceName){
  
  # add new coordinates address data table
  existingData <- left_join(x = existingData, y = newData, by = "addressID")
  
  # condense data table (transfer data, rename columns, add data source, delete redundant columns)
  newPos <- which(!is.na(existingData[,"searchAddress"]))
  existingData[newPos, "xcoords.x"] <- existingData[newPos, "xcoords.y"]
  existingData[newPos, "ycoords.x"] <- existingData[newPos, "ycoords.y"]
  newpos2 <- which(existingData[newPos, "xcoords.x"] != "")
  if (length(newpos2) > 0) existingData[newPos[newpos2], "source"] <- newDataSourceName
  existingData <- existingData[,-which(colnames(existingData) == "xcoords.y")]
  existingData <- existingData[,-which(colnames(existingData) == "ycoords.y")]
  existingData <- existingData[,-which(colnames(existingData) == "searchAddress")]
  colnames(existingData)[which(colnames(existingData) == "xcoords.x")] <- "xcoords"
  colnames(existingData)[which(colnames(existingData) == "ycoords.x")] <- "ycoords"

  return(existingData)
    
}
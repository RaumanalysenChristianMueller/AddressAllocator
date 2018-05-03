# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script filters address references for place names which occur in the to be geocoded tabular address data file

filterAdminAdresses_addInfo <- function(toBeGeocodedPlaces, adminAdd, referenceData,
                                        addPlaceNames = F, addZipCode = F, zipCodes){
  
  
  # collect occuring place names in to be geocoded data
  placeNames <- unique(toBeGeocodedPlaces)
  
  
  # create container for data positions of administrative addresses to be further used
  adminAdd_usePos <- c()
  
  
  # add place name column to data
  if (addPlaceNames) adminAdd <- cbind.data.frame(adminAdd, placeName = "", stringsAsFactors = F)
  
  # add zip code column to data
  if (addZipCode) adminAdd <- cbind.data.frame(adminAdd, zipCode = "", stringsAsFactors = F)
  
  # iterate over each place name in to be geocoded addresses
  for (p in 1:length(placeNames)){
    
    # get position of respective place name in reference data
    pos <- which(referenceData == placeNames[p], arr.ind = T)
    
    # continue if the name was found in reference data
    if (length(pos) > 0){
      
      # get reference data line
      ref_line <- as.character(referenceData[pos[1,1],])
      
      # get number of used columns for the code
      ncols <- length(ref_line) - 2
      
      # first round of filtration: 'land'
      pos <- which(as.numeric(adminAdd[,"land"]) == as.numeric(ref_line[2]))
      
      # second round of filtration: 'regbez'
      if (ncols > 1) {
        temp <- which(as.numeric(adminAdd[pos,"regbez"]) == as.numeric(ref_line[3]))
        pos <- pos[temp]
      }
      
      # third round of filtration: 'kreis'
      if (ncols > 2) {
        temp <- which(as.numeric(adminAdd[pos,"kreis"]) == as.numeric(ref_line[4]))
        pos <- pos[temp]
      }
      
      # forth round of filtration: 'gemeinde'
      if (ncols > 3) {
        temp <- which(as.numeric(adminAdd[pos,"gemeinde"]) == as.numeric(ref_line[5]))
        pos <- pos[temp]
      }
      
      # fith round of filtration: 'gemeindeteil'
      if (ncols > 4) {
        temp <- which(as.numeric(adminAdd[pos,"gemeindeteil"]) == as.numeric(ref_line[6]))
        pos <- pos[temp]
      }
      
      
      # add place name to respective data rows
      if (addPlaceNames) adminAdd[pos,"placeName"] <- placeNames[p]
      
      # add place name to respective data rows
      zip_pos <- which(zip[,2] == placeNames[p])
      if (length(zip_pos) > 0) if (addZipCode) adminAdd[pos,"zipCode"] <- zip[zip_pos, 3]
      
      
      # write to collection container
      adminAdd_usePos <- c(adminAdd_usePos, pos)
      
    }
    
  }
  
  # get unique data positions
  adminAdd_usePos <- unique(adminAdd_usePos)
  
  # filter addresses
  adminAdd_use <- adminAdd[adminAdd_usePos,]
  
  # return filtered addresses
  return(adminAdd_use)
  
}

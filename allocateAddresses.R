# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# The AddressAllocator geocodes addresses in a three step procedure:
# 1. Matching with official administrative building references (tested for Germany - Northrhine-Westfalia)
# 2. Geocoding by Nominatim/OSM API
# 3. Geocoding by Google API
# This script is designed for usage in QGIS

## inputs for debugging
# addressFile = "C:/HochschuleBochum/Daten/Herdecke/EinwMeldeAmt/EinwMeldeAmt.xlsx"
# streetCol = "str."
# houseNrCol = "HAUSNR"
# houseNrApCol = "ZUSATZ"
# zipCol = "PLZ"
# placeCol = "ORT"
# adminAddDirectory = ""
# outShp = "C:/HochschuleBochum/Daten/Herdecke/EinwMeldeAmt/OutTest.shp"
# missingAddsOut = "C:/HochschuleBochum/Daten/Herdecke/EinwMeldeAmt/OutTest.csv"


# install/load libraries
for (i in 1:2){
  if (!require("readxl")) install.packages("readxl", dependencies = T)
  if (!require("dplyr")) install.packages("dplyr", dependencies = T)
  if (!require("jsonlite")) install.packages("jsonlite", dependencies = T)
  if (!require("ggmap")) install.packages("ggmap", dependencies = T)
  if (!require("sp")) install.packages("sp", dependencies = T)
  if (!require("rgdal")) install.packages("rgdal", dependencies = T)
}


# load address data
sh_names <- excel_sheets(addressFile)
addtab <- as.data.frame(read_excel(addressFile, sh_names[1]))


# build address-ID-field for tabular address data
source("createAddressIDField.r")
addtab <- createAddressIDField(data = addtab, streetCol = streetCol,
                               houseNrCol = houseNrCol, houseNrApCol = houseNrApCol,
                               zipCol = zipCol, placeCol = placeCol)


# download and unzip administrative addressess and zip codes when not provided as file (only functional for Germany - Northrhine-Westphalia!)
if (adminAddDirectory == ""){
  
  download.file("https://www.opengeodata.nrw.de/produkte/geobasis/lika/alkis_sek/gebref/gebref_EPSG4647_ASCII.zip",
                paste0(tempdir(), "/adminAddZip.zip"))
  
  unzip(paste0(tempdir(), "/adminAddZip.zip"), exdir = tempdir())
  
  adminAddDirectory <- tempdir()
  
  download.file("https://www.suche-postleitzahl.org/download_files/public/zuordnung_plz_ort.csv",
                paste0(tempdir(), "/zipCodes.csv"))
  
}


# load administrative address data
adminAdd <- read.table(paste0(adminAddDirectory, "/gebref.txt"),
                       sep = ";", dec = ",", header = T, as.is = T, encoding = "UTF-8")


# load reference data for coded place names
ref <- read.table(paste0(adminAddDirectory, "/gebref_schluessel.txt"),
                  sep = ";", encoding = "UTF-8", fill = T, as.is = T)

# load zip codes
zip <- read.table(paste0(adminAddDirectory, "/zipCodes.csv"),
                  sep = ",", encoding = "UTF-8", as.is = T)


# filter administrative addresses for place names in the tabular data file
source("filterAdminAdresses_addInfo.r")
adminAdd <- filterAdminAdresses_addInfo(toBeGeocodedPlaces = addtab[,placeCol],
                                adminAdd = adminAdd, referenceData = ref,
                                addPlaceNames = T, addZipCode = T, zipCodes = zip)


# build address-ID-field for administrative address data
adminAdd <- createAddressIDField(data = adminAdd, streetCol = "strassenname",
                                 houseNrCol = "hsnr", houseNrApCol = "hsnrzusatz",
                                 zipCol = "zipCode", placeCol = "placeName")

# unify address-ID-fields
source("unifyAddressIDField.r")
addtab <- unifyAddressIDField(data = addtab, addressIDField = "addressID")
adminAdd <- unifyAddressIDField(data = adminAdd, addressIDField = "addressID")



# join data sets
addtab <- left_join(x = addtab, y = adminAdd, by = "addressID")


# remove redundant columns
remCols <- colnames(adminAdd)
remCols <- remCols[-which(remCols == "rechtswert")]
remCols <- remCols[-which(remCols == "hochwert")]
remCols <- remCols[-which(remCols == "addressID")]
addtab <- addtab[,-which(colnames(addtab) %in% remCols)]


# create columns for x- and y-coordinates and data source information
addtab <- cbind(addtab, xcoords = addtab[,"rechtswert"], ycoords = addtab[,"hochwert"],
                source = "", stringsAsFactors = F)
geocodedPos <- which(!is.na(addtab[,"xcoords"]))
addtab[geocodedPos,"source"] <- "Amtliche_Adresse"



# extract addresses which were not geocoded
misPos <- which(is.na(addtab[,"xcoords"]))
mis <- addtab[misPos,]


# geocode missing addresses via Nominatim/OSM
source("geocodeOSM.r")
osmAdds <- geocodeOSM(data = mis, street = streetCol, houseNumber = houseNrCol,
                      houseNumberAppendix = houseNrApCol, zipCode = zipCol, placeName = placeCol)


# add new coordinates address data table
addtab <- addAddressLocationsToDataTable(existingData = addtab, newData = osmAdds,
                                         newDataSourceName = "OpenStreetMap")


# extract addresses which were not geocoded
misPos2 <- which(is.na(addtab[,"xcoords"]))
mis2 <- addtab[misPos2,]


# google only allows this geocoding, if the data is shown with a google basemap. Therefore it is commented out: https://developers.google.com/maps/documentation/geocoding/policies
# source("geocodeGoogle.r")
# googleAdds <- geocodeGoogle(data = mis2, street = streetCol, houseNumber = houseNrCol,
#                             houseNumberAppendix = houseNrApCol, zipCode = zipCol, placeName = placeCol)
# 
# 
# # add new coordinates address data table
# source("addAddressLocationsToDataTable.r")
# addtab <- addAddressLocationsToDataTable(existingData = addtab, newData = googleAdds,
#                                          newDataSourceName = "Google")



# extract addresses which were not geocoded
misPos3 <- which(is.na(addtab[,"xcoords"]))
mis3 <- addtab[misPos3,]


# write missing addresses to file
write.table(x = mis3[,1:(ncol(mis3)-6)], file = missingAddsOut, sep = ";", dec = ".",
            col.names = T, row.names = F)



# build spatial object out of addresses geocoded by administrative building references
sPos <- which(addtab[,"source"] == "Amtliche_Adresse")
if (length(sPos) > 0) {
  sp_addsAdmin <- SpatialPointsDataFrame(coords = cbind(as.numeric(addtab[sPos, "xcoords"]),
                                                        as.numeric(addtab[sPos, "ycoords"])),
                                         data = addtab[sPos,], proj4string =  CRS("+init=epsg:25832"))
  sp_out <- sp_addsAdmin
}
  
# build spatial object out of addresses geocoded by Nominatim/OSM
sPos <- which(addtab[,"source"] == "OpenStreetMap")
if (length(sPos) > 0){
  sp_addsOsm <- SpatialPointsDataFrame(coords = cbind(as.numeric(addtab[sPos, "xcoords"]),
                                                        as.numeric(addtab[sPos, "ycoords"])),
                                         data = addtab[sPos,], proj4string =  CRS("+init=epsg:3857"))
  
  # transform data
  sp_addsOsm <- spTransform(sp_addsOsm, CRS("+init=epsg:25832"))
  sp_out <- rbind(sp_out, sp_addsOsm)
  
}


# google only allows this geocoding, if the data is shown with a google basemap. Therefore it is commented out: https://developers.google.com/maps/documentation/geocoding/policies
# # build spatial object out of addresses geocoded by google
# sPos <- which(addtab[,"source"] == "Google")
# if (length(sPos) > 0){
#   sp_addsGoogle <- SpatialPointsDataFrame(coords = cbind(as.numeric(addtab[sPos, "xcoords"]),
#                                                       as.numeric(addtab[sPos, "ycoords"])),
#                                        data = addtab[sPos,], proj4string =  CRS("+init=epsg:4326"))
#   
#   # transform data
#   sp_addsGoogle <- spTransform(sp_addsGoogle, CRS("+init=epsg:25832"))
#   sp_out <- rbind(sp_out, sp_addsGoogle)
#   
# }


# remove temporary coordinate data
pos <- which(colnames(sp_out@data) == "rechtswert")
if (length(pos) > 0) sp_out@data <- sp_out@data[,-pos]
pos <- which(colnames(sp_out@data) == "hochwert")
if (length(pos) > 0) sp_out@data <- sp_out@data[,-pos]
pos <- which(colnames(sp_out@data) == "xcoords")
if (length(pos) > 0) sp_out@data <- sp_out@data[,-pos]
pos <- which(colnames(sp_out@data) == "ycoords")
if (length(pos) > 0) sp_out@data <- sp_out@data[,-pos]
pos <- which(colnames(sp_out@data) == "xcoords.1")
if (length(pos) > 0) sp_out@data <- sp_out@data[,-pos]
pos <- which(colnames(sp_out@data) == "ycoords.1")
if (length(pos) > 0) sp_out@data <- sp_out@data[,-pos]


# add actual coordinates data
sp_out@data <- cbind(sp_out@data, coordinates(sp_out))
colnames(sp_out@data)[ncol(sp_out@data)-1] <- "xcoords"
colnames(sp_out@data)[ncol(sp_out@data)] <- "ycoords"


# write results to shapefile
writeOGR(sp_out, dsn = dirname(outShp),
         layer = strsplit(basename(outShp), ".", fixed = T)[[1]][1],
         driver = "ESRI Shapefile")


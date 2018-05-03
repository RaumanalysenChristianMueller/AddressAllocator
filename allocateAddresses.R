# AddressAllocator
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# The AddressAllocator geocodes addresses in a three step procedure:
# 1. Matching with official administrative building references (tested for Germany - Northrhine-Westfalia)
# 2. Geocoding by Nominatim/OSM API
# 3. Geocoding by Google API
# This script is designed for usage in QGIS


# inputs from QGIS
##Raumanalysen_ChristianMueller=group
##Exceltabelle_Adressen=table
##Strasse_Spalte=field Exceltabelle_Adressen
##Hausnummer_Spalte=field Exceltabelle_Adressen
##Hausnummerzusatz_Spalte=field Exceltabelle_Adressen
##PLZ_Spalte=field Exceltabelle_Adressen
##Ort_Spalte=field Exceltabelle_Adressen
##Ausgabeshapefile_Geocodierte_Adressen=output vector
##Ausgabetabelle_fehlende_Adressen=output table


# inputs for debugging
# addressFile = "C:/Raumanalysen_ChristianMueller/Projekte/Bergheim_Sozialmonitoring/Arbeitsdaten/Schuladressen.xlsx"
# streetCol = "Adresse"
# houseNrCol = "Hausnummer"
# houseNrApCol = "Zusatz"
# zipCol = "PLZ"
# placeCol = "Ort"
# adminAddDirectory = ""
# outShp = "C:/Raumanalysen_ChristianMueller/Projekte/Bergheim_Sozialmonitoring/Arbeitsdaten/Schuladressen.shp"
# missingAddsOut = "C:/Raumanalysen_ChristianMueller/Projekte/Bergheim_Sozialmonitoring/Arbeitsdaten/fehlende_Schuladressen.csv"

# rename variables
addressFile <- Exceltabelle_Adressen
streetCol <- Strasse_Spalte
houseNrCol <- Hausnummer_Spalte
houseNrApCol <- Hausnummerzusatz_Spalte
zipCol <- PLZ_Spalte
placeCol <- Ort_Spalte
adminAddDirectory <- ""
outShp <- Ausgabeshapefile_Geocodierte_Adressen
missingAddsOut <- Ausgabetabelle_fehlende_Adressen


# install/load libraries
for (i in 1:2){
  if (!require("readxl")) install.packages("readxl", dependencies = T)
  if (!require("dplyr")) install.packages("dplyr", dependencies = T)
  if (!require("jsonlite")) install.packages("jsonlite", dependencies = T)
  if (!require("ggmap")) install.packages("ggmap", dependencies = T)
  if (!require("sp")) install.packages("sp", dependencies = T)
  if (!require("rgdal")) install.packages("rgdal", dependencies = T)
  if (!require("tcltk")) install.packages("tcltk", dependencies = T)
  if (!require("tcltk2")) install.packages("tcltk2", dependencies = T)
  if (!require("geonames")) install.packages("geonames", dependencies = T)
}

# define whether or not google should be used for geocoding
# google only allows this geocoding, if the data is shown with a google basemap. Therefore it is set to "False" by default: https://developers.google.com/maps/documentation/geocoding/policies
useGoogle <- F


# create progress bar
try(tk <- tktoplevel(), silent = T)
try(tk2ico.setFromFile(win = tk, iconfile =  paste0(getwd(), "/Logo.ico")), silent = T)
try(font_text <- tkfont.create(family = "Ebrima", size = 12, weight = "bold"), silent = T)
try(tktitle(tk) <- "Raumanalysen - Christian Mueller - AddressAllocator", silent = T)
try(tk_lab <- tk2label(tk), silent = T)
try(tk_lab <- tk2label(tk, font = font_text), silent = T)
try(tk_pb <- tk2progress(tk, length = 700), silent = T)
try(tkgrid(tk_lab, row = 0), silent = T)
try(tkgrid(tk_pb, row = 1), silent = T)
try(tkraise(tk), silent = T)
try(tk_center(tk), silent = T)




# load address data
try(tkconfigure(tk_lab, text = "Lade tabellarische Adressdaten..."), silent = T)
try(tkconfigure(tk_pb, value = 2, maximum = 100), silent = T)
sh_names <- excel_sheets(addressFile)
addtab <- as.data.frame(read_excel(addressFile, sh_names[1]))


# build address-ID-field for tabular address data
try(tkconfigure(tk_lab, text = "Erzeuge Adress-ID-Feld für tabellarische Adressdaten..."), silent = T)
try(tkconfigure(tk_pb, value = 3, maximum = 100), silent = T)
source("createAddressIDField.r")
addtab <- createAddressIDField(data = addtab, streetCol = streetCol,
                               houseNrCol = houseNrCol, houseNrApCol = houseNrApCol,
                               zipCol = zipCol, placeCol = placeCol)


# download and unzip administrative addressess and zip codes when not provided as file (only functional for Germany - Northrhine-Westphalia!)
try(tkconfigure(tk_lab, text = "Lade externe Daten..."), silent = T)
try(tkconfigure(tk_pb, value = 5, maximum = 100), silent = T)
if (adminAddDirectory == ""){
  
  try(tkconfigure(tk_lab, text = "Lade amtliche Adressdaten herunter..."), silent = T)
  try(tkconfigure(tk_pb, value = 6, maximum = 100), silent = T)
  download.file("https://www.opengeodata.nrw.de/produkte/geobasis/lika/alkis_sek/gebref/gebref_EPSG4647_ASCII.zip",
                paste0(tempdir(), "/adminAddZip.zip"))
  
  try(tkconfigure(tk_lab, text = "Entpacke amtliche Adressdaten..."), silent = T)
  try(tkconfigure(tk_pb, value = 8, maximum = 100), silent = T)
  unzip(paste0(tempdir(), "/adminAddZip.zip"), exdir = tempdir())
  
  adminAddDirectory <- tempdir()
  
  try(tkconfigure(tk_lab, text = "Lade PLZ-Zuordnungen herunter..."), silent = T)
  try(tkconfigure(tk_pb, value = 9, maximum = 100), silent = T)
  download.file("https://www.suche-postleitzahl.org/download_files/public/zuordnung_plz_ort.csv",
                paste0(tempdir(), "/zipCodes.csv"))
  
}


# load administrative address data
try(tkconfigure(tk_lab, text = "Lade amtliche Adressdaten (kann ein wenig dauern)..."), silent = T)
try(tkconfigure(tk_pb, value = 12, maximum = 100), silent = T)
adminAdd <- read.table(paste0(adminAddDirectory, "/gebref.txt"),
                       sep = ";", dec = ",", header = T, as.is = T, encoding = "UTF-8")


# load reference data for coded place names
try(tkconfigure(tk_lab, text = "Lade Schlüsseldatei für amtliche Adressdaten..."), silent = T)
try(tkconfigure(tk_pb, value = 15, maximum = 100), silent = T)
ref <- read.table(paste0(adminAddDirectory, "/gebref_schluessel.txt"),
                  sep = ";", encoding = "UTF-8", fill = T, as.is = T)

# load zip codes
try(tkconfigure(tk_lab, text = "Lade PLZ-Zuordnungen..."), silent = T)
try(tkconfigure(tk_pb, value = 18, maximum = 100), silent = T)
zip <- read.table(paste0(adminAddDirectory, "/zipCodes.csv"),
                  sep = ",", encoding = "UTF-8", as.is = T)


# filter administrative addresses for place names in the tabular data file
try(tkconfigure(tk_lab, text = "Filtere amtliche Adressdaten..."), silent = T)
try(tkconfigure(tk_pb, value = 20, maximum = 100), silent = T)
source("filterAdminAdresses_addInfo.r")
try(adminAdd <- filterAdminAdresses_addInfo(toBeGeocodedPlaces = addtab[,placeCol],
                                adminAdd = adminAdd, referenceData = ref,
                                addPlaceNames = T, addZipCode = T, zipCodes = zip), silent = T)


# build address-ID-field for administrative address data
try(tkconfigure(tk_lab, text = "Erzeuge Adress-ID für amtliche Adressdaten..."), silent = T)
try(tkconfigure(tk_pb, value = 22, maximum = 100), silent = T)
try(adminAdd <- createAddressIDField(data = adminAdd, streetCol = "strassenname",
                                 houseNrCol = "hsnr", houseNrApCol = "hsnrzusatz",
                                 zipCol = "zipCode", placeCol = "placeName"), silent = T)

# unify address-ID-fields
try(tkconfigure(tk_lab, text = "Vereinheitliche Adress-ID-Felder..."), silent = T)
try(tkconfigure(tk_pb, value = 28, maximum = 100), silent = T)
source("unifyAddressIDField.r")
try(addtab <- unifyAddressIDField(data = addtab, addressIDField = "addressID"), silent = T)
try(adminAdd <- unifyAddressIDField(data = adminAdd, addressIDField = "addressID"), silent = T)



# join data sets
try(tkconfigure(tk_lab, text = "Verknüpfe Datensätze..."), silent = T)
try(tkconfigure(tk_pb, value = 29, maximum = 100), silent = T)
try(addtab <- left_join(x = addtab, y = adminAdd, by = "addressID"), silent = T)


# remove redundant columns
remCols <- colnames(adminAdd)
remCols <- remCols[-which(remCols == "rechtswert")]
remCols <- remCols[-which(remCols == "hochwert")]
remCols <- remCols[-which(remCols == "addressID")]
if (length(remCols) > 0) addtab <- addtab[,-which(colnames(addtab) %in% remCols)]


# create columns for x- and y-coordinates and data source information
addtab <- cbind(addtab, xcoords = NA, ycoords = NA,
                source = "", stringsAsFactors = F)
try(addtab[,"xcoords"] <- addtab[,"rechtswert"], silent = T)
try(addtab[,"ycoords"] <- addtab[,"hochwert"], silent = T)
geocodedPos <- which(!is.na(addtab[,"xcoords"]))
if (length(geocodedPos) > 0) addtab[geocodedPos,"source"] <- "Amtliche_Adresse"



# extract addresses which were not geocoded
misPos <- which(is.na(addtab[,"xcoords"]))
if (length(misPos) > 0){
  mis <- addtab[misPos,]
  
  
  # geocode missing addresses via Nominatim/OSM
  try(tkconfigure(tk_lab, text = "Geocodiere fehlende Adressen über OSM..."), silent = T)
  source("geocodeOSM.r")
  osmAdds <- geocodeOSM(data = mis, street = streetCol, houseNumber = houseNrCol,
                        houseNumberAppendix = houseNrApCol, zipCode = zipCol, placeName = placeCol)
  pos <- which(osmAdds[,"xcoords"] == "")
  if (length(pos) > 0) {
    osmAdds[pos, "xcoords"] <- NA
    osmAdds[pos, "ycoords"] <- NA
  }
  
  
  # add new coordinates address data table
  source("addAddressLocationsToDataTable.r")
  addtab <- addAddressLocationsToDataTable(existingData = addtab, newData = osmAdds,
                                           newDataSourceName = "OpenStreetMap")
  
  
  # extract addresses which were not geocoded
  misPos2 <- which(is.na(addtab[,"xcoords"]))
  if (length(misPos2) > 0){
    mis2 <- addtab[misPos2,]
    
    # google only allows this geocoding, if the data is shown with a google basemap: https://developers.google.com/maps/documentation/geocoding/policies
    if (useGoogle){
      
      try(tkconfigure(tk_lab, text = "Geocodiere fehlende Adressen über Google..."), silent = T)
      source("geocodeGoogle.r")
      googleAdds <- geocodeGoogle(data = mis2, street = streetCol, houseNumber = houseNrCol,
                                  houseNumberAppendix = houseNrApCol, zipCode = zipCol, placeName = placeCol)
      pos <- which(googleAdds[,"xcoords"] == "")
      if (length(pos) > 0) {
        googleAdds[pos, "xcoords"] <- NA
        googleAdds[pos, "ycoords"] <- NA
      }
      
      
      # add new coordinates address data table
      source("addAddressLocationsToDataTable.r")
      addtab <- addAddressLocationsToDataTable(existingData = addtab, newData = googleAdds,
                                               newDataSourceName = "Google")
      
      
      
      # extract addresses which were not geocoded
      misPos3 <- which(is.na(addtab[,"xcoords"]))
      if (length(misPos3) > 0){
        mis3 <- addtab[misPos3,]
      } else {
        mis3 <- mis2[0,]
      }
      
    } else {
      
      mis3 <- mis2
      
    }
  } else {
    
    mis3 <- mis
    
  }
  
  # write missing addresses to file
  write.table(x = mis3[,1:(ncol(mis3)-6)], file = missingAddsOut, sep = ";", dec = ".",
              col.names = T, row.names = F)
  
}

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
  # sp_addsOsm <- SpatialPointsDataFrame(coords = cbind(as.numeric(addtab[sPos, "xcoords"]),
  #                                                       as.numeric(addtab[sPos, "ycoords"])),
  #                                        data = addtab[sPos,], proj4string =  CRS("+init=epsg:3857"))
  sp_addsOsm <- SpatialPointsDataFrame(coords = cbind(as.numeric(addtab[sPos, "xcoords"]),
                                                        as.numeric(addtab[sPos, "ycoords"])),
                                         data = addtab[sPos,], proj4string =  CRS("+init=epsg:4326"))
  
  # transform data
  sp_addsOsm <- spTransform(sp_addsOsm, CRS("+init=epsg:25832"))
  if ("sp_out" %in% ls()){
    sp_out <- rbind(sp_out, sp_addsOsm)
  } else {
    sp_out <- sp_addsOsm
  }
  
}


# build spatial object out of addresses geocoded by google
sPos <- which(addtab[,"source"] == "Google")
if (length(sPos) > 0){
  sp_addsGoogle <- SpatialPointsDataFrame(coords = cbind(as.numeric(addtab[sPos, "xcoords"]),
                                                      as.numeric(addtab[sPos, "ycoords"])),
                                       data = addtab[sPos,], proj4string =  CRS("+init=epsg:4326"))

  # transform data
  sp_addsGoogle <- spTransform(sp_addsGoogle, CRS("+init=epsg:25832"))
  if ("sp_out" %in% ls()){
    sp_out <- rbind(sp_out, sp_addsGoogle)
  } else {
    sp_out <- sp_addsGoogle
  }
  

}


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
Ausgabeshapefile_Geocodierte_Adressen = sp_out

# # test output outside QGIS environment
# writeOGR(sp_out, dsn = dirname(outShp), layer = "AddressAllocator_output",
#          driver = "ESRI Shapefile", overwrite_layer=TRUE)

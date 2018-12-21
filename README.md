# AddressAllocator
The AddressAllocator geocodes addresses in a three step procedure:
1. Matching with official administrative building references (tested for Germany - Northrhine-Westfalia)
2. Geocoding by Nominatim/OSM API
3. Geocoding by Google API (outcommented as Google restricts the usage of this data: It is only allowed for use in combination with a Google basemap: https://developers.google.com/maps/documentation/geocoding/policies)

This script is designed for usage in QGIS
Run allocateAddresses.r to run the AddressAllocator

This project was created under QGIS version < 3.0 and is not further pursued. Please visit https://github.com/RaumanalysenChristianMueller/AddressAllocator_py for a script which performs a similar task and can be executed under QGIS with version numbers > 3.0.

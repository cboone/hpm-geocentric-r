library("geonames")   # The geonames package, which queries the Geonames database.

coordinates.from.place.name <- function(place.name) {
  require("geonames")
  
  gn.data <- suppressWarnings(GNsearch(q = place.name, maxRows = 1))
  
  if (length(gn.data) == 0) {
    stop(paste("Sorry, but Geonames didn't return any results for \"", place.name, "\".", sep = ""))
  } else {
    c(gn.data$lng, gn.data$lat)
  }
}

place.name.from.coordinates <- function(coordinates, radius = 10) {
  require("geonames")
  
  # For some reasons, Geonames seems to ignore the maxRows parameter here. So, force it.
  # 
  gn.data <- suppressWarnings(GNfindNearbyPlaceName(lat = coordinates[2], lng = coordinates[1], radius = radius, maxRows = 1))[1,]
  
  if (length(gn.data) == 0) {
    stop(paste("Sorry, but Geonames didn't return any results for latitude ", coordinates[2], ", longitude ", coordinates[1], ".", sep = ""))
  } else {
    name <- gn.data$name
  }
}

time.zone.offset.from.coordinates <- function(coordinates) {
  require("geonames")
  
  time.zone <- suppressWarnings(GNtimezone(lat = coordinates[2], lng = coordinates[1]))
  time.zone$gmtOffset
}

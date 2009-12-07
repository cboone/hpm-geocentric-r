library("maptools")   # The maptools package, which generates the sunrise, solar noon, and sunset data.

earliest.sun.times <- function(times, year, time.zone.offset) {
  index <- which.min(times)
  time <- times[index] + (time.zone.offset / 24)
  date <- paste(year, index, sep = "-")
  date <- strptime(date, format = "%Y-%j") + (time * 60 * 60 * 24)
  
  list(index = index, time = time, date = date)
}

latest.sun.times <- function(times, year, time.zone.offset) {
  index <- which.max(times)
  time <- times[index] + (time.zone.offset / 24)
  date <- paste(year, index, sep = "-")
  date <- strptime(date, format = "%Y-%j") + (time * 60 * 60 * 24)
  
  list(index = index, time = time, date = date)
}

sun.data <- function(coordinates, year, time.zone.offset) {
  require("maptools")
  
  days <- utc.year(year)
  
  sunrises <- sunriset(coordinates, days, proj4string = CRS("+proj=longlat +datum=WGS84"), direction = "sunrise")
  solar.noons <- solarnoon(coordinates, days, proj4string = CRS("+proj=longlat +datum=WGS84"))
  sunsets <- sunriset(coordinates, days, proj4string = CRS("+proj=longlat +datum=WGS84"), direction = "sunset")
  times <- matrix(c(sunrises, solar.noons, sunsets), nrow = 3, byrow = TRUE) + (time.zone.offset / 24)
  
  earliest.sunrise <- earliest.sun.times(sunrises, year, time.zone.offset)
  latest.sunrise <- latest.sun.times(sunrises, year, time.zone.offset)
  
  earliest.solar.noon <- earliest.sun.times(solar.noons, year, time.zone.offset)
  latest.solar.noon <- latest.sun.times(solar.noons, year, time.zone.offset)
  
  earliest.sunset <- earliest.sun.times(sunsets, year, time.zone.offset)
  latest.sunset <- latest.sun.times(sunsets, year, time.zone.offset)
  
  list(times = times,
       sunrise = list(earliest = earliest.sunrise, latest = latest.sunrise),
       solar.noon = list(earliest = earliest.solar.noon, latest = latest.solar.noon),
       sunset = list(earliest = earliest.sunset, latest = latest.sunset))
}

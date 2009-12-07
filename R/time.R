library("chron")      # The chron package, which knows about leap years.

utc.year <- function(year, POSIXct.out = TRUE) {
  require("chron")
  
  if (leap.year(year)) {
    days.of.year <- seq(1, 366)
  } else {
    days.of.year <- seq(1, 365)
  }
  
  days.of.year <- paste(year, days.of.year, sep = "-")
  days.of.year <- strptime(days.of.year, format = "%Y-%j", tz = "UTC")
  
  if (POSIXct.out) {
    as.POSIXct(days.of.year)
  } else {
    days.of.year
  }
}

convert.day.to.radians <- function(day) {
  (day / 365) * (2 * pi)
}

adjust.for.time.zone.offset <- function(time, time.zone.offset) {
  time + (time.zone.offset / 24)
}

library("chron")

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

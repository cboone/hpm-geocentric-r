winter.season <- function(sunsets, sunrise.data, sunset.data) {
  if ((sunrise.data$latest$index - sunset.data$earliest$index) < 0) {
    # 
    # If the latest sunrise is before the earliest sunset, then we need to calculate winter as 2 parts.
    # 
    start.of.winter <- sunset.data$earliest$index
    end.of.winter <- sunrise.data$latest$index
    
    december.winter.times <- sunsets[start.of.winter:365]
    january.winter.times <- sunsets[1:end.of.winter]
    rest.of.the.year <- rep(0, 365 - length(january.winter.times) - length(december.winter.times))
    
    c(january.winter.times, rest.of.the.year, december.winter.times)
  } else {
    # 
    # Otherwise, it only has 1 part.
    # 
    start.of.winter <- min(sunset.data$earliest$index, sunrise.data$latest$index)
    end.of.winter <- max(sunset.data$earliest$index, sunrise.data$latest$index)
    
    winter.times <- sunsets[start.of.winter:end.of.winter]
    before.winter <- rep(0, (start.of.winter - 1))
    after.winter <- rep(0, 365 - end.of.winter)
    
    c(before.winter, winter.times, after.winter)
  }
}

summer.season <- function(sunsets, sunrise.times, sunset.times) {
  if ((sunset.times$latest$index - sunrise.times$earliest$index) < 0) {
    # 
    # If the latest sunset is before the earliest sunrise, then we need to calculate summer as 2 parts.
    # 
    start.of.december.summer <- sunrise.times$earliest$index
    end.of.december.summer <- 365
    start.of.january.summer <- 1
    end.of.january.summer <- sunset.times$latest$index
    
    december.summer.times <- sunsets[start.of.december.summer:end.of.december.summer]
    january.summer.times <- sunsets[start.of.january.summer:end.of.january.summer]
    rest.of.the.year <- rep(0, 365 - length(january.summer.times) - length(december.summer.times))
    
    c(january.summer.times, rest.of.the.year, december.summer.times)
  } else {
    # 
    # Otherwise, it only has 1 part.
    # 
    start.of.summer <- min(sunset.times$latest$index, sunrise.times$earliest$index)
    end.of.summer <- max(sunset.times$latest$index, sunrise.times$earliest$index)

    summer.times <- sunsets[start.of.summer:end.of.summer]
    before.summer <- rep(0, (start.of.summer - 1))
    after.summer <- rep(0, 365 - end.of.summer)
    
    c(before.summer, summer.times, after.summer)
  }
}

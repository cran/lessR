# create an R time series programmatically
.tsMake <- function (x.date, y) {

  intervals <- diff(x.date)

  # Determine the frequency based on intervals of adjacent dates
  if (all(head(intervals == 1, -1))) {
    frequency <- 365  # Daily data
  } else if (all(head(intervals >= 28 & intervals <= 31, -1))) {
    frequency <- 12  # Monthly data
  } else if (all(head(intervals == 7, -1))) {
    frequency <- 52  # Weekly data
  } else if (all(head(intervals >= 80 & intervals <= 100, -1))) {
    frequency <- 4  # Quarterly data
  } else if (all(head(intervals >= 365, -1))) {
    frequency <- 1  # Yearly data
  } else
    stop("Only daily, monthly, weekly, quarterly, or yearly frequencies")

  # Convert the start date to appropriate start values for ts()
  start_date <- as.Date(min(x.date))
  if (frequency == 365) {
    start_year <- as.numeric(format(start_date, "%Y"))
    start_day <- as.numeric(format(start_date, "%j"))
    ts_start <- c(start_year, start_day)
  } else if (frequency == 12) {
    start_year <- as.numeric(format(start_date, "%Y"))
    start_month <- as.numeric(format(start_date, "%m"))
    ts_start <- c(start_year, start_month)
  } else if (frequency == 4) {
    start_year <- as.numeric(format(start_date, "%Y"))
    start_quarter <- (as.numeric(format(start_date, "%m")) - 1) %/% 3 + 1
    ts_start <- c(start_year, start_quarter)
  } else if (frequency == 1) {
    start_year <- as.numeric(format(start_date, "%Y"))
    ts_start <- c(start_year, 1)
  }

  # Create the time series object of y
  ts.y <- ts(y, start=ts_start, frequency=frequency)
  return(ts.y)
}

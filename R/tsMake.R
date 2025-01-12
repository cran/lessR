# create an R time series programmatically
.tsMake <- function (x.date, y, ts_unit) {

  intervals <- diff(x.date)

  # Determine the frequency based on intervals of adjacent dates
  # the head(..., n=-1) includes all but the last data value
  if (all(head(intervals == 1, -1))) {
    if (ts_unit == "days7")
      frequency <- 7  # Daily data across the week
    else
      frequency <- 365  # Daily data across the year
  } else if (all(head(intervals >= 28 & intervals <= 31, -1))) {
    frequency <- 12  # Monthly data
  } else if (all(head(intervals == 7, -1))) {
    frequency <- 52  # Weekly data
  } else if (all(head(intervals >= 80 & intervals <= 100, -1))) {
    frequency <- 4  # Quarterly data
  } else if (all(head(intervals >= 365, -1))) {
    frequency <- 1  # Yearly data
  } else {
    tbl <- table(intervals)
    tbl.df <- data.frame(Interval=as.numeric(names(tbl)), Counts=as.vector(tbl))
    message("\nData are not spaced at regular date intervals.\n")
    print(tbl.df)
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Forecasting here requires dates spaced at regular intervals:\n",
      "daily, monthly, weekly, quarterly, or yearly data.\n\n",
      "Perhaps aggregate the data at a larger time interval with\n",
      "parameter  ts_unit  such as ts_unit=\"quarters\".\n\n")
  }

  # Convert the start date to appropriate start values for ts()
  start_date <- as.Date(min(x.date))
  if (frequency %in% c(7, 365)) {
    start_year <- as.numeric(format(start_date, "%Y"))
    start_day <- as.numeric(format(start_date, "%j"))
    ts_start <- c(start_year, start_day)
  } else if (frequency %in% c(52, 12)) {
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
  if (is.data.frame(y))
    y.ts <- ts(y[,1], start=ts_start, frequency=frequency)
  else
    y.ts <- ts(y, start=ts_start, frequency=frequency)
  return(y.ts)
}

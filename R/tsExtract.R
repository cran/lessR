# extract dates and y-values from a ts object
.tsExtract <- function(y.ts, x.name=NULL) {

  # function to convert output of time() to as.Date()
  frac.yr_to.date <- function(date.num, freq) {
    year <- floor(date.num)  # extract the year
    frac <- date.num - year  # fractional part

    leap.yr <-  # divisible by 4 but not 100, or divisible by 400 
      (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
    days.in.yr <- ifelse(leap.yr, 366, 365)
    
    if (freq %in% c(7, 365, 366)) {  # Daily data
      day.of.yr <- round(frac * days.in.yr) + 1
      return(as.Date(day.of.yr - 1, origin = paste0(year, "-01-01")))
    }
    if (freq == 52) {  # Weekly data
      day.of.yr <- round(frac * days.in.yr / 7) * 7 + 1
      return(as.Date(day.of.yr - 1, origin = paste0(year, "-01-01")))
    }
    if (freq == 12) {  # Monthly data, ts() does not account for uneven months
      month <- round(frac * 12) + 1
      if (month > 12) {
        month <- 1
        year <- year + 1
      }
      return(as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
    }
    if (freq == 4) {  # Quarterly data
      quarter <- floor(frac * 4) + 1
      month <- c(1, 4, 7, 10)[quarter]
      return(as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
    }
    if (freq == 1) {  # Yearly data
      return(as.Date(paste0(year, "-01-01")))
    }

    cat("\n"); stop(call.=FALSE, "\n------\n",
      "Unsupported frequency. Use one of the following frequencies:\n",
      "365(daily), 52(weekly), 12(monthly), 4(quarterly), or 1(yearly).\n\n")
  }  # end frac.yr_to.date


  # x: assign ts date values based on function frac.yr_to.date()
  freq <- frequency(y.ts)
  date.num <- as.numeric(time(y.ts))  # time() creates vector of ts times
  x.dates <- lapply(date.num, frac.yr_to.date, freq)
  x.dates <- as.Date(unlist(x.dates))  # simplify back to a vector of Dates
  x.dates <- data.frame(x.dates)


  # y: assign ts values, one column for each ts column
  # different data structures for univariate ts and multivariate ts
  nc <- ifelse (is.matrix(y.ts), ncol(y.ts), 1)  # number of columns of x
  if (nc == 1) { # single column ts
    y <- as.numeric(y.ts)
  }
  else {  # multivariate ts, build y column-by-column
    y <- as.numeric(y.ts[,1])
    for (i in 2:nc) y <- cbind(y, as.numeric(y.ts[,i]))
  }

  # name the column(s) of y
  y <- data.frame(y)
  if (nc == 1) {
    y.name <- getOption("yname")
    names(y) <- y.name
    options(yname = y.name)
  }
  else {
    names(y) <- colnames(y.ts)
    y.name <- getOption("yname")
  }

  return(list(x.dates=x.dates, y=y, y.name=y.name))
}


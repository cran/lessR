# extract dates and y-values from a ts object
.tsExtract <- function(y.ts, x.name=NULL) {

  # get x.dates data frame, later to be set to x.call
  date_num <- as.numeric(time(y.ts))  # time() creates vector of ts times
  year <- floor(date_num)
  year_beg <- as.POSIXlt.character(paste0(year, "-01-01 01:01:01"))
  year_end <- as.POSIXlt.character(paste0(year+1, "-01-01 01:01:01"))
  diff.yr <- year_end - year_beg
  dates <- year_beg + ((date_num %% 1) * diff.yr)
  dates <- as.Date(format(dates, format="%Y-%m-%d")) # from POSIX to Date
  x.dates <- data.frame(dates)  # dates to be on x-axis
  names(x.dates) <- "date"

  # y: assign ts values, one column for each ts column
  # different data structures for univariate ts and multivariate ts
  nc <- ifelse (is.matrix(y.ts), ncol(y.ts), 1)  # number of columns of x
  if (nc == 1) { # single column ts
    y <- y.ts
  }
  else {  # multivariate ts, build y column-by-column
    y <- y.ts[,1]
    for (i in 2:nc) y <- cbind(y, y.ts[,i])
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


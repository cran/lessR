Rescale <-
function(x, data=d, kind="z", digits_d=3) {

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata 
    mydata.ok <- TRUE
  }

  # get variable name before potential call of data$x
  x.name <- deparse(substitute(x))  # could be a vars list
  options(xname = x.name)

  data.null <- ifelse (is.null(data), TRUE, FALSE) 

  # let deprecated mydata work as default
  dfs <- .getdfs() 
  mydata.ok <- FALSE
  if ("mydata" %in% dfs  &&  !("d" %in% dfs)) {
    d <- mydata
    df.name <- "mydata"
    mydata.ok <- TRUE
    options(dname = df.name)
  }
  if (!mydata.ok) {
    df.name <- deparse(substitute(data))  # get name of data table
    options(dname = df.name)
  }
  
  x.in.df <- FALSE
  if (!data.null)
    if (exists(df.name, where=.GlobalEnv, inherits=FALSE))
      if (exists(x.name, where=data)) x.in.df <- TRUE
 
  # if a tibble convert to data frame
  if (df.name %in% ls(name=.GlobalEnv)) {  # tibble to df
   if (any(grepl("tbl", class(data), fixed=TRUE))) {
      data <- data.frame(data)
   }
  }


  # x not in global env, in df, specify data= forces to data frame
  if ((!exists(x.name, where=.GlobalEnv) && !data.null) || x.in.df) {
    .nodf(df.name)  # check to see if data frame container exists     
    .xcheck(x.name, df.name, names(data))  # var in df?, vars lists not checked
    all.vars <- as.list(seq_along(data))  # even if only a single var
    names(all.vars) <- names(data)  # all data in data frame
    ind <- eval(substitute(x), envir=all.vars)  # col num selected vars
    if (!("list" %in% class(data))) {
      x.call <- data[, ind]
      if (length(ind) == 1) {  # x is 1 var
        data <- data.frame(data, stringsAsFactors=TRUE)
        names(data) <- x.name
       }
    }
  }
  else  # x a vector, not in a data frame
    x.call <- x


  if (kind == "z")  {
    x.call <- round(scale(x.call), digits_d)
  }
  
  else if (kind == "0to1") {
    mn.x <- min(x.call, na.rm=TRUE)
    mx.x <- max(x.call, na.rm=TRUE)
    x.call <- round((x.call - mn.x) / (mx.x - mn.x), digits_d)
  }

  else if (kind == "robust") {
    md.x <- median(x.call, na.rm=TRUE)
    IQR.x <- IQR(x.call, na.rm=TRUE)
    x.call <- round((x.call - md.x) / IQR.x, digits_d)
  }

  return(as.vector(x.call))

}

details <-
function(data=mydata, n.mcut=1, miss.zero=FALSE, max.lines=30,
         miss.show=30, miss.matrix=FALSE, brief=getOption("brief")) {

  dname <- deparse(substitute(data))  # from read is called data

  # feedback regarding data
  n.var <- ncol(data)
  n.obs <- nrow(data)
  max.lines <- min(max.lines, nrow(data))  #  max.lines now actual lines

  nu <- integer(length(n.var))

  ord.fac <- FALSE
  for (i in 1:n.var) if (class(data[,i])[1] == "ordered") ord.fac <- TRUE


  if (!brief) {
    cat("\n")
    cat("Basics\n")
    .dash(47)
    cat("Number of Variables in the data frame:    ", n.var, "\n", sep="")
    cat("Number of Rows of Data in the data frame: ", n.obs, "\n", sep="")
    .dash(47)

    cat("\n\n")
    cat("Row Names\n")
    .dash(67)
    cat("First two row names:", row.names(data)[1],"   ", 
        row.names(data)[2], "\n") 
    cat("Last two row names: ", row.names(data)[n.obs-1],
        "   ", row.names(data)[n.obs], "\n") 
    .dash(67)
  }

  cat("\n")
  if (!brief) cat("\n")
  cat("Variable Names and Data Types\n")
  if (!brief) {
    .dash(70)
    cat("factor: Non-numeric categories, initially read as unordered categories\n")
    if (ord.fac)
      cat("ordfact: Ordered, non-numeric categories\n")
    cat("integer: The data values are numeric but integers only\n")
    cat("numeric: The data values are numeric with decimal digits\n")
  }

  .dash(70)
  max.char <- 0
  for (i in 1:n.var) {
    x.name <- names(data)[i]
    if (nchar(x.name) > max.char) max.char <- nchar(x.name)
  }
  pad <- ifelse (max.char > 7, max.char + 2, max.char + (10-max.char))
  if (!brief) cat("\n")
  cat(.fmtc(" ",pad+16), .fmtc("Missing",6), .fmtc("Unique",7), "\n")
  cat(.fmtc(" Variable",max.char+1), .fmtc("Type",8), .fmtc("Values",7),
      .fmtc("Values",7), .fmtc("Values",7), "  First and last values\n")
  .dash(88)

  colm.ID <- 0
  maybe.ID <- NULL
  for (i in 1:n.var) {

    x.name <- names(data)[i]

    nu[i] <- length(unique(na.omit(data[,i])))

    n <- sum(!is.na(data[,i]))
    n.miss <- sum(is.na(data[,i]))

    the.class <- class(data[,i])[1]  # could be an ordered factor
    if (the.class == "ordered") the.class <- "ordfact"

    cat(.fmtc(x.name,pad-1), .fmtc(the.class,9), .fmti(n,6),
        .fmti(n.miss,7), .fmti(nu[i],7))

    n1 <- as.character(data[1,i])  # the as.character is for cat of factors 
    n2 <- as.character(data[2,i]) 
    n3 <- as.character(data[3,i])
    e3 <- as.character(data[n.obs-2,i])
    e2 <- as.character(data[n.obs-1,i])
    e1 <- as.character(data[n.obs,i])
    bn2 <- "  ";  bn3 <- "  ";  be2 <- "  ";  be3 <- "  "
    tot.char <- nchar(paste(n1,n2,n3,e3,e2,e1))
    if (tot.char > 34) {
      n3 <- "";  e3 <- ""; bn3 <- ""; be3 <- "";
    } 
    if (tot.char > 58) {
      n2 <- ""; e2 <- ""; bn2 <- ""; be2 <- ""
    } 
    cat("   ", n1, bn2, n2, bn3, n3, " ... ", e3, be3,  e2, be2,  e1, sep="")
    cat("\n") 

    if (nu[i]==n  &&  (is.factor(data[,i]))) {
      maybe.ID <- x.name
      colm.ID <- i
    }
  }
  .dash(88)

  if (!brief) {
    if (colm.ID > 0) {
      cat("\n\n")
      cat("For the following 'variable', each row of data is unique. Perhaps\n",
          "these values specify a unique ID for each row. To implement, re-read\n",
          "with the following setting added to your Read statement: row.names=", 
          toString(colm.ID), sep="", "\n")
      .dash(70)
      cat(maybe.ID, "\n")
      .dash(70)
    }

    n.cat <- getOption("n.cat")
    int.cat <- FALSE
    n.cat.temp  <- 4
    for (j in 1:n.var) {
      if (is.numeric(data[,j]) && nu[j] <= n.cat.temp) int.cat <- TRUE
    }
    if (int.cat) {
      cat("\n\n")
      cat("Each of these variables is numeric, but has less than or equal\n", 
         n.cat.temp, " unique values. If these variables are categorical consider to\n",
         "transform each variable into a factor with the Transform and\n",
         "factor functions. To see examples enter:  > ?Transform\n", 
         "Or, specify a value for n.cat, ",
         "such as:  > set(n.cat=4)\n", sep="")
      .dash(63)
      for (j in 1:n.var) if (is.numeric(data[,j]) && nu[j] <= n.cat.temp)
        cat(names(data)[j], "\n")
      .dash(63)
    }
    
    cat("\n\n")
    cat("Missing Data Analysis\n")
    .dash(52)

    n.miss.tot <- sum(is.na(data))
    
    if (n.miss.tot > 0) {
      
      cat("n.miss ", "Observation\n")
      n.lines <- 0
      i <- 0
      while ( i<n.obs && n.lines>=0 ) {
        i <- i + 1
        n.miss <- sum(is.na((data)[i, ]))
        if ( (n.miss >= n.mcut  || miss.zero) && (n.lines < miss.show) ) {
          n.lines <- n.lines + 1
          cat(n.miss, "     ", row.names((data)[i, ]), "\n")
        }
        if (n.lines == miss.show) {
          n.lines <- -1
          cat("\nToo many data rows have at least this many missing values: ", n.mcut, "\n",
            "No more lines displayed to conserve space.\n",
            "Specify n.mcut=2 to see just those lines with 2 missing values, etc,",
            "or use a higher number.\n",
            "Or increase the default value of miss.show=30 to show more lines.\n")
        }
      }
      cat("\n")

      cat("Total number of cells in data table: ", n.var*n.obs,"\n")
      cat("Total number of cells with the value missing: ", n.miss.tot,"\n")
      .dash(52)
      cat("\n")

      if (miss.matrix && n.miss.tot>0) {
        cat("\n\nTable of Missing Values, 1 means missing\nn")
        print(matrix(as.numeric(is.na(data)), nrow=n.obs, ncol=n.var,
        dimnames = list(row.names(data), as.character(1:n.var)) ))
      }
          
    }

    else cat("No missing data\n\n")
  }


  # feedback regarding variable labels
  if (brief) cat("\n")
  mylabels <- attr(data, which="variable.labels")
  myunits <- attr(data, which="variable.units")
  cat("\nVariable Names   Variable Labels\n")

  if (!is.null(mylabels)) {
    n.rows <- length(mylabels)
    n.lines <- min(max.lines, n.rows)

    mx.nm <- 0
    for (i in 1:n.lines)  # var names
      if (nchar(names(mylabels)[i]) > mx.nm) mx.nm <- nchar(names(mylabels)[i])
    width.nm <- mx.nm + 1

    mx.ln <- 0
    for (i in 1:n.lines) {  # get width of largest line
      nc <- nchar(as.character(mylabels[i]))
      if (nc > mx.ln) mx.ln <- mx.nm + nc
    }
    width.ln <- max(mx.ln, nchar("Variable Names  Variable Labels"))
    width.ln <- min(mx.ln+3, 80)

    .dash(width.ln)
    for (i in 1:n.lines) {
      blanks <- paste(rep(" ", width.nm-nchar(names(mylabels)[i])), collapse = "")
      if (is.na(mylabels[i])) mylabels[i] <- ""
      cat(names(mylabels)[i], blanks, mylabels[i], "\n")
    }
    .dash(width.ln)

    if (n.rows > n.lines) {
      cat("To see all the variable labels set mx.lines to", n.rows, "\n")
      .dash(64)
      cat("\n")
    }
  }
  else {
    .dash(64)
    cat("None\n")
  }

  # feedback regarding variable units
  cat("\n\nVariable Names   Variable Units\n")

  if (!is.null(myunits)) {
    n.units <- length(myunits)
    n.lines <- min(max.lines, n.units)

    for (i in 1:n.lines)  # get width of largest line
      width.ln <- width.nm + nchar(as.character(myunits[i]))
    width.ln <- max(width.ln, 1+nchar("Variable Names  Variable Units"))

    .dash(width.ln)
    for (i in 1:n.lines) {
      blanks <- paste(rep(" ", 15-nchar(names(myunits)[i])), collapse = "")
      if (is.na(myunits[i])) myunits[i] <- ""
      cat(names(myunits)[i], blanks, myunits[i], "\n")
    }
    .dash(width.ln)
  }
  else {
    .dash(64)
    cat("None\n")
  }

  cat("\n")
}

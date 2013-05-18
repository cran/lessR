details <-
function(data=mydata, n.mcut=1, miss.zero=FALSE,
         max.lines=30, miss.show=30, miss.matrix=FALSE) {

  dname <- deparse(substitute(data))  # from read is called data

  # feedback regarding data
  n.var <- ncol(data)
  n.obs <- nrow(data)
  n.lines <- min(max.lines, ncol(data))

  nu <- integer(length(n.var))

  ord.fac <- FALSE
  for (i in 1:n.var) if (class(data[,i])[1] == "ordered") ord.fac <- TRUE
  
  cat("\n")
  cat("Basics\n")
  .dash(55)
  if (dname != "data") cat("Name of data frame: ", dname, "\n", sep="")
  cat("Number of Variables in the data frame:    ", n.var, "\n", sep="")
  cat("Number of Rows of Data in the data frame: ", n.obs, "\n", sep="")
  .dash(55)

  cat("\n\n")
  cat("Row Names\n")
  .dash(67)
  cat("First two row names:", row.names(data)[1],"   ", 
      row.names(data)[2], "\n") 
  cat("Last two row names: ", row.names(data)[n.obs-1],
      "   ", row.names(data)[n.obs], "\n") 
  .dash(67)

  cat("\n\n")
  cat("Variable Names and Types of the Values\n")
  .dash(67)
  cat("factor: Non-numeric categories, which, as read here, are unordered\n")
  if (ord.fac)
    cat("ordfact: Ordered, non-numeric categories\n")
  cat("integer: The values are numeric and integers\n")
  cat("numeric: The values are numeric with decimal digits\n")
  .dash(67)

  max.char <- 0
  for (i in 1:n.var) {
    x.name <- names(data)[i]
    if (nchar(x.name) > max.char) max.char <- nchar(x.name)
  }
  if (max.char > 7)
    pad <- max.char + 2
  else
    pad <- max.char + (10-max.char)
  cat("\n")
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

    cat(.fmtc(x.name,pad-1), .fmtc(the.class,8), .fmti(n,7),
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
       "factor functions. To see examples enter:  > ?trans\n", 
       "Or, specify a value for n.cat, ",
       "such as:  > set(n.cat=4)\n", sep="")
    .dash(83)
    for (j in 1:n.var) if (is.numeric(data[,j]) && nu[j] <= n.cat.temp)
      cat(names(data)[j], "\n")
    .dash(83)
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
          "Or increase the default value of miss.show=30 to show more lines.")
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


  # feedback regarding variable labels
  mylabels <- attr(data, which="variable.labels")
  cat("\nVariable Names    Variable Labels\n")

  if (!is.null(mylabels)) {
    mylabels <- data.frame(mylabels)
    names(mylabels) <- ""
    n.labels <- nrow(mylabels)
    n.lines <- min(max.lines, n.labels)
    max.chr <- 0
    for (i in 1:n.labels) {  # get width of largest variable label
      xlbl <- as.character(mylabels[i,])
      if (nchar(xlbl) > max.chr) max.chr <- nchar(xlbl)
    }
    .dash(max.chr+13)
    print(head(mylabels, n=n.lines))
    .dash(max.chr+13)
    if (n.labels > n.lines) {
      cat("To see all the variable labels set max.lines to", n.labels, "\n")
      .dash(64)
      cat("\n")
    }
  }
  else {
    .dash(64)
    cat("None\n")
  }

    cat("\n")

}

details <-
function(data=d, n_mcut=1, max_lines=30,
         miss_show=30, miss_matrix=FALSE, var_labels=FALSE,
         brief=getOption("brief")) {

  # get name of data table
  dname <- deparse(substitute(data))  # from read is called data

  # if a tibble convert to data frame
  dfs <- .getdfs()
  if (!is.null(dfs)) {
    if (dname %in% dfs) {  # tibble to df
      if (any(grepl("tbl", class(data), fixed=TRUE))) {
        data <- data.frame(data)
      }
    }
  }

  # feedback regarding data
  n.var <- ncol(data)
  n.obs <- nrow(data)
  max_lines <- min(max_lines, nrow(data))  #  max_lines now actual lines

  nu <- integer(length(n.var))

  if (!brief) {
    cat("\n")
    .dash(58)
    cat("Dimensions:", n.var, "variables over", n.obs, "rows of data")

    cat("\n\n")
    cat("First two row names:", row.names(data)[1],"   ", 
        row.names(data)[2], "\n") 
    cat("Last two row names: ", row.names(data)[n.obs-1],
        "   ", row.names(data)[n.obs], "\n") 
    .dash(58)
  }

    reg.fac <- FALSE
    for (i in 1:n.var)
      if (is.factor(data[,i])[1]  &&  !(is.ordered(data[,i])[1]))
        reg.fac <- TRUE

    ord.fac <- FALSE
    for (i in 1:n.var) if (is.ordered(data[,i])[1]) ord.fac <- TRUE

    chr.flg <- FALSE
    for (i in 1:n.var) if (is.character(data[,i])[1]) chr.flg <- TRUE

    int.flg <- FALSE
    for (i in 1:n.var) if (is.integer(data[,i])[1]) int.flg <- TRUE
    
    dte.flg <- FALSE
    for (i in 1:n.var) if (inherits(data[,i][1], "Date")) dte.flg <- TRUE

    num.flg <- FALSE
    for (i in 1:n.var) if (is.double(data[,i])[1]) num.flg <- TRUE


    cat("Data Types\n")
    .dash(60)
    if (reg.fac) cat("factor: Non-numeric categories, read as unordered",
        "categories\n")
    if (ord.fac) cat("ordfactor: Ordered, non-numeric categories\n")
    if (chr.flg) cat("character: Non-numeric data values\n")
    if (int.flg) cat("integer: Numeric data values, integers only\n")
    if (dte.flg) cat("Date: Date with year, month and day\n")
    if (num.flg) cat("double: Numeric data values with decimal digits\n")
    .dash(60)
  cat("\n")

  #if (any(is.na(d)))
    #cat("NA indicates a missing data value, Not Available\n")
  #cat("\n")

  max.char <- 0
  for (i in 1:n.var) {
    x.name <- names(data)[i]
    if (nchar(x.name) > max.char) max.char <- nchar(x.name)
  }
  pad <- ifelse (max.char > 7, max.char + 2, max.char + (10-max.char))
  pad2 <- ifelse (max.char > 8, max.char - 8, 0)
  if (!brief) cat("\n")
  cat("   ")  # account for variable number in first two columns
  cat(.fmtc(" Variable",9+pad2), .fmtc(" ",pad2+(16-pad2)),
      .fmtc("Missing",7), .fmtc("Unique",7), "\n")
  cat("   ")
  cat(.fmtc("     Name",max.char+1), .fmtc("Type",8), .fmtc("Values",7),
      .fmtc("Values",7), .fmtc("Values",7), "  First and last values\n")
  .dash(90)

  colm.ID <- 0
  maybe.ID <- NULL
  for (i in 1:n.var) {

    x.name <- names(data)[i]

    nu[i] <- length(unique(na.omit(data[,i])))

    n <- sum(!is.na(data[,i]))
    n.miss <- sum(is.na(data[,i]))

    the.class <- class(data[,i])[1]  # could be an ordered factor
    if ("ordered" %in% the.class) the.class <- "ordfactor"
    if ("numeric" %in% the.class) the.class <- "double"

    cat(.fmti(i,2), .fmtc(x.name,pad-1), .fmtc(the.class,9), .fmti(n,6),
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

    if (nu[i]==n  &&  ((is.factor(data[,i])) || (is.character(data[,i])))) {
      maybe.ID <- x.name
      colm.ID <- i
    }
  }
  .dash(90)

  if (colm.ID > 0  &&  !var_labels) {
    cat("\n\n")
    txt <- maybe.ID
    cat(
"For the column ", txt, ", each row of data is unique. Are these values\n",
"a unique ID for each row? To define as a row name, re-read the data file\n",
"with the following setting added to your Read() statement: row_names=", 
        toString(colm.ID), sep="")
  }

  if (!brief) {
    n_cat <- getOption("n_cat")
    num.cat <- FALSE
    n_cat.temp  <- 4
    for (j in 1:n.var) {
      if (is.double(data[,j]) && nu[j] <= n_cat.temp) num.cat <- TRUE
    }
    if (num.cat) {
      cat("\n\n")
      cat("Each of these variables has numeric values, but has less than ",
          n_cat.temp, "\n",
          "unique values_ Perhaps these variables are categorical. Consider\n",
          "to transform each variable into a factor with the\n",
          "factor() function.j\n", 
          "Or, specify a value for n_cat, ",
          "such as:  > style(n_cat=4)\n", sep="")
      .dash(63)
      for (j in 1:n.var) if (is.double(data[,j]) && nu[j] <= n_cat.temp)
        cat(names(data)[j], "\n")
      .dash(63)
    }
    
    cat("\n\n")

    n.miss_tot <- sum(is.na(data))
    
    if (n.miss_tot > 0) {  # missing data exists
      cat("Missing Data Analysis\n")
      .dash(60)
      
#     cat("n.miss ", "Observation\n")
      bad <- integer(length=n.var)
      n.lines <- 0
      i <- 0
      while ( i<n.obs && n.lines>=0 ) {
        i <- i + 1
        n.miss <- sum(is.na((data)[i, ]))
        if ( (n.miss >= n_mcut) && (n.lines < miss_show) ) {
          n.lines <- n.lines + 1
#         cat(n.miss, "     ", row.names(data[i, ]), "\n")
          bad[n.lines] <- i 
        }
        if (n.lines == miss_show) {
          n.lines <- -1
          cat("\nMore data rows have at least this many missing values: ",
              n_mcut, "\n",
            " Specify n_mcut=2 to see those lines with at least",
            "2 missing values, etc.\n",
            " Or increase the default value of miss_show=30 to show more",
            "rows of data with missing values.\n")
        }
      }  # end while

      cat("Number of cells in the data table: ", n.var*n.obs,"\n")
      cat("Number of missing data values: ", n.miss_tot,"\n")
      cat("Proportion of missing data values: ",
           round(n.miss_tot/(n.var*n.obs), 3), "\n")
      if (n.lines > -1)
        cat("Number of rows of data with missing values: ", n.lines,"\n")
      .dash(60)
      cat("\n")
      print(data[bad, ])
      .dash(60)
      cat("\n")

      if (miss_matrix && n.miss_tot>0) {
        cat("\n\nTable of Missing Values, 1 means missing\nn")
        print(matrix(as.double(is.na(data)), nrow=n.obs, ncol=n.var,
        dimnames = list(row.names(data), as.character(1:n.var)) ))
      }
          
    }  # end missing data processing

    else cat("No missing data\n\n")
  }


  # feedback regarding variable labels
  if (brief) cat("\n")
  l <- attr(data, which="variable.labels")
  myunits <- attr(data, which="variable.units")

  if (!is.null(l)) {
    cat("\nVariable Names   Variable Labels\n")
    n_rows <- length(l)
    n.lines <- min(max_lines, n_rows)

    mx.nm <- 0
    for (i in 1:n.lines) {  # width of var names
      if (!is.na(l[i])) if (nchar(names(l)[i]) > mx.nm)
        mx.nm <- nchar(names(l)[i])
    }
    width.nm <- mx.nm + 1

    mx.ln <- 0
    nc <- 0
    for (i in 1:n.lines) {  # get width of largest line
      if (!is.na(l[i])) nc <- nchar(as.character(l[i]))
      if (nc > mx.ln) mx.ln <- mx.nm + nc
    }
    width.ln <- max(mx.ln, nchar("Variable Names  Variable Labels"))
    width.ln <- min(mx.ln+3, 80)

    .dash(width.ln)
    for (i in 1:n.lines) {
      blanks <- paste(rep(" ", width.nm-nchar(names(l)[i])), collapse="")
      if (is.na(l[i])) l[i] <- ""
      cat(names(l)[i], blanks, l[i], "\n")
    }
    .dash(width.ln)

    if (n_rows > n.lines) {
      cat("To see all the variable labels set max_lines =", n_rows, "\n")
      .dash(64)
    }
  }
  #else
  #  cat("No variable labels\n")

    cat("\n")


  # feedback regarding variable units

  if (!is.null(myunits)) {
    cat("\nVariable Names   Variable Units\n")
    n.units <- length(myunits)
    n.lines <- min(max_lines, n.units)

    width.ln <- width.nm
    for (i in 1:n.lines)  # get width of largest line
      if (!is.na(myunits[i])) width.ln <- width.nm + nchar(as.character(myunits[i]))
    width.ln <- max(width.ln, 1+nchar("Variable Names  Variable Units"))

    .dash(width.ln)
    for (i in 1:n.lines) {
      blanks <- paste(rep(" ", 15-nchar(names(myunits)[i])), collapse = "")
      if (is.na(myunits[i])) myunits[i] <- ""
      cat(names(myunits)[i], blanks, myunits[i], "\n")
    }
    .dash(width.ln)
  }
  #else
  #  cat("No variable units\n")

  cat("\n")
}

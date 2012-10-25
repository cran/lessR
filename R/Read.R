Read <- 
function(ref=NULL, lessR.data=NULL, brief=FALSE, n.mcut=1, 
         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
         format=c("csv", "SPSS", "R"), data=TRUE, labels=FALSE, 
         missing="", max.lines=30, widths=NULL, dframe=mydata, ...) {
         
  format <- match.arg(format)

  if (!is.null(ref) && !is.null(lessR.data)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Cannot specify a file to read and a built-in file to read.\n\n")
  }

  dname <- deparse(substitute(dframe))

  # option for browsing for data file, and then display file name
  cat("\n")
  if (is.null(ref) && is.null(lessR.data)) {
    ref <- file.choose()
    .dash(68)
    cat("File: \n")
    cat("   ", ref, "\n")
    .dash(68)
    cat("\n")
  }

  # do the read
  if (!is.null(widths)) {
    assign(dname, read.fwf(file=ref, widths=widths, ...), pos=.GlobalEnv)
  }

  else if (!is.null(lessR.data)) {
    if (substr(lessR.data,1,4) == "data")
      txt <- ""
    else
      txt <- "data"
    file.name <- paste(txt, lessR.data, ".rda", sep="")

    path.name <- paste(find.package("lessR"), "/data/",  file.name, sep="")

    if (!file.exists(path.name)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No lessR data file with that name.\n\n",
        "To view the list of data files, enter  > Help(lessR)\n",
        "The data file names begin with  'data.'\n\n")
    }

    x.env <- new.env()  # scratch environment
    load(path.name, envir=x.env)

    dframe.name <-  paste(txt, lessR.data, sep="")
    dframe <- get(dframe.name, pos=x.env)

    assign(dname, dframe, pos=.GlobalEnv)
  }

  else {

    if (labels && !data)
      assign("mylabels", read.csv(file=ref, row.names=1, col.names=c("","label"),
                            header=FALSE), pos=.GlobalEnv)

    else if (labels && data) {
      mylabels <- read.csv(file=ref, nrows=1, ...)
      var.names <- names(mylabels)
      mylabels <- data.frame(t(mylabels))
      names(mylabels) <- "label"
      assign("mylabels", mylabels, pos=.GlobalEnv)
      assign(dname, suppressWarnings(read.csv(file=ref, skip=1, 
                     na.strings=missing, col.names=var.names, ...)), pos=.GlobalEnv)
    }

    else if (!labels && data) {
      if (grepl(".sav$", ref)) format <- "SPSS" 
      if (grepl(".rda$", ref)) format <- "R" 

      if (format == "csv")
        assign(dname, suppressWarnings(read.csv(file=ref, 
          na.strings=missing, ...)), pos=.GlobalEnv)

      else if (format == "SPSS") {
        assign(dname, suppressWarnings(read.spss(file=ref, 
              to.data.frame = TRUE, ...)), pos=.GlobalEnv)
        if (!is.null(attr(dframe, which="variable.labels"))) {
          mylabels <- matrix(nrow=ncol(dframe), ncol=2)
          for (i in 1:ncol(dframe)) {
            mylabels[i,1] <- attr(dframe, which="names")[i]
            mylabels[i,2] <- attr(dframe, which="variable.labels")[i]
          }
          mylabels <- data.frame(mylabels, row.names=1)
          names(mylabels) <- c("label")      
          assign("mylabels", mylabels, pos=.GlobalEnv)
        }
      }

      else if (format == "R")
        load(file=ref, envir=.GlobalEnv)
    }

    if (labels && !brief) {
      n.labels <- nrow(get("mylabels", pos=.GlobalEnv))
      n.lines <- min(max.lines, n.labels)
      cat("\n")
      cat("Name of data frame that contains the labels:  mylabels ", "\n")
      cat("Number of Rows in mylabels: ", n.labels, "\n")
      cat("\n")
      .dash(64)
      if (n.labels > n.lines)
        txt <- paste("First ", toString(n.lines)) else txt <- "The"
      cat(txt, "variable names and labels\n")
      .dash(64)
      print(head(mylabels, n=n.lines))
      .dash(64)
      if (n.labels > n.lines) {
        cat("To see all the variable labels, enter:  mylabels\n")
        .dash(64)
        cat("\n")
      }
      cat("\n")
    }

  }


  if (data  &&  !brief) {  # feedback regarding data
    n.var <- ncol(dframe)
    n.obs <- nrow(dframe)
    n.lines <- min(max.lines, ncol(dframe))

    nu <- integer(length(n.var))

    ord.fac <- FALSE
    for (i in 1:n.var) if (class(dframe[,i])[1] == "ordered") ord.fac <- TRUE
    
    cat("\n")
    cat("Basics\n")
    .dash(55)
    cat("Name of data frame that contains the data:", dname, "\n")
    cat("Number of Variables in ", dname, ":    ", n.var, "\n", sep="")
    cat("Number of Rows of Data in ", dname, ": ", n.obs, "\n", sep="")
    .dash(55)

    cat("\n\n")
    cat("Row Names\n")
    .dash(67)
    cat("First two row names:", row.names(dframe)[1],"   ", 
        row.names(dframe)[2], "\n") 
    cat("Last two row names: ", row.names(dframe)[n.obs-1],
        "   ", row.names(dframe)[n.obs], "\n") 
    .dash(67)

    cat("\n\n")
    cat("Variable Names and Types of the Values Read\n")
    .dash(67)
    cat("factor: Non-numeric categories, which, as read here, are unordered\n")
    if (ord.fac)
      cat("ordfact: Ordered, non-numeric categories\n")
    cat("integer: The values are numeric and integers\n")
    cat("numeric: The values are numeric with decimal digits\n")
    .dash(67)
    if (format == "SPSS")
      cat("Note: For SPSS files, all variables with numeric values are",
          "interpreted as numeric, even integers.\n") 

    max.char <- 0
    for (i in 1:n.var) {
      x.name <- names(dframe)[i]
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

      x.name <- names(dframe)[i]

      nu[i] <- length(unique(na.omit(dframe[,i])))

      n <- sum(!is.na(dframe[,i]))
      n.miss <- sum(is.na(dframe[,i]))

      the.class <- class(dframe[,i])[1]  # could be an ordered factor
      if (the.class == "ordered") the.class <- "ordfact"

      cat(.fmtc(x.name,pad-1), .fmtc(the.class,8), .fmti(n,7),
          .fmti(n.miss,7), .fmti(nu[i],7))

      n1 <- as.character(dframe[1,i])  # the as.character is for cat of factors 
      n2 <- as.character(dframe[2,i]) 
      n3 <- as.character(dframe[3,i])
      e3 <- as.character(dframe[n.obs-2,i])
      e2 <- as.character(dframe[n.obs-1,i])
      e1 <- as.character(dframe[n.obs,i])
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

      if (nu[i]==n  &&  (is.factor(dframe[,i]))) {
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
    for (j in 1:n.var) {
      if (is.numeric(dframe[,j]) && nu[j] <= n.cat) int.cat <- TRUE
    }
    if (int.cat) {
      cat("\n\n")
      cat("Each of the following variables is numeric, ", 
         "but has less than or equal n.cat=", n.cat," \n",
         "unique values. So will treat these variables as categorical in ",
         "subsequent analyses.\n",
         "  If really categorical, better to transform these variables ",
         "into a factor with\n",
         "     the functions: Transform and factor, to see examples ",
         "enter:  > ?trans\n", 
         "  If really integer, increase the value of n.cat, ",
         "such as:  > set(n.cat=8)\n", sep="")
      .dash(83)
      for (j in 1:n.var) if (is.numeric(dframe[,j]) && nu[j] <= n.cat)
        cat(names(dframe)[j], "\n")
      .dash(83)
    }
    
    cat("\n\n")
    cat("Missing Data Analysis\n")
    .dash(52)

    n.miss.tot <- sum(is.na(dframe))
    
    if (n.miss.tot > 0) {
    
    cat("n.miss ", "Observation\n")
    n.lines <- 0
    i <- 0
    while ( i<n.obs && n.lines>=0 ) {
      i <- i + 1
      n.miss <- sum(is.na((dframe)[i, ]))
      if ( (n.miss >= n.mcut  || miss.zero) && (n.lines < miss.show) ) {
        n.lines <- n.lines + 1
        cat(n.miss, "     ", row.names((dframe)[i, ]), "\n")
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
      print(matrix(as.numeric(is.na(dframe)), nrow=n.obs, ncol=n.var,
      dimnames = list(row.names(dframe), as.character(1:n.var)) ))
    }
          
   }

   else cat("No missing data\n\n")

  }

}


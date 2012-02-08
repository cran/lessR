rad <- 
function(ref=NULL, brief=FALSE, show.R=FALSE, attach=FALSE,
         n.mcut=1, miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
         format=c("csv", "SPSS"), data=TRUE, labels=FALSE, 
         missing="", max.lines=30, ...) {
         
format <- match.arg(format)

pre <- ">"
line <- "----------------------------------------------------------------\n"

# option for browsing for data file, and then display file name
cat("\n")
if (is.null(ref)) {
  ref <- file.choose()
  cat(line)
  cat("File: \n")
  cat("   ", ref, "\n")
  cat(line)
}
# do the read
if (labels && !data)
  mylabels <<- read.csv(file=ref, row.names=1, col.names=c("","label"), header=FALSE)
else if (labels && data) {
  mylabels <- read.csv(file=ref, nrows=1, ...)
  var.names <- names(mylabels)
  mylabels <<- data.frame(t(mylabels))
  names(mylabels) <<- "label"
  mydata <<- read.csv(file=ref, skip=1, na.strings=missing, ...)
  names(mydata) <<- var.names
}
else if (!labels && data) {
  if (grepl(".sav$", ref)) format <- "SPSS" 
  if (format == "csv") mydata <<- read.csv(file=ref, na.strings=missing, ...)
  if (format == "SPSS") {
    check.foreign <- suppressWarnings(require(foreign, quietly=TRUE))
    if (check.foreign) {
      mydata <<- read.spss(file=ref, to.data.frame = TRUE, ...)
      if (!is.null(attr(mydata, which="variable.labels"))) {
        mylabels <- matrix(nrow=ncol(mydata), ncol=2)
        for (i in 1:ncol(mydata)) {
          mylabels[i,1] <- attr(mydata, which="names")[i]
          mylabels[i,2] <- attr(mydata, which="variable.labels")[i]
        }
      mylabels <- data.frame(mylabels, row.names=1)
      names(mylabels) <- c("label")      
      mylabels <<- mylabels
    }
    }
    else {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        ">>> Reading a SPPS .sav data file requires package:  foreign\n",
        ">>> To obtain the foreign package, run one time only: ",
        "install.packages('foreign')\n\n")
    }
    if (attach) attach(mydata, warn.conflicts=FALSE)
  }
}

if (labels && !brief) {
  n.labels <- nrow(get("mylabels", pos=.GlobalEnv))
  n.lines <- min(max.lines, n.labels)
  cat("\n\n")
  cat("Name of data frame that contains the labels:  mylabels ", "\n")
  cat("Number of Rows in mylabels: ", n.labels, "\n")
  cat("\n")
  cat(line)
  if (n.labels > n.lines)
    txt <- paste("First ", toString(n.lines)) else txt <- "The"
  cat(txt, "variable names and labels\n")
  cat(line, "\n")
  print(head(mylabels, n=n.lines))
  cat("\n", line, sep="")
  if (n.labels > n.lines) {
    cat("To see all the variable labels, enter:  mylabels\n")
    cat(line, sep="", "\n")
  }
}

if (data) {

  if(show.R) {
    if(ref == "file.choose()") {
      cat(line, pre, " mydata <- read.csv(file.choose())", "\n", sep="")
      cat("\nFile: ", ref, "\n")
     }
     else cat(line, pre, " mydata <- read.csv(file=\"",ref,"\")", "\n", sep="")
     if (attach) cat(pre, " attach(mydata)", "\n", line, sep="")
  }

  if (!brief) {
    n.var <- ncol(mydata)
    n.obs <- nrow(mydata)
    n.lines <- min(max.lines, ncol(mydata))

    cat("\n\n")
    cat("Name of data frame that contains the data:  mydata ", "\n")
    cat("Number of Columns in mydata:    ", n.var, "\n")
    cat("Number of Rows of Data in mydata: ", n.obs, "\n")
    
    cat("\n")
    cat(line)
    if (n.obs > max.lines) {  
      cat("Variable names and first and last three rows of data", "\n")
      cat(line, "\n")
      if (show.R) 
        cat(line, pre, " head(mydata, n=3)   # First 3 rows", "\n", line, sep="", "\n")
      print(head(mydata, n=3))   
      cat("\n\n")
      if (show.R) 
        cat(line, pre, " tail(mydata, n=3)   # Last 3 rows", "\n", sep="", line, "\n")
      print(tail(mydata, n=3))
    }
    else {
      cat("Variable names and the data", "\n")
      cat(line, "\n")
      if (show.R) 
        cat(line, pre, " mydata", "\n", line, sep="", "\n")
      print(mydata)   
      cat("\n")
    }
    
    if (format == "csv") {
      cat("\n\n")
      if (show.R) 
        cat(line, pre, " str(mydata, digits.d=15)   # Types of variables", "\n", sep="")
      cat(line)
      cat("Data type of each variable\n")
      cat(line)
      cat("Factor: Variable with non-numeric values, stored as an integer\n")
      cat("num: Variable with numeric values that may have decimal digits\n")
      cat("int: Variable with numeric values limited to integer values\n")
      cat(line, "\n")
      cat(str(mydata, digits.d=15))
    }
    
    cat("\n\n")
    cat(line)
    cat("Missing Data Analysis\n")
    cat(line, "\n")

    n.miss.tot <- sum(is.na(mydata))
    
    if (n.miss.tot > 0) {
    
      cat("n.miss ", "Variable\n")
      for (i in 1:n.var) {
        n.miss <- sum(is.na((mydata)[, i]))
        if (n.miss > 0 || miss.zero) cat(n.miss, "    ", names((mydata)[i]),  "\n")
      }
      
    cat("\nn.miss ", "Observation\n")
    n.lines <- 0
    i <- 0
    while ( i<n.obs && n.lines>=0 ) {
      i <- i + 1
      n.miss <- sum(is.na((mydata)[i, ]))
      if ( (n.miss >= n.mcut  || miss.zero) && (n.lines < miss.show) ) {
        n.lines <- n.lines + 1
        cat(n.miss, "     ", row.names((mydata)[i, ]), "\n")
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
    cat("\n\n")

    cat("Total number of cells in data table: ", n.var*n.obs,"\n\n")
    cat("Total number of cells with the value missing: ", n.miss.tot,"\n")
  
    if (miss.matrix && n.miss.tot>0) {
      cat("\n\nTable of Missing Values, 1 means missing\n\n")
      print(matrix(as.numeric(is.na(mydata)), nrow=n.obs, ncol=n.var,
      dimnames = list(row.names(mydata), as.character(1:n.var)) ))
    }
          
   }

   else cat("No missing data\n\n")

  }

# provide some guidance for what to do next
  if (!brief) {
    cat("\n\n")
    cat(line, "What is next? Try one of the following.", "\n", sep="", line)
    cat("mydata: List all rows (observations) of data\n")
    cat("full(): A data summary and graph for each variable in the data table, mydata\n")
    cat("help.me(): List of topics for analysis with related R/lessR functions\n")
    cat(line, sep="")
    cat("\n")

    cat("\n")
    cat("Note to users of previous versions of lessR\n")
    cat(line, sep="")
    cat("As of lessR version 2.1, mydata is no longer attached, which is no longer\n",
        "needed for lessR functions.  To mimic previous behavior with standard R\n",
        "functions, manually add an attach(mydata) statement, use the with function\n",
        "or put mydata$ in front of each variable name.\n")
    cat(line, sep="")
    cat("\n")
  }
}

}

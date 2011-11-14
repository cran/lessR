# read, attach, display
rad <- 
function(ref=NULL, display=TRUE, show.R=FALSE, no.attach=FALSE, 
         n.cut=1, miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
         format=c("csv", "SPSS"), ...) {
         
format <- match.arg(format)

pre <- ">"
line <- "------------------------------------------------------------\n"

cat("\n")
if (is.null(ref)) ref <- file.choose()

cat(line)
cat("Data file: \n")
cat("   ", ref, "\n")
cat(line)

if (format == "csv") mydata <<- read.csv(file=ref, ...)
if (format == "SPSS") {
  check.foreign <- suppressWarnings(require(foreign, quietly=TRUE))
  if (check.foreign) {
    mydata <<- read.spss(file=ref, to.data.frame = TRUE, ...)
  }
  else {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      ">>> Reading a SPPS .sav dedta file requires package:  foreign\n",
      ">>> To obtain the foreign package, run one time only: ",
      "install.packages('foreign')\n\n")
  }
}

if (!no.attach) attach(mydata, warn.conflicts=FALSE)

if(show.R) {
if(ref == "file.choose()") {
  cat(line, pre, " mydata <- read.csv(file.choose())", "\n", sep="")
  cat("\nFile: ", ref, "\n")
 }
 else cat(line, pre, " mydata <- read.csv(file=\"",ref,"\")", "\n", sep="")
 cat(pre, " attach(mydata)", "\n", line, sep="")
}

if (display) {
  n.var <- ncol(mydata)
  n.obs <- nrow(mydata)

  if (nargs() > 1) cat("Plus the optional arguments that were entered in rad.", "\n")
  cat("\n")
  cat("Name of data frame that contains the data:   mydata ", "\n")
  cat("Number of Columns in mydata:    ", n.var, "\n")
  cat("Number of Rows of Data in mydata: ", n.obs, "\n")
  
  cat("\n\n")
  cat(line)
  cat("Variable names, first and last three rows of data\n")
  cat(line, "\n")
  if (show.R) 
    cat(line, pre, " head(mydata, n=3)   # First three rows", "\n", line, sep="", "\n")
  print(head(mydata, n=3))   
  cat("\n\n")
  if (show.R) 
    cat(line, pre, " tail(mydata, n=3)   # Last three rows", "\n", sep="", line, "\n")
  print(tail(mydata, n=3))
  
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
      if ( (n.miss >= n.cut  || miss.zero) && (n.lines < miss.show) ) {
        n.lines <- n.lines + 1
        cat(n.miss, "     ", row.names((mydata)[i, ]), "\n")
      }
      if (n.lines == miss.show) {
        n.lines <- -1
        cat("\nToo many data rows have at least this many missing values: ", n.cut, "\n",
          "No more lines displayed to conserve space.\n",
          "Specify n.cut=2 to see just those lines with 2 missing values, etc,",
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

    cat("\n\n")
    cat(line, "What is next? Try one of the following.", "\n", sep="", line)
    cat("mydata: List all rows (observations) of data, or just enter the\n")
    cat("        variable name to see the data just for that variable\n")
    cat("full(): A data summary and graph for each variable in the data table, mydata\n")
    cat("help.me(): List of topics for analysis with related R/lessR functions\n")
    cat(line, sep="")
    cat("\n")
	  
   }

   else cat("No missing data\n\n")

  }

}

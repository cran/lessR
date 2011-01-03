# read, attach, display
rad <- 
function(ref=NULL, display=TRUE, ...) {
  
  if (display) {
  
    pre <- ">"
    line <- "------------------------------------------------------------\n"
   
    cat("\n")
    if (is.null(ref)) {
      cat(line, pre, " mydata <- read.csv(file.choose())", "\n", sep="")
      ref <- file.choose()
      cat("\nFile: ", ref, "\n")
    }
    else {
      cat(line, pre, " mydata <- read.csv(file=\"",ref,"\")", "\n", sep="")
    }
    mydata <<- read.csv(file=ref, ...)
    
    cat(pre, " attach(mydata)", "\n", line, sep="")
    attach(mydata, warn.conflicts=FALSE)
    
    if (nargs() > 1) cat("Plus the optional arguments that were entered in rad.", "\n")
    cat("\n")
    cat("Name of data frame that contains the data:   mydata ", "\n")
    cat("Number of Variables in mydata:    ", ncol(mydata), "\n")
    cat("Number of Rows of Data in mydata: ", nrow(mydata), "\n")
    
    cat("\n\n")
    cat(line, pre, " head(mydata, n=3)   # First three rows", "\n", line, sep="", "\n")
    print(head(mydata, n=3))
    
    cat("\n\n")
    cat(line, pre, " tail(mydata, n=3)   # Last three rows", "\n", sep="", line, "\n")
    print(tail(mydata, n=3))
    
    cat("\n\n")
    cat(line, pre, " str(mydata, digits.d=15)   # Types of variables", "\n", sep="")
    cat(line)
    cat("Factor: Variable with non-numeric categories or levels, stored as an integer.\n")
    cat("int: Numeric variable limited to integer values.\n")
    cat("num: Numeric variable that may have decimal digits.\n")
    cat(line, "\n")
    print(str(mydata, digits.d=15))
    
    cat("\n\n")
    cat(line, "What is next? Try one of the following.", "\n", sep="", line)
    cat("mydata: List all rows (observations) of data, or just enter the\n")
    cat("        variable name to see the data just for that variable\n")
    cat("full(): A data summary and graph for each variable in the data table, mydata\n")
    cat("help.me(): List of topics for analysis with related R/lessR functions\n")
    cat(line, sep="")
    cat("\n")
  }
  
  else {
    mydata <<- read.csv(file=ref, ...)    
    attach(mydata, warn.conflicts=FALSE)
  }

}

# read, attach, display
rad <- 
function(ref=NULL, display=TRUE, show.R=FALSE, no.attach=FALSE, 
         format=c("csv", "SPSS"), ...) {
         
  format <- match.arg(format)
  
  pre <- ">"
  line <- "------------------------------------------------------------\n"
   
  cat("\n")
  if (is.null(ref)) ref <- file.choose()
  
  if (format == "csv") mydata <<- read.csv(file=ref, ...)
  if (format == "SPSS") mydata <<- read.spss(file=ref, to.data.frame = TRUE, ...)
  
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
    
    if (nargs() > 1) cat("Plus the optional arguments that were entered in rad.", "\n")
    cat("\n")
    cat("Name of data frame that contains the data:   mydata ", "\n")
    cat("Number of Columns in mydata:    ", ncol(mydata), "\n")
    cat("Number of Rows of Data in mydata: ", nrow(mydata), "\n")
    
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
      cat("int: Numeric variable limited to integer values\n")
      cat("num: Numeric variable that may have decimal digits\n")
      cat(line, "\n")
      print(str(mydata, digits.d=15))
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

}

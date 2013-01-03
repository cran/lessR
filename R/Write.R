Write <- 
function(ref=NULL, format=c("csv", "R"), data=mydata, ...) {

  format <- match.arg(format)

  dname <- deparse(substitute(data))

  if (!exists(dname, where=.GlobalEnv)) {
    cat("\n");
    if (grepl('"', dname))
      cat(">>> Do NOT have quotes around the data frame name.\n\n")
    stop(call.=FALSE, "\n","------\n",
         "Data frame ", dname, " does not exist.\n")
    cat("\n")
  }

  if (format == "csv") {
    if (is.null(ref))
      file.data <- paste(dname, ".csv", sep="")
    else {
       if (grepl(".csv", ref)) 
         txt <- ""
       else
         txt <- ".csv"
       file.data <- paste(ref, txt, sep="")
    }
    write.csv(data, file=file.data, ...)
  }

  else if (format == "R") {
    if (is.null(ref))
      file.data <- paste(dname, ".rda", sep="")
    else {
      if (grepl(".rda", ref)) 
        txt <- ""
      else
        txt <- ".rda"
      file.data <- paste(ref, txt, sep="")
    }
    save(list=dname, file=file.data, ...)
  }
  
  .showfile(file.data, c(dname, "data frame contents"))

}

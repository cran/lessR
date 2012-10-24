Write <- 
function(ref=NULL, type=c("csv", "R"), dframe=mydata, ...) {

  type <- match.arg(type)

  dname <- deparse(substitute(dframe))

  if (!exists(dname, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Data frame ", dname, " does not exist\n\n")
  }

  if (type == "csv") {
    if (is.null(ref))
      file.dframe <- paste(dname, ".csv", sep="")
    else {
       if (grepl(ref, ".csv")) 
         txt <- ""
       else
         txt <- ".csv"
       file.dframe <- paste(ref, txt, sep="")
    }
    write.csv(dframe, file=file.dframe, ...)
  }

  else if (type == "R") {
    if (is.null(ref))
      file.dframe <- paste(dname, ".rda", sep="")
    else {
      if (grepl(ref, ".rda")) 
        txt <- ""
      else
        txt <- ".rda"
      file.dframe <- paste(ref, txt, sep="")
    }
    save(list=dname, file=file.dframe, ...)
  }
  
  .showfile(file.dframe, c(dname, " data frame contents"))

}

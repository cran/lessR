Write <- 
function(file="mydata", type=c("csv", "R")) {

  type <- match.arg(type)

  if (!exists("mydata")) 
    stop("First need to have a data frame called mydata or specify one.")
  
  if (type == "csv") {
    file <- paste(file, ".csv", sep="")
    write.csv(mydata, file=file, row.names=FALSE)
  }
  else if (type == "R") {
    file <- paste(file, ".rda", sep="")
    save(mydata, file=file)
  }
  
  .showfile(file, "mydata contents")

}

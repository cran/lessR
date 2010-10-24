out <- 
function(myfile="mydata.csv") {

if (!exists("mydata")) stop("First need to have a data frame called mydata.")

pre <- ">"
line <- "------------------------------------------------------------\n"

cat(line, pre, sep="")
cat(" write.csv(mydata, file=\"",myfile, "\", row.names=FALSE)", sep="", "\n")
cat(line)

write.csv(mydata, file=myfile, row.names=FALSE)

if (getwd() =="/")
  workdir <- "top level of your file system"
else
  workdir <- getwd()
cat("csv file of mydata contents written at current working directory.\n")
cat("       ", myfile, "at:  ", workdir, "\n")

}

# read, attach, display
rad <- 
function(ref=NULL, ...) {

send.to.console <-
function (...) {
	args <- substitute(list(...))[-1L]
	sink(stdout())
	on.exit(sink())
	pf <- parent.frame()
	evalVis <- function(expr) withVisible(eval(expr, pf))
		for (i in seq_along(args)) {
			expr <- args[[i]]
			tmp <- switch(mode(expr), expression = lapply(expr, evalVis), 
				call = , name = list(evalVis(expr)), stop("bad argument"))
			for (item in tmp) if (item$visible) print(item$value)
		}
	on.exit()
}

pre <- ">"
line <- "------------------------------------------------------------\n"

if (is.null(ref)) {
	cat(line, pre, " mydata <- read.csv(file.choose())", "\n", line, sep="")
	ref <- file.choose()
	cat("\nFile: ", ref, "\n")
}
else {
	cat(line, pre, " mydata <- read.csv(file=\"",ref,"\")", "\n", line, sep="")
}
mydata <<- read.csv(file=ref, ...)

if (nargs() > 1) cat("Plus the optional arguments that were entered in rad.", "\n")
cat("\n")
cat("Name of data frame that contains the data:   mydata ", "\n")
cat("Number of Variables in mydata:    ", ncol(mydata), "\n")
cat("Number of Rows of Data in mydata: ", nrow(mydata), "\n\n")

cat(line, pre, " attach(mydata)", sep="", "\n")
attach(mydata, warn.conflicts=FALSE)

cat(pre, " head(mydata, n=3)", "\n", line, sep="", "\n")
send.to.console(head(mydata, n=3))

cat("\n")
cat(line, pre, " tail(mydata, n=3)", "\n", sep="", line, "\n")
send.to.console(tail(mydata, n=3))

cat("\n")
cat(line, "What is next? Can enter one of the following.", "\n", sep="", line)
cat("mydata: List all rows of data.\n")
cat("str(mydata): Detailed look at each variable in mydata.\n")
cat("help.me(): List of topics for analysis with related R functions.")
cat("\n", line, sep="")

}

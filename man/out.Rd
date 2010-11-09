\name{out}
\alias{out}

\title{Write the Contents of Data Frame mydata to a csv File}

\description{
Writes the contents of the data frame called \code{mydata} to a csv data file with a default name of \code{mydata.csv} to the current working directory.  The purpose of \code{out} is to save some keyboard entry, speeding up the process of writing a csv data file and reducing entry mistakes.
}

\usage{
out(myfile="mydata.csv")
}


\arguments{
  \item{myfile}{File reference, either null to specify the default of 
       \code{mydata.csv}, or a specified character string.}
}


\details{
\code{out} runs the R statement \cr
\code{write.csv(mydata, file=myfile, row.names=FALSE)}\cr
The name of the file, as well as the name of the working directory into which the file was written, are displayed at the console.

\code{out} is designed to work in conjunction with the function \code{\link{rad}} from this package, which reads a csv file into the data frame \code{mydata}.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{write.csv}}, \code{\link{rad}}.
}

\examples{
# create data frame called mydata
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(X,Y)

# write the contents of a data frame called mydata
#   into a csv file called "mydata.csv" in the working directory
# out()

# same as above, but specify the file name
out("mybestdata.csv")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }

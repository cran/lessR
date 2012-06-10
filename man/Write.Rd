\name{Write}
\alias{Write}
\alias{wrt}
\alias{wrt.r}

\title{Write the Contents of Data Frame mydata to an External File}

\description{
Abbreviation: \code{wrt}, \code{wrt.r}

Writes the contents of the data frame called \code{mydata} to to the current working directory as either the default \code{csv} data file or an R native data file.  
}

\usage{
Write(file="mydata", type=c("csv", "R"))

wrt(\ldots)

wrt.r(\ldots, type="R")
}


\arguments{
  \item{file}{File reference as a character string.}
  \item{type}{Type of file to be written with \code{.csv} as the default.}
  \item{...}{Other parameter values consistent with the usual \code{\link{write.table}}.}
}


\details{
Specify the file name without the file type, which \code{wrt} adds automatically, \code{.csv} for a comma separated values data file and \code{.rda} for a native R data file. The name of the file, as well as the name of the working directory into which the file was written, are displayed at the console.

\code{wrt} is designed to work in conjunction with the function \code{\link{Read}} from this package, which reads a csv, fixed width format, or native SPSS or R data files into the data frame \code{mydata}. \code{wrt} relies upon the R functions \code{\link{write.csv}} and \code{\link{save}}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}, \code{\link{write.csv}}, \code{\link{save}}.
}

\examples{
# create data frame called mydata
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(X,Y)

# specify the name of the output csv file
Write("mybestdata")
# short name
wrt("mybestdata")

# specify the name of the output native R data file
Write("mybestdata", type="R")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }

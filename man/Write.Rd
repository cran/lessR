\name{Write}
\alias{Write}
\alias{wrt}
\alias{wrt.r}

\title{Write the Contents of Data Frame mydata to an External File}

\description{
Abbreviation: \code{wrt}, \code{wrt.r}

Writes the contents of the specified data frame, such as with the default \code{mydata}, to the current working directory as either the default \code{csv} data file or an R native data file of the specified data frame.
}

\usage{
Write(ref, format=c("csv", "R"), data=mydata, \ldots)

wrt(\ldots)

wrt.r(\ldots, format="R")
}


\arguments{
  \item{ref}{Name of the output file as a character string, that is, with quotes.
        If not included in the name, the file type is automatically added to the
        name, either \code{.csv} or \code{.rda}, depending of the value of
        \code{format}.}
  \item{format}{Format of file to be written with \code{.csv} as the default.}
  \item{data}{Data frame to be written as an object, that is, no quotes.}
  \item{...}{Other parameter values consistent with the usual \code{\link{write.table}}, such as \code{row.names=FALSE} to suppress writing the row names.}
}


\details{
Can specify the file name without the file type, which \code{Write} adds automatically, \code{.csv} for a comma separated values data file and \code{.rda} for a native R data file. The default file name is the name of the data frame to be written. The name of the file that is written, as well as the name of the working directory into which the file was written, are displayed at the console. 


\code{Write} is designed to work in conjunction with the function \code{\link{Read}} from this package, which reads a csv, fixed width format, or native SPSS or R data files into the data frame \code{mydata}. \code{Write} relies upon the R functions \code{\link{write.csv}} and \code{\link{save}}.
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

# write the current contents of default data frame mydata to GoodData.csv
Write("GoodData")
# short name
# write the default data frame mydata to the R data file mydata.rda
wrt.r()

# access the R data frame warpbreaks
data(warpbreaks)
# write the file warpbreaks.rda
wrt.r(data=warpbreaks)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }

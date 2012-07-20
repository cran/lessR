\name{values}
\alias{values}

\title{List the Values of a Variable}

\description{
List the values of a variable from the global environment or a data frame.
}

\usage{
values(x, dframe=mydata, \ldots) 
}


\arguments{
  \item{x}{Variable for which to construct the histogram and density plots.}
  \item{dframe}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{\dots}{Other parameter values for as defined processed 
      by \code{\link{print}}, including \code{digits}.}
}


\details{
Provided for listing the values of a variable in an unattached data frame.  All \code{lessR} functions that access data for analysis from a data frame, such as the default \code{mydata} provided by the \code{\link{rad}} function that reads the data frame from an external data file, do not require the data frame to be attached.  Attaching a data frame can lead to some confusing issues, but one negative of not attaching is that simply listing the name of a variable within the data frame leads to an 'object not found' error.  The \code{values} function provides access to that variable within a data frame just as is true for any other \code{lessR} function that accesses data.

The function displays the values of the specified variable with the standard R \code{\link{print}} function, so parameter values for \code{\link{print}} can also be passed to \code{values}.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{print}}
}


\examples{
# generate 10 random normal data values
Y <- rnorm(10)
mydata <- data.frame(Y)
rm(Y)

# list the values of Y
values(Y)

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
values(breaks, dframe=warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ values }
\keyword{ print }

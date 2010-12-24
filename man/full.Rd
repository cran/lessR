\name{full}
\alias{full}

\title{Full Descriptive Analysis of all Variables in the Data Frame}

\description{
Call the following functions in this package:  \code{\link{describe}}, \code{\link{color.hist}} and \code{\link{color.barchart}}. The result is set of summary statistics for every variable in the data frame, by default called mydata, a histogram for each numerical variable and a bar chart for each non-numerical variable.
}

\usage{
full(x=NULL)
}

\arguments{
  \item{x}{Data frame that contains the variables to analyze, by default \code{mydata}.}
}


\details{
\code{full} is designed to work in conjunction with the function \code{\link{rad}} from this package, which reads a csv file into the data frame \code{mydata}.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{describe}}, \code{\link{color.hist}}, \code{\link{color.barchart}}.
}

\examples{
# create data frame called mydata
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(X,Y)

# full descriptive analysis of mydata
# full()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ descriptive }
\keyword{ histogram }
\keyword{ bar chart }

\name{CountAll}
\alias{CountAll}
\alias{ca}

\title{CountAll Descriptive Analysis of all Variables in the Data Frame}

\description{
Automatically call the following functions in this package:  \code{\link{SummaryStats}}, \code{\link{Histogram}} and \code{\link{BarChart}}. The result is set of summary statistics for every variable in the data frame, by default called \code{d}, a histogram for each numerical variable and a bar chart for each categorical variable.
}

\usage{
CountAll(x=d, quiet=FALSE, \dots)

ca(\ldots)
}

\arguments{
  \item{x}{Data frame that contains the variables to analyze, by default \code{d}.}
  \item{quiet}{Suppress text output if \code{TRUE}.}
  \item{\dots}{Other parameter values for graphics.}
}

\details{
\code{CountAll} is designed to work in conjunction with the \code{lessR} function \code{\link{Read}}, which reads a \code{csv} or other formatted data file into the data frame \code{d}. All the bar charts and associated summary statistics are written to one file and all the histograms and associated summary statistics for the numerical variables are written to another file.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{SummaryStats}}, \code{\link{Histogram}}, \code{\link{BarChart}}.
}

\examples{
# create data frame called d
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
d <- data.frame(X,Y)
rm(X); rm(Y)

# CountAll descriptive analysis of d
CountAll()
# short name
ca()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ descriptive }
\keyword{ histogram }
\keyword{ bar chart }

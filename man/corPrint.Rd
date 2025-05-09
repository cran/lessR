\name{corPrint}
\alias{corPrint}

\title{Print a Correlation Matrix with Special Formatting}

\description{
Display a correlation matrix in readable, compact form. 
}

\usage{
corPrint(R, min_value=0)
}

\arguments{
  \item{R}{Square, input correlation matrix.}
  \item{min_value}{The minimal value in magnitude of the displayed correlations.}
}

\details{
Drop the \code{0.} characters of each displayed correlation coefficient. For example, display 0.42 as 42. Have an option to only display correlations larger in magnitude than a minimum threshold, so that all correlations between \code{-min_value} and \code{+min_value} display as blank spaces.
}

\value{
The output correlation matrix.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Correlation}}, \code{\link{corReorder}}.
}

\examples{
mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("X1", "X2", "X3", "X4", "X5", "X6")
rownames(mycor) <- colnames(mycor)

# display all the correlations
corPrint(mycor)

# only display correlations in magnitude of 0.2 or larger
corPrint(mycor, min_value=.2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }


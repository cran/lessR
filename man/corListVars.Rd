\name{corListVars}
\alias{corListVars}
\alias{listvars}

\title{List Ordinal Position of Specified Variables in a Correlation Matrix}

\description{
Abbreviation: \code{listvars}

Each of the correlational analysis routines in \code{lessR}, such as \code{\link{corCFA}}, reference the specified variables by their ordinal position in the input correlation matrix.  For example, a 3 refers to the third variable in the matrix.

This routine lists, for each variable in the input correlation matrix, the ordinal position and corresponding variable name.
}

\usage{
corListVars(x=mycor)

listvars(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{\dots}{Parameter values.}
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{corCFA}}. \code{\link{corReflect}}, \code{\link{corProp}}, \code{\link{corReorder}}.
}

\examples{
# input correlation matrix of perfect two-factor model
# Factor Pattern for each Factor: 0.8, 0.6, 0.4
# Factor-Factor correlation: 0.3
mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("V1", "V2", "V3", "V4", "V5", "V6")
rownames(mycor) <- colnames(mycor) 

# list the variable names and ordinal positions of mycor
corListVars()

# abbreviated form
listvars()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }



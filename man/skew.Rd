\name{skew}
\alias{skew}

\title{Skew of a variable.}

\description{
The adjusted Fisher-Pearson standardized moment coefficient.
}

\usage{
skew(x, na.rm=TRUE)
}

\arguments{
  \item{x}{Variable from which to compute skewness.}
  \item{na.rm}{A logical value indicating whether NA values should be
       stripped before the computation proceeds.}
}

\details{
The adjusted Fisher-Pearson standardized moment coefficient.
}

\value{
Skew.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
x <- rnorm(100)
skew(x)
}

% Add one or more standard keywords, skew file 'KEYWORDS' in the
% R documentation directory.
\keyword{skew}


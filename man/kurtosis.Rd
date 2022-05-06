\name{kurtosis}
\alias{kurtosis}

\title{Kurtosis}

\description{
Kurtosis computed from the from the unbiased estimates of variance and of the fourth moment about the mean.
}

\usage{
kurtosis(x, na.rm=TRUE)
}

\arguments{
  \item{x}{Variable from which to compute kurtosis.}
  \item{na.rm}{A logical value indicating whether NA values should be
       stripped before the computation proceeds.}
}

\details{
Kurtosis as implemented by SAS, Type 2 formula as classified by Joanes and Gill (1998). This version of the formula relies upon the unbiased estimates of variance and of the fourth moment about the mean. A perfect normal distribution would have a kurtosis of 0.
}

\value{
kurtosis.
}

\references{
Joanes, D.N. and Gill, C.A (1998). Comparing measures of sample skewness and kurtosis. The Statistician, 47, 183-189.}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
x <- rnorm(100)
kurtosis(x)
}

% Add one or more standard keywords, kurtosis file 'KEYWORDS' in the
% R documentation directory.
\keyword{kurtosis}


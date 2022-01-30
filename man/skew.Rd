\name{skew}
\alias{skew}

\title{Skew of a variable.}

\description{
The Fisher-Pearson standardized moment coefficient adjusted for sample size.
}

\usage{
skew(x, na.rm=TRUE)
}

\arguments{
  \item{x}{Variable from which to compute skewness.}
  \item{na.rm}{A logical value indicating whether NA values should be
       removed before the computation proceeds.}
}

\details{
\code{G1}, the adjusted Fisher-Pearson standardized moment coefficient. The adjustment is the sample size \code{n} divided by the product of \code{n-1} and \code{n-2}.

The core component of the skewness expression is for each data value calculate, standardize the value, then raise the standardized value to the third power. The component is the sum of these cubics.  
}

\value{
Skew.
}


\references{
Doane, D. P. & Seward, L. E. (2011). Measuring Skewness: A Forgotten Statistic?,Journal of Statistics Education, 19(2), 1-18. URL: https://doi.org/10.1080/10691898.2011.11889611.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
x <- rnorm(100)
skew(x)
}

% Add one or more standard keywords, skew file 'KEYWORDS' in the
% R documentation directory.
\keyword{skew}


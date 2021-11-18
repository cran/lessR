\name{rescale}
\alias{rescale}

\title{Rescale a Variable}

\description{
Rescale a variable to either z-scores with a mean of 0 and standard deviation of 1, normalized with a minimum of 0 and a maximum of 1, or to a variable computed like a z-score except use the median in place of the mean and the IQR in place of the standard deviation.
}

\usage{
rescale(x, data=d, kind="z", digits_d=3)
}

\arguments{
  \item{x}{Variable to rescale.}
  \item{data}{Data frame that contains \code{x}.}
  \item{kind}{Type of rescaling.}
  \item{digits_d}{Number of significant digits.}
}

\details{
The default rescaling is standardization to z-scores, explicit with \code{kind} set to \code{"z"}, or just centering about the mean with \code{"center"}. For the min-max normalization to a range from 0 to 1, set \code{kind} to \code{"0to1"}. For the robust equivalent of standardization, set \code{kind} to \code{"robust"}. 

If \code{x} is a vector in the global environment, then set \code{data} to \code{NULL}.
}

\value{
The rescaled data.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{scale}}.
}

\examples{

# z-score for m01 
d <- Read("Employee")

d[, .("Salary")] 
x <- rescale(Salary)
x
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rescale }







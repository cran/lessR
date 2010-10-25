\name{prob.normal}
\alias{prob.normal}

\title{Compute and Plot Normal Curve Probabilities over an Interval}

\description{
Calculate the probability of an interval for a normal distribution with specified mean and standard deviation, providing both the numerical probability and a plot of the interval with the corresponding normal curve.
}

\usage{
prob.normal(mu=0, sigma=1, lo=NULL, hi=NULL, col.nrm="black", 
         col.fill.nrm="grey85", col.fill.int="steelblue3")
}


\arguments{
  \item{mu}{Population mean of normal distribution.}
  \item{sigma}{Population standard deviation of normal distribution.}
  \item{lo}{Lowest value in the interval for which to compute probability.}
  \item{hi}{Highest value in the interval for which to compute probability.}
  \item{col.nrm}{Color of the border of the normal curve.}
  \item{col.fill.nrm}{Fill color of the normal curve.}
  \item{col.fill.int}{Fill color of the interval for which the probability is computed.}
}


\details{
Calculate the normal curve probability for the specified interval and normal curve. If there is no upper value of the interval provided, \code{hi}, then the upper tail probability is provided, that is, from the specified value until positive infinity.  If there is no lower value, \code{lo}, then the lower tail probability is provided.  The probability is calculated with \code{\link{pnorm}}.
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}


\seealso{
\code{\link{pnorm}}, \code{\link{plot}}.
}


\examples{
# Mu=0, Sigma=1: # Normal probability for values between 0 and 2
prob.normal(lo=0, hi=2)

# Mu=0, Sigma=1: # Normal probability for values lower than 2
prob.normal(hi=2)

# Mu=0, Sigma=1: # Normal probability for values larger than 2
prob.normal(lo=2)

# Mu=100, Sigma=15: change default fill of plotted interval
prob.normal(m=100, s=15, lo=115, hi=125, col.fill.int="plum")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ probability }

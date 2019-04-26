\name{prob_norm}
\alias{prob_norm}
\alias{prob.norm}

\title{Compute and Plot Normal Curve Probabilities over an Interval}

\description{
Calculate the probability of an interval for a normal distribution with specified mean and standard deviation, providing both the numerical probability and a plot of the interval with the corresponding normal curve.
}

\usage{
prob_norm(lo=NULL, hi=NULL, mu=0, sigma=1, nrm_color="black", 
         fill_nrm="grey91", fill_int="slategray3", 
         ylab="", y_axis=FALSE, z=TRUE, axis_size=.9,
         pdf_file=NULL, width=5, height=5, \ldots)

prob.norm(\ldots)
}


\arguments{
  \item{lo}{Lowest value in the interval for which to compute probability.}
  \item{hi}{Highest value in the interval for which to compute probability.}
  \item{mu}{Population mean of normal distribution.}
  \item{sigma}{Population standard deviation of normal distribution.}
  \item{nrm_color}{Color of the border of the normal curve.}
  \item{fill_nrm}{Fill color of the normal curve.}
  \item{fill_int}{Fill color of the interval for which the probability is computed.}
  \item{ylab}{Label for the optional vertical axis_}
  \item{y_axis}{If \code{TRUE}, then a vertical axis is included.}
  \item{z}{If \code{TRUE}, then include z-values on the horizontal-axis_ Set to 
       \code{FALSE} if \code{mu=0} and \code{sigma=1}.}
  \item{axis_size}{Magnification factor for the axis labels, the value of
       \code{axis_cex}.}
  \item{pdf_file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\ldots}{Other parameter values for graphics.}
}

\details{
Calculate the normal curve probability for the specified interval and normal curve. If there is no upper value of the interval provided, \code{hi}, then the upper tail probability is provided, that is, from the specified value until positive infinity.  If there is no lower value, \code{lo}, then the lower tail probability is provided.  The probability is calculated with \code{\link{pnorm}}.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{pnorm}}, \code{\link{plot}}.
}


\examples{
# Mu=0, Sigma=1: Standard normal prob, values between 0 and 2
prob.norm(0,2)

# Mu=0, Sigma=1: Standard normal prob, values lower than 2
prob.norm(hi=2)

# Mu=0, Sigma=1: Standard normal prob, values larger than 2
prob.norm(lo=2)

# Mu=100, Sigma=15: Change default fill color of plotted interval
prob.norm(lo=115, hi=125, mu=100, sigma=15, fill_int="plum")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ probability }

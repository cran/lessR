\name{qnt.t}
\alias{qnt.t}

\title{Plot t-distribution Curve and Corresponding Normal Curve with Tail Cutoffs}

\description{
Plot a specified t-distribution against the standardized normal curve with the corresponding upper and lower tail cutoffs.
}

\usage{
qnt.t(df, alpha=0.05, dig.dec=3, y.axis=FALSE,
         col.fill="aliceblue", col.tail="palevioletred4",
         col.nrm=gray(.7), col.t=gray(.08), \ldots)
}


\arguments{
  \item{df}{Degrees of freedome for t-distribution, must be 2 or larger.}
  \item{alpha}{Alpha to define the tail cutoff area.}
  \item{dig.dec}{Number of decimal digits in the output.}
  \item{y.axis}{If \code{FALSE}, then the y axis is not displayed.}
  \item{col.fill}{Fill color for the interior of the t-distribution curve.}
  \item{col.tail}{Color of the tail areas of the t-distribution.}
  \item{col.nrm}{Color of the normal curve.}
  \item{col.t}{Color of the t-distribution curve.}
  \item{\ldots}{Other parameter values for graphics.}
}

\details{
Replaces a t-table by providing the corresponding t-cutoff, the quantile or critical value, as well as a plot that illustrates the tail probabilities and also compared to the standardized normal curve.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{pnorm}}, \code{\link{plot}}.
}


\examples{
# t-distribution with 0.025 cutoffs for degrees of freedom of 15
qnt.t(15)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ probability }
\keyword{ quantile }
\keyword{ t-distribution }

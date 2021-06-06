\name{prob_tcut}
\alias{prob_tcut}

\title{Plot t-distribution Curve and Specified Cutoffs with Normal Curve}

\description{
Plot a specified t-distribution against the standardized normal curve with the corresponding upper and lower tail cutoffs.
}

\usage{
prob_tcut(df, alpha=0.05, digits_d=3, y_axis=FALSE,
         fill="aliceblue", color_tail="palevioletred4",
         nrm_color=gray(.7), color_t=gray(.08),
         pdf_file=NULL, width=5, height=5, \ldots)
}


\arguments{
  \item{df}{Degrees of freedom for t-distribution, must be 2 or larger.}
  \item{alpha}{Alpha to define the tail cutoff area.}
  \item{digits_d}{Number of decimal digits in the output.}
  \item{y_axis}{If \code{FALSE}, then the y axis is not displayed.}
  \item{fill}{Fill color for the interior of the t-distribution curve.}
  \item{color_tail}{Color of the tail areas of the t-distribution.}
  \item{nrm_color}{Color of the normal curve.}
  \item{color_t}{Color of the t-distribution curve.}
  \item{pdf_file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\ldots}{Other parameter values for graphics.}
}

\details{
Replaces a t-table by providing the corresponding t-cutoff, the critical value based on the corresponding quantile, as well as a plot that illustrates the tail probabilities. Also compare to the standardized normal curve.
}

\value{
\code{cutoff}: Cutoff-value, the corresponding quantile.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{qt}}, \code{\link{pnorm}}.
}


\examples{
# t-distribution with 0.025 cutoffs for degrees of freedom of 15
prob_tcut(15)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ probability }
\keyword{ t-cutoff }
\keyword{ t-distribution }

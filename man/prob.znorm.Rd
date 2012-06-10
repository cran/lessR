\name{prob.znorm}
\alias{prob.znorm}

\title{Plot a Normal Curve with Shaded Intervals by Standard Deviation}

\description{
Display a normal curve with shading according to the z-score, the number of standard deviations from the mean.
}

\usage{
prob.znorm(mu=0, sigma=1, col.border="gray10",
         r=.10, g=.34, b=.94, a=.20,
         xlab="", ylab="", main="", 
         y.axis=FALSE, z=TRUE, mag=.9, \ldots)
}


\arguments{
  \item{mu}{Population mean of normal distribution.}
  \item{sigma}{Population standard deviation of normal distribution.}
  \item{col.border}{Color of the border of the normal curve.}
  \item{r}{Red component of fill color, from 0 to 1.}
  \item{g}{Green component of fill color, from 0 to 1.}
  \item{b}{Blue component of fill color, from 0 to 1.}
  \item{a}{Alpha component of fill color, that is, the transparency, from 0 to 1.}
  \item{xlab}{Label for the horizontal axis.}
  \item{ylab}{Label for the optional vertical axis.}
  \item{main}{Label for the graph title.}
  \item{y.axis}{If \code{TRUE}, then a vertical axis is included.}
  \item{z}{If \code{TRUE}, then include z-values on the horizontal-axis. Set to \code{FALSE} if \code{mu=0} and \code{sigma=1}.}
  \item{mag}{Magnification factor for the axis labels, the value of \code{cex.axis}.}
  \item{\ldots}{Other parameter values for graphics.}
}

\details{
Provide a normal curve with shading of each interval defined by the number of standard deviations from the mean.  The layers are written with transparency, and over-written so that the middle interval is the darkest and the most extreme intervals, beyond three standard deviations from the mean, are the lightest.  Specify \code{a=0} to turn off the colors.  Higher values of the alpha channel, as specified by \code{a}, yield darker colors. Specify \code{a=1} for the same solid color for all intervals. 

The normal densities are calculated with \code{\link{dnorm}} and plotted with \code{\link{plot}}.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{dnorm}}, \code{\link{plot}}.
}


\examples{
# Mu=0, Sigma=1: Standard normal 
prob.znorm()

# distribution for height of American women, mu=65.5, sigma=2.5
prob.znorm(65.5, 2.5, xlab="Height of American Women")

# do a red fill color
prob.znorm(65.5, 2.5, r=.9, xlab="Height of American Women")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ probability }



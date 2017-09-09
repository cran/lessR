\name{showWheel}
\alias{showWheel}

\title{Hue, Chroma, Luminance (hcl) Color Space Wheel}

\description{

Generates a color wheel for specified \code{hcl} colors at fixed values of chroma and luminance for a range of hues, or for a manually specified set of colors.
}

\usage{
  showWheel(clr=NULL, h.beg=0, h.end=300, n.colors=10, c=100, l=65,
           radius=0.9)
}

\arguments{
  \item{clr}{Optional specified colors to plot on the wheel. If specified,
  then the following parameters are not relevant.}/cr

  \item{h.beg}{Beginning hue, 0 to 360.}
  \item{h.end}{Ending hue, 0 to 360.}
  \item{n.colors}{Number of hues to display.}
  \item{c}{Constant value of chroma.}
  \item{l}{Constant value of luminance.}\cr

  \item{radius}{Size of wheel.}\cr
}


\details{
The \code{hcl} color space can provide palette of colors that have the same gray-scale intensities if desaturated. That means, no brightness bias for viewing different colors that represent different areas, such as in a bar chart of two variables, or a pie chart.

Plots a \code{hcl} color wheel with constant chroma and luminance. Or, enter a set of custom colors. Also lists the hue values and associated colors.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{hcl}}.
}


\examples{
# default color wheel
showWheel()

# generate a color wheel with 10 slices, starting at h=220 (blue)
showWheel(h.beg=220, h.end=200, n.colors=10)

# manual entry
showWheel(c("black", "blue", "red"))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hcl }
\keyword{ color }






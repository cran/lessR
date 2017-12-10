\name{showWheel}
\alias{showWheel}

\title{Hue, Chroma, Luminance (hcl) Color Space Wheel}

\description{

Generates a color wheel for specified \code{hcl} colors at fixed values of chroma and luminance for a range of hues, or for a manually specified set of colors.
}

\usage{
  showWheel(clr="discrete", h1=0, h2=NULL, n=10, c=100, l=65, fixup=TRUE,
         radius=0.9, \ldots)
}

\arguments{
  \item{clr}{Optional specified colors to plot on the wheel. If list of
  custom colors, then the following parameters are not relevant.}/cr

  \item{h1}{Beginning hue, 0 to 360.}
  \item{h2}{Ending hue, 0 to 360.}
  \item{n}{Number of hues to display.}
  \item{c}{Constant value of chroma.}
  \item{l}{Constant value of luminance.}
  \item{fixup}{R parameter name. If \code{TRUE}, then HCL values outside of
        the displayable RGB color space are transformed to fit into that space
        so as to display.}\cr

  \item{radius}{Size of wheel.}\cr

  \item{\ldots}{Other parameter values.}
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
showWheel(h1=220, h2=200, n=10)

# manual entry
showWheel(c("black", "blue", "red"))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hcl }
\keyword{ color }






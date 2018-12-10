\name{showPalettes}
\alias{showPalettes}

\title{Display Color Palettes}

\description{
For each specified set of palettes display each in the set.
}

\usage{
showPalettes(palette="hcl", n=12, border="transparent", file=NULL)
}


\arguments{
  \item{palette}{Name of the palette.}
  \item{n}{Number of colors per palette with a default of 12.}
  \item{border}{Border between intervals. By default is off.}
  \item{file}{Name of pdf file that contains the list of colors with a default of the name of the palette. Default is name of palette with a \code{.pdf} filetype.}
}


\details{
Available palettes are \code{"hcl"} for sequential palettes for each of 12 hues across the hcl color wheel in 30 degree intervals plus the qualitative scale of different hues and grayscale, \code{"viridis"}, and \code{"wesanderson"}.}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# all hcl palettes based on each  hue from 30 degrees of the color wheel,
#   including "colors" and "grays"
# default is 12 colors per palette
#showPalettes()

# viridis palate, simulate continuity
#showPalettes("viridis", n=500, border="off")}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }

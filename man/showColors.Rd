\name{showColors}
\alias{showColors}

\title{Display All Named R Colors and Corresponding rgb Values}

\description{
For each specified color, displays the color, the name and the associated \code{rgb} definition.
}

\usage{
showColors(file="colors.pdf", color=NULL)
}


\arguments{
  \item{file}{Name of pdf file that contains the list of colors with a default of \code{colors.pdf}.}
  \item{color}{NULL for all colors, otherwise specify a color and all colors which include that color as part of their name are displayed.}
}


\details{
Every color name is defined in terms of a red, a green and a blue component. This function lists the \code{rgb} definitions for the specified colors, as well as the name and a display of each color. The output should be routed to an external pdf file for storage. The directory and file name of the output file are displayed.}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# all colors
#showColors()

# all colors with 'blue' in their name
#showColors(file="theblues.pdf", color="blue")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }

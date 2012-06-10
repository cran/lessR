\name{set}
\alias{set}

\title{Set the Default Color Palette}

\description{
Each graph is based on a default color theme. The original default is "blue", but \code{set} allows other color palettes to be set as default as well.
}

\usage{
set(colors=c("blue", "gray", "rose", "green", "gold", "red"),
         trans.pts=NULL)
}


\arguments{
  \item{colors}{The specified color scheme.}
  \item{trans.pts}{Specified level of transparency for plotted points, from 0 for
        no transparency to 1 for total transparency.}
}


\details{
Sets the default color palette via the R \code{\link{options}} statement, as well as the transparency of plotted points.

The gray color scheme is based on the colors used in Hadley Wickham's \code{ggplot2} package.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Wickham, H., ggplot2: Elegant Graphics for Data Analysis, 2nd edition, 2009.
}

\seealso{
\code{\link{options}}.
}

\examples{
# set all subsequent graphic output to gray scale
set(colors="gray")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sets }
\keyword{ csv }



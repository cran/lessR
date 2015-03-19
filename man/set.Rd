\name{set}
\alias{set}

\title{Set the Default Color Theme and Other System Settings}

\description{
Each graph is based on a default color theme. The original default is \code{colors="dodgerblue"}, but \code{set} allows other color palettes to be set as default as well. Setting \code{ghost=TRUE} provides transparency effects against a black background. Other system settings are also available.}

\usage{
set(colors=c("blue", "gray", "rose", "green", "gold", "red",
         "dodgerblue", "purple", "sienna", "orange.black",
         "gray.black", "white"),

         col.fill.bar=NULL, trans.fill.bar=NULL,
         col.fill.pt=NULL, trans.fill.pt=NULL,
         col.stroke.bar=NULL, col.stroke.pt=NULL, 
         col.bg=NULL, col.grid=NULL, col.heat=NULL, ghost=NULL,

         n.cat=getOption("n.cat"), quiet=getOption("quiet"),
         brief=getOption("brief"), width=120, show=FALSE)
}


\arguments{
  \item{colors}{The specified color scheme.}
  \item{col.fill.bar}{Color of a filled region, such as a histogram bar.}
  \item{trans.fill.bar}{The transparency of a filled bar or rectangular region,
       such as a histogram bar or the box in a box plot. Value from 0 to 1,
       opaque to transparent.}
  \item{col.fill.pt}{Color of a filled region, such as a plotted point.}
  \item{trans.fill.pt}{The transparency of the inner region of a plotted point.
       Value from 0 to 1, opaque to transparent.}
  \item{col.stroke.bar}{Color of the border of a filled region such as a histogram
       bar.}
  \item{col.stroke.pt}{Color of a line or outline of a filled region, such as the
       border of a plotted point.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{col.heat}{Color of the heat map for correlation matrices.}
  \item{ghost}{If \code{TRUE}, add a black background, transparent grid lines
       and transparency for the bars. Overrides individual settings for
       those values. Setting to \code{FALSE} has no effect.}
  \item{n.cat}{Number of categories, which specifies the largest number of unique
        values of variable of a numeric data type for which the variable will be
        analyzed as categorical. Default is turned off, a value of 0.}
  \item{quiet}{If \code{TRUE} then some functions suppress console output.}
  \item{brief}{If set to \code{TRUE}, reduced text output. Can change system default
       with \code{\link{set}} function.}
  \item{width}{Maximum width of each line displayed at the console, just accesses
        the standard R options function for width.}
  \item{show}{Option for showing all settings.}
}


\details{
Sets the default color palette via the R \code{\link{options}} statement, as well as the transparency of plotted bars and points. Each time \code{colors} is reset, the specific color options are reset to their default values, which includes a transparency fill for plotted points of 0.66.

The gray color scheme is based on the colors used in Hadley Wickham's \code{ggplot2} package.

Set \code{ghost=TRUE} is equivalent to setting \code{col.bg} to \code{"black"} and \code{col.grid} to \code{"transparent"} with a bar transparency of 0.7. It overrides these settings individually, so must turn \code{ghost=FALSE} and set each of the three settings individually to change from the standard \code{ghost} settings.}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Wickham, Hadley, ggplot2: Elegant Graphics for Data Analysis, 2nd edition, Springer, 2009.
}

\seealso{
\code{\link{options}}.
}

\examples{
# set all subsequent graphic output to gray scale
set(colors="gray")

# define a custom color theme: sienna
# this works regardless of the set color theme because
#   all of the individual components of the theme are changed
# the trans options are the default values, so not necessary to
#   set unless already changed previously
# the three ghost settings - trans.fill.bar, col.grid and col.bg --
#   must be manually entered to achieve  a ghost plot
set(col.fill.bar="seagreen3")
set(col.fill.pt="seagreen3")
set(col.stroke.bar="seagreen4")
set(col.stroke.pt="seagreen4")
set(col.bg="seashell1")
set(col.grid="seashell2")
set(trans.fill.bar=.00)
set(trans.fill.pt=.66)

# all numeric variables with 8 or less unique values are analyzed
#   as categorical variables
set(n.cat=8)}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sets }
\keyword{ csv }



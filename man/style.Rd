\name{style}
\alias{style}
\alias{set}

\title{Set the Default Color Theme and Other System Settings}

\description{
Deprecated Names: \code{set}, \code{theme}

The color and style attributes of each plot can be individually set from the following list of attributes. For convenience, groups of these attributes are specified to define color themes, plus style sub-themes that apply to any theme, with default values:  \code{theme="lighbronze"} and \code{sub.theme="default"}. 
}

\usage{
style(
  theme=c("lightbronze", "darkred", "dodgerblue", "gray", "gold",
        "darkgreen", "blue", "red", "rose", "green", "purple", "sienna",
        "brown", "orange", "white"),
  sub.theme=c("default", "black", "no.y.axis"),

  device.fill=getOption("device.fill"),
  bg.fill=getOption("bg.fill"), bg.stroke=getOption("bg.stroke"),
  bg.lwd=getOption("bg.lwd"), bg.lty=getOption("bg.lty"),

  fill=NULL, bar.fill=getOption("bar.fill"), pt.fill=getOption("pt.fill"),
  trans=NULL, trans.bar.fill=getOption("trans.bar.fill"),
  trans.pt.fill=getOption("trans.pt.fill"),
  stroke=NULL, bar.stroke=getOption("bar.stroke"),
  pt.stroke=getOption("pt.stroke"),
  se.fill=getOption("se.fill"), ellipse.fill=getOption("ellipse.fill"),
  bubble.fill=getOption("bubble.fill"),
  bubble.text.stroke=getOption("bubble.text.stroke"),
  heat=getOption("heat"), 

  cex.axis=getOption("cex.axis"),
  axis.x.stroke=getOption("axis.x.stroke"),
  axis.y.stroke=getOption("axis.y.stroke"),
  values.stroke=getOption("values.stroke"),
  rotate.x=getOption("rotate.x"),
  rotate.y=getOption("rotate.y"),
  offset=getOption("offset"),

  grid.stroke=NULL,
  grid.x.stroke=getOption("grid.x.stroke"),
  grid.y.stroke=getOption("grid.y.stroke"),
  grid.lwd=getOption("grid.lwd"), grid.lty=getOption("grid.lty"),
  lab.stroke=getOption("lab.stroke"),
  lab.size=getOption("lab.size"),
  main.stroke=getOption("main.stroke"),
  segment.stroke=getOption("segment.stroke"),

  add.stroke=getOption("add.stroke"),
  add.cex=getOption("add.cex"),
  add.lwd=getOption("add.lwd"),
  add.lty=getOption("add.lty"),
  add.fill=getOption("pt.fill"),

  n.cat=getOption("n.cat"), suggest=getOption("suggest"),
  quiet=getOption("quiet"), brief=getOption("brief"),

  results=getOption("results"), explain=getOption("explain"),
  interpret=getOption("interpret"), document=getOption("document"), 
  code=getOption("code"),

  width=120, show=FALSE, \ldots)

set(\ldots)
}


\arguments{
  \item{theme}{The specified color scheme. If specified, re-sets all style
       attributes to their start-up values.}
  \item{sub.theme}{Further modification of the main themes.}\cr

  \item{device.fill}{Fill color of the entire device window.}
  \item{bg.fill}{Color of the plot background.}
  \item{bg.stroke}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}
  \item{bg.lwd}{Line width of the box around the plot.}
  \item{bg.lty}{Line type of the box around the plot: "solid", "dashed",
        "dotted", "dotdash", "longdash", or "twodash", or "blank".}\cr

  \item{fill}{Color of a filled region, both bars and points.}
  \item{bar.fill}{Color of a filled region, such as a histogram bar.}
  \item{pt.fill}{Color of a filled region, such as a plotted point.}
  \item{trans}{Transparency of a filled bar or rectangular region and points.}
  \item{trans.bar.fill}{The transparency of a filled bar or rectangular region,
       such as a histogram bar or the box in a box plot. Value from 0 to 1,
       opaque to transparent.}
  \item{trans.pt.fill}{The transparency of the inner region of a plotted point.
       Value from 0 to 1, opaque to transparent.}
  \item{stroke}{Color of a stroke, a line such as the border of bar or point.}
  \item{bar.stroke}{Color of the border of a filled region such as a histogram
       bar.}
  \item{pt.stroke}{Color of a line or outline of a filled region, such as the
       border of a plotted point.}
  \item{se.fill}{Color of the fill for the standard error plot about a
       fit line in a scatter plot.}
  \item{ellipse.fill}{Color of the fill for an ellipse in a scatter plot.}
  \item{bubble.fill}{Fill color of bubbles.}
  \item{bubble.text.stroke}{Color of bubble text.}
  \item{heat}{Color of the heat map for correlation matrices.}\cr

  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis
        values to be smaller than the axis labels. Provides the functionality of,
        and can be replaced by, the standard R \code{cex.axis.}}
  \item{axis.x.stroke}{Color of the \code{x}-axis.}
  \item{axis.y.stroke}{Color of the \code{y}-axis.}
  \item{values.stroke}{Color of the font used to label the axis values.}\cr
  \item{rotate.x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate.y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}\cr

  \item{grid.stroke}{Color of the grid lines.}
  \item{grid.x.stroke}{Color of the grid lines for the \code{x}-axis.}
  \item{grid.y.stroke}{Color of the grid lines for the \code{y}-axis.}
  \item{grid.lwd}{Width of grid lines.}
  \item{grid.lty}{Line type for grid lines: "solid", "dashed",
        "dotted", "dotdash", "longdash", or "twodash", or "blank".}
  \item{lab.stroke}{Color of the axis labels.}
  \item{lab.size}{Size of labels for x and y axes.}
  \item{main.stroke}{Color of the title.}
  \item{segment.stroke}{Color of line segments.}\cr

  \item{add.stroke}{Color of added text.}
  \item{add.cex}{Size of added text.}
  \item{add.lwd}{Line width of added lines.}
  \item{add.lty}{Line type of added lines.}
  \item{add.fill}{Fill color of added rectangles.}\cr

  \item{n.cat}{Number of categories that specifies the largest number of unique
        equally-spaced values of variable of a numeric data type for which the
        variable will be analyzed as categorical. Applies to
        \code{\link{ScatterPlot}} and 
        \code{\link{SummaryStats}}, and also to the functions such as 
        \code{\link{Histogram}} when processing multiple graphs.}
  \item{suggest}{If \code{TRUE}, then provide suggestions for alternative analyses.}
  \item{quiet}{If \code{TRUE} then some functions suppress console output.}
  \item{brief}{If set to \code{TRUE}, reduced text output.}\cr

  \item{results}{For the R markdown file generated by the Rmd option, 
        show the results.}
  \item{explain}{For the R markdown file generated by the Rmd option, 
        explain the results.}
  \item{interpret}{For the R markdown file generated by the Rmd option, 
        interpret the results.}
  \item{document}{For the R markdown file generated by the Rmd option,
        documents the code that generated the results.} 
  \item{code}{For the R markdown file generated by the Rmd option, 
        shows the code that generated the results.}\cr

  \item{width}{Maximum width of each line displayed at the console, just accesses
        the standard R options function for width.}
  \item{show}{Option for showing all settings.}
  \item{\dots}{Parameter values.}
}


\details{
OVERVIEW\cr
Sets the default color palette via the R \code{\link{options}} statement, as well as the transparency of plotted bars and points and other non-color characteristics such as the color of the grid lines. For convenience, groups of attributes are organized into themes and sub-themes. When \code{theme} is specified, \emph{all} options are reset to their default values. All other modifications, with individual parameters or grouped parameters as a sub-theme, are cumulative. For example, one sub-theme can be followed by another, as well as the specifications of individual attributes.

The \code{"gray"} color theme is based on the colors used in Hadley Wickham's \code{ggplot2} package. The \code{"lightbronze"} theme, especially with the \code{no.y.axis} sub-theme, is based on Jeffrey Arnold's \code{wsj} theme from his ggthemes package.

SUB-THEMES\cr
\code{"black"}: Black background of the entire device window witth translucent fill colors from the current theme. 
\code{"no.y.axis"}: Similar to the \code{wsj} theme from the \code{ggthemes} package, especially with the theme of \code{"lightbronze"}. The \code{y}-axis is removed with though the value labels retained, the vertical grid is removed, and the horizontal grid is dotted and thicker than the default.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Arnold, Jeffrey B., (2017), ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 3.4.0. https://CRAN.R-project.org/package=ggthemes

Wickham, Hadley, (2009), ggplot2: Elegant Graphics for Data Analysis, 2nd edition, Springer.
}

\seealso{
\code{\link{options}}.
}

\examples{

# some data
mydata <- rd("Employee", format="lessR", quiet=TRUE)

# gold colors embedded in a black background
style("gold", sub.theme="black")
Plot(Years, Salary, size=0, ellipse=seq(.1,.9,.1))

# four ways to go gray scale
# 1. remove the light bronze background from default lightbronze theme,
#    which otherwise displays only in gray scale
style(device.fill="white")
# 2. gray scale with a light gray background
style("gray")
# 3. gray scale with a dark, almost black, background
style("gray", sub.theme="black")
# 4. mostly black and white
style("white")

# define a custom style beyond just colors
style(bg.fill="off", bg.stroke="off",
      device.fill=rgb(247,242,230, maxColorValue=255),
      pt.fill="black", trans=0,
      lab.stroke="black", values.stroke="black",
      axis.y.stroke="off",
      grid.x.stroke="off", grid.y.stroke="black",  grid.lty="dotted", grid.lwd=1)
hs(Salary)


# all numeric variables with 8 or less unique values and equally spaced
#  intervals are analyzed as categorical variables
style(n.cat=8)}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sets }
\keyword{ csv }





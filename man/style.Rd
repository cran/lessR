\name{style}
\alias{style}
\alias{set}

\title{Set the Default Color Theme and Other System Settings}

\description{
Deprecated Names: \code{set}, \code{theme}

The color and style attributes of each plot can be set as a general theme, or individually set from the following list of attributes. For convenience, groups of these attributes are specified to define color themes, plus style sub-themes that apply to any theme, with default values:  \code{theme="lightbronze"} and \code{sub.theme="default"}. To reset to the default theme: \code{style()}. 
}

\usage{
style(
  theme=c("colors", "lightbronze", "dodgerblue", "darkred", "gray",
      "gold", "darkgreen", "blue", "red", "rose", "green", "purple",
	    "sienna", "brown", "orange", "white"),
  sub.theme=c("default", "black", "wsj"),
  set=NULL, get=FALSE, reset=TRUE,

  window.fill=getOption("window.fill"),
  panel.fill=getOption("panel.fill"),
  panel.color=getOption("panel.color"),
  panel.lwd=getOption("panel.lwd"),
  panel.lty=getOption("panel.lty"),

  fill=NULL,
  bar.fill=getOption("bar.fill"),
  bar.fill.discrete=getOption("bar.fill.discrete"),
  bar.fill.ordered=getOption("bar.fill.ordered"),
  trans=NULL,
  trans.bar.fill=getOption("trans.bar.fill"),
  color=NULL,
  bar.color=getOption("bar.color"),
  bar.color.ordered=getOption("bar.color.ordered"),
  bar.color.discrete=getOption("bar.color.discrete"),
  
  values=getOption("values"),
  values.color=getOption("values.color"), 
  values.size=getOption("values.size"),
  values.digits=getOption("values.digits"),
  values.position=getOption("values.position"),
		 
  pt.fill=getOption("pt.fill"),
  trans.pt.fill=getOption("trans.pt.fill"),
  pt.color=getOption("pt.color"),
  se.fill=getOption("se.fill"),
  ellipse.fill=getOption("ellipse.fill"),
  ellipse.color=getOption("ellipse.color"),
  ellipse.lwd=getOption("ellipse.lwd"),
  fit.color=getOption("fit.color"),
  fit.lwd=getOption("fit.lwd"),
  bubble.text.color=getOption("bubble.text.color"),
  segment.color=getOption("segment.color"),
  ID.color=getOption("ID.color"),
  area.fill=getOption("area.fill"),
  out.fill=getOption("out.fill"),
  out.color=getOption("out.color"),
  out2.fill=getOption("out2.fill"),
  out2.color=getOption("out2.color"),

  violin.fill=getOption("violin.fill"),
  violin.color=getOption("violin.color"),
  box.fill=getOption("box.fill"),
  box.color=getOption("box.color"),

  axis.color=getOption("axis.color"),
  axis.x.color=getOption("axis.x.color"),
  axis.y.color=getOption("axis.y.color"),
  axis.lwd=getOption("axis.lwd"),
  axis.x.lwd=getOption("axis.x.lwd"),
  axis.y.lwd=getOption("axis.y.lwd"),
  axis.lty=getOption("axis.lty"),
  axis.x.lty=getOption("axis.x.lty"),
  axis.y.lty=getOption("axis.y.lty"),
  axis.cex=getOption("axis.cex"),
  axis.x.cex=getOption("axis.x.cex"),
  axis.y.cex=getOption("axis.y.cex"),
  axis.text.color=getOption("axis.text.color"),
  axis.x.text.color=getOption("axis.x.text.color"),
  axis.y.text.color=getOption("axis.y.text.color"),
  rotate.x=getOption("rotate.x"),
  rotate.y=getOption("rotate.y"),
  offset=getOption("offset"),

  lab.color=getOption("lab.color"),
  lab.x.color=getOption("lab.x.color"),
  lab.y.color=getOption("lab.y.color"),
  lab.cex=getOption("lab.cex"),
  lab.x.cex=getOption("lab.x.cex"),
  lab.y.cex=getOption("lab.y.cex"),
  main.color=getOption("main.color"),
  main.cex=getOption("main.cex"),

  grid.color=getOption("grid.color"),
  grid.x.color=getOption("grid.x.color"),
  grid.y.color=getOption("grid.y.color"),
  grid.lwd=getOption("grid.lwd"),
  grid.x.lwd=getOption("grid.x.lwd"),
  grid.y.lwd=getOption("grid.y.lwd"),
  grid.lty=getOption("grid.lty"),
  grid.x.lty=getOption("grid.x.lty"),
  grid.y.lty=getOption("grid.y.lty"),

  strip.fill=getOption("strip.fill"),
  strip.color=getOption("strip.color"),
  strip.text.color=getOption("strip.text.color"),

  add.fill=getOption("add.fill"),
  add.trans=getOption("add.trans"),
  add.color=getOption("add.color"),
  add.cex=getOption("add.cex"),
  add.lwd=getOption("add.lwd"),
  add.lty=getOption("add.lty"),

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
       attributes to the values consistent with that theme.}
  \item{sub.theme}{Further modification of the main themes.}
  \item{set}{A list of parameter values, a theme, that was previously
       saved, and now is read back to become the current set of parameter
       values. See the examples.}
  \item{get}{Save the current list of parameter values, a theme, into
       an R object.}
  \item{reset}{Change one or more settings or the entire theme.}\cr

  \item{window.fill}{Fill color of the entire device window.}
  \item{panel.fill}{Color of the plot background.}
  \item{panel.color}{Color of border around the plot background, the box, that
        encloses the plot, with a default of \code{"black"}.}
  \item{panel.lwd}{Line width of the box around the plot.}
  \item{panel.lty}{Line type of the box around the plot. Acceptable values are 
        \code{"blank"}, \code{"solid"}, \code{"dashed"}, \code{"dotted"},
        \code{"dotdash"}, and \code{"longdash"}.}\cr

  \item{fill}{Color of a filled region -- bars, points and bubbles -- 
        depending on the
        objected plotted. Can explicitly choose \code{"grays"}
        or \code{"hues"} , or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}}
  \item{bar.fill}{Color of a filled bar, bubble or box.}
  \item{bar.fill.discrete}{Color of a filled bar chart bar or pie chart slice.}
  \item{bar.fill.ordered}{Color of a filled histogram bar.}
  \item{trans}{Transparency of a filled bar, rectangular region, or points from 0
       (none) to 1 (complete).}
  \item{trans.bar.fill}{The transparency of a filled bar or rectangular region,
       such as a histogram bar or the box in a box plot. Value from 0 to 1,
       opaque to transparent.}
  \item{color}{Color of a line segment such as the border of bar or point.
        Can explicitly choose \code{"grays"}
        or \code{"hues"}, or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}}
  \item{bar.color}{Color of the border of a filled region such as a histogram
       bar.}
  \item{bar.color.discrete}{Color of the border of a filled region for
       values on a qualitative scale.}
  \item{bar.color.ordered}{Color of the border of a filled region for
       values on a quantitative scale, such as a histogram bar.}\cr

  \item{values}{If not the default value of \code{"off"}, adds the
        numerical results to the plot according to \code{"\%"}, 
        \code{"prop"} or \code{"input"}, that is, percentages,
         proportions, or the value from which the slices are plotted,
         such as tabulated counts if \code{y} is not specified,
         or the value of \code{y} if the plotted values are provided. If
         any other \code{values} parameter is specified, default is set
         to \code{"\%"}.}
  \item{values.color}{Color of the plotted text. Could be a vector to specify
        a unique color for each value. If fewer colors are specified than
        the number of categories, the colors are recycled.}
  \item{values.size}{Character expansion factor, the size, of the plotted text, 
        for which the default value is 0.95.}
  \item{values.digits}{Number of decimal digits for which to display the values.
        Default is 0, round to the nearest integer, for \code{"\%"}
        and 2 for \code{"prop"}.}
  \item{values.position}{Position of the plotted text. Default is inside the pie,
        or, if \code{"label"}, as part of the label for each value
        outside of the pie.}\cr

  \item{pt.fill}{Color of a filled plotted point.}
  \item{trans.pt.fill}{The transparency of the inner region of a plotted point.
       Value from 0 to 1, opaque to transparent.}
  \item{pt.color}{Color of a line or outline of a filled region, such as the
       border of a plotted point.}
  \item{se.fill}{Color of the fill for the standard error plot about a
       fit line in a scatter plot.}
  \item{ellipse.fill}{Color of the fill for an ellipse in a scatter plot.}
  \item{ellipse.color}{Color of the border for an ellipse in a scatter plot.}
  \item{ellipse.lwd}{Line width of the border for an ellipse in a scatter plot.}
  \item{fit.color}{Color of the fit line in a scatter plot.}
  \item{fit.lwd}{Width of fit line. By default is 2 for Windows and 1.5 for Mac.}
  \item{bubble.text.color}{Color of the displayed text regarding the size of a bubble,
        either a tabulated frequency for categorical variables, or the value of a
        third variable according to \code{size}.}
  \item{segment.color}{Color of connecting line segments when there are also plotted
        points, such as in a frequency polygon. Default color is \code{color}.}
  \item{ID.color}{Color of the text to display the ID labels.}
  \item{area.fill}{Fill color of the area under a line in a run chart or time
       series.}
  \item{out.fill}{For a scatterplot, color of the border of potential outliers, which,
       for the unadjusted boxplot, are default values
       1.5 IQR's beyond the lower or upper quartile.}
  \item{out.color}{For a scatterplot, color of potential outliers.}
  \item{out2.fill}{For a scatterplot, color of extreme outliers, which,
       for the unadjusted boxplot, are default values
       3 IQR's beyond the lower or upper quartile.}
  \item{out2.color}{For a scatterplot, color of the border of extreme outliers.}\cr

  \item{violin.fill}{Fill color for a \bold{violin plot.}}
  \item{violin.color}{Border color for the violin in a violin plot.}
  \item{box.fill}{Fill color for a box plot.}
  \item{box.color}{Border color of a box in a box plot.}\cr

  \item{axis.color}{Color of the axes.}
  \item{axis.x.color}{Color of the \code{x}-axis.}
  \item{axis.y.color}{Color of the \code{y}-axis.}
  \item{axis.lwd}{Line width of axes.}
  \item{axis.x.lwd}{Line width of horizontal axis.}
  \item{axis.y.lwd}{Line width of vertical axis.}
  \item{axis.lty}{Line type of axes, either "solid", "dashed",
       "dotted", "dotdash", "longdash", "twodash", or "blank".}
  \item{axis.x.lty}{Line type of horizontal axis.}
  \item{axis.y.lty}{Line type of vertical axis.}
  \item{axis.cex}{Scale magnification factor, which by defaults displays the axis
        values to be smaller than the axis labels. Provides the functionality of,
        and can be replaced by, the standard R \code{cex.axis}.}
  \item{axis.x.cex}{Scale magnification factor for the \code{x}-axis.}
  \item{axis.y.cex}{Scale magnification factor for the \code{y}-axis.}
  \item{axis.text.color}{Color of the font used to label the axis values.}
  \item{axis.x.text.color}{Color of the font used to label the \code{x}-axis values.}
  \item{axis.y.text.color}{Color of the font used to label the \code{y}-axis values.}
  \item{rotate.x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate.y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}\cr

  \item{lab.color}{Color of the axis labels.}
  \item{lab.x.color}{Color of the axis labels on the horizontal axis.}
  \item{lab.y.color}{Color of the axis labels on the vertical axis.}
  \item{lab.cex}{Size of labels for x and y axes.}
  \item{lab.x.cex}{Size of labels for x.}
  \item{lab.y.cex}{Size of labels for y.}
  \item{main.color}{Color of the title.}
  \item{main.cex}{Size of the title font.}\cr

  \item{grid.color}{Color of the grid lines.}
  \item{grid.x.color}{Color of the grid lines for the \code{x}-axis.}
  \item{grid.y.color}{Color of the grid lines for the \code{y}-axis.}
  \item{grid.lwd}{Width of grid lines.}
  \item{grid.x.lwd}{Width of vertical grid lines, inherits from \code{grid.lwd}.}
  \item{grid.y.lwd}{Width of horizontal grid lines, inherits from \code{grid.lwd}.}
  \item{grid.lty}{Line type for grid lines: "solid", "dashed",
        "dotted", "dotdash", "longdash", or "twodash", or "blank".}
  \item{grid.x.lty}{Line-type of vertical grid lines, inherits from \code{grid.lty}.}
  \item{grid.y.lty}{Line-type of horizontal grid lines, inherits from \code{grid.lty}.}\cr

  \item{strip.fill}{Fill color for the \bold{strip that labels each panel} in
   a Trellis plot.}
  \item{strip.color}{Border color for the strip that labels each panel in
   a Trellis plot.}
  \item{strip.text.color}{Color of the label in each strip of a Trellis plot.}\cr

  \item{add.fill}{Interior fill color of added object.
        Can explicitly choose \code{"grays"}
        or \code{"hues"}, or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}}
  \item{add.trans}{Transparency level of color or fill, which ever is
       applicable from 0 (opaque) to 1 (transparent).}
  \item{add.color}{Color of borders and lines of added object.}
  \item{add.cex}{Text expansion factor, relative to 1. As with the following
   properties, can be a vector for multiple placement or objects.}
  \item{add.lwd}{Line width of added object.}
  \item{add.lty}{Line type of added object. See \code{panel.lty} for types.}\cr

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
Sets the default color palette via the R \code{\link{options}} statement, as well as the transparency of plotted bars and points and other non-color characteristics such as the color of the grid lines. For convenience, groups of attributes are organized into themes and sub-themes. When the \code{theme} is specified, \emph{all} options are reset to their default values. All other modifications, with individual parameters or grouped parameters as a sub-theme, are cumulative. For example, one sub-theme can be followed by another, as well as the specifications of individual attributes. Calling the function with no arguments sets to the default style.

Available themes:\cr
"lightbronze" [default]\cr
"dodgerblue" [default \code{lessR} 3.6.0 and earlier]\cr
"darkred"\cr
"gray"\cr
"gold"\cr
"darkgreen"\cr
"blue"\cr
"red"\cr
"rose"\cr
"green"\cr
"purple"\cr
"sienna"\cr
"brown"\cr
"orange"\cr
"white"\cr

The \code{"gray"} color theme is based on the colors used in Hadley Wickham's \code{ggplot2} package. The \code{"lightbronze"} theme, especially with the \code{wsj} sub-theme, is based on Jeffrey Arnold's \code{wsj} theme from his ggthemes package.

SUB-THEMES\cr
\code{"black"}: Black background of the entire device window with translucent fill colors from the current theme. 
\code{"wsj"}: Similar to the \code{wsj} theme from the \code{ggthemes} package, especially with the theme of \code{"lightbronze"}. The \code{y}-axis is removed with though the value labels retained, the vertical grid is removed, and the horizontal grid is dotted and thicker than the default.
}


\value{
The current settings can optionally be saved into an \code{R} object, and then read back at a later time with the \code{set} function.\cr
}


\references{
Arnold, Jeffrey B., (2017), ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 3.4.0. https://CRAN.R-project.org/package=ggthemes

Wickham, Hadley, (2009), ggplot2: Elegant Graphics for Data Analysis, 2nd edition, Springer.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{options}}.
}

\examples{

# some data
d <- rd("Employee", in.lessR=TRUE, quiet=TRUE)

# gold colors embedded in a black background
style("gold", sub.theme="black")
Plot(Years, Salary, size=0, ellipse=seq(.1,.9,.1))

# three ways to do gray scale
style(window.fill="white")
# 1. gray scale with a light gray background
style("gray")
# 2. gray scale with a dark, almost black, background
style("gray", sub.theme="black")
# 3. mostly black and white
style("white")

# reset style to the default "colors"
style()

# set bar fill to qualitative hcl colors
# here also turn off bar borders and set to a mild transparency
Histogram(Salary, fill="greens", color="off")
# same as
# style(bar.fill.ordered="greens", bar.color="off")
# Histogram(Salary)

# set bar fill to 6 blue colors
# for continuous band explicitly call getColors and specify n
#   to obtain the full spectrum, such as for analysis of Likert
#   scale responses with six possible responses per item
style(bar.fill=getColors("blues", n=6))

# adjust Trellis strip to a dark background
style(strip.fill="gray60", strip.color="gray20",
      strip.text.color=rgb(247,242,230, maxColorValue=255))
Plot(Years, Salary, by1=Gender)

# define a custom style beyond just colors
style(panel.fill="off", panel.color="off",
      window.fill=rgb(247,242,230, maxColorValue=255),
      pt.fill="black", trans=0,
      lab.color="black", axis.text.color="black",
      axis.y.color="off",
      grid.x.color="off", grid.y.color="black",  grid.lty="dotted", grid.lwd=1)
hs(Salary)

# save the current theme settings into an R object without changes
# unless set to FALSE, get is always TRUE, for all calls to style
mystyle <- style(get=TRUE)
# ...  bunch of changes
# then recall older settings to current theme setting
style(set=mystyle)

# create a gray-scale with a sub-theme of wsj
# save, and then at a later session read back in
grayWSJ <- style("gray", sub.theme="wsj")
Write("grayWSJ", data=grayWSJ, format="R")
# ...
mystyle <- Read("grayWSJ.rda")  # read grayWSJ.rda
style(set=mystyle)



# all numeric variables with 8 or less unique values and equally spaced
#  intervals are analyzed as categorical variables
style(n.cat=8)}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sets }
\keyword{ csv }







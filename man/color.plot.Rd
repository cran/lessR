\name{color.plot}
\alias{color.plot}

\title{Scatter-Plot, Function Plot, or Run Chart with Optional Dates in Color}

\description{
Plots individual points, such as for a scatterplot, or plots connected line segments for the plot of a function or a run chart, including an option for adding dates to the horizontal axis for a time series chart. One enhancement over the usual \code{\link{plot}} function is the automatic inclusion of color.  The color of the line segments and/or the points, background, area under the plotted line segments, gridlines, and border can each be explicitly specified, with default colors provided.  For Likert style response data, so that each value has less than 10 unique integer values, the points in the plot are transformed into a bubble plot with the size of each bubble, i.e., point, determined by the corresponding joint frequency.

For data exhibiting little trend, the the center line is provided for the generation of a run chart, plotting the values of a variable in order of occurrence over time.  When the center line, the median by default, is plotted, the analyses of the number and composition of the individual runs, number of consecutive values above or below the center line, is also displayed.  Also, the defaults change for each of the types of plots.  The intent is to rely on the default values for a relatively sophisticated plot, particularly when compared to the default values of the standard R \code{\link{plot}} function.

If a scatterplot is displayed, then the corresponding correlation coefficient as well as the hypothesis test of zero population correlation and the 95\% confidence interval are also displayed.  The same numeric values of the standard R function R \code{\link{cor.test}} function are generated, though in a more readable format.
}

\usage{
color.plot(x, y=NULL, type=NULL, col.line="darkblue", col.area=NULL,  
           col.point="darkblue", col.fill=NULL, col.grid="grey90", 
           col.bg="ghostwhite", col.box="black", xy.ticks=TRUE, 
           xlab=NULL, ylab=NULL, pch=NULL, cex=NULL, center.line=NULL,
           kind=c("regular", "bubble.freq", "sunflower.freq"),
           x.start=NULL, x.end=NULL, size=.25, 
           fit.line=c("none", "lowess", "ls"), col.fit.line="grey55", 
           col.bubble="lightsteelblue", col.flower="steelblue",
           time.start=NULL, time.by=NULL, time.reverse=FALSE, \ldots)
}


\arguments{
  \item{x}{If both x and y are specified, then the x values are plotted on the
      horizontal axis.  If x is not sorted, a scatter plot is produced.  If x is sorted, then a function is plotted with a smooth line.  If only x is specified with no y, then these x values are plotted as a run chart, which adds a centerline, the mean, to the chart by default if the data do not exhibit obvious trend. The time-oriented data for a run chart should be ordered with the first data record representing the earliest point in time.  Otherwise, use the \code{x.reverse} option.}
  \item{y}{Coordinates of points in the plot on the vertical axis.}
  \item{type}{Character string that indicates the type of plot, either \code{"p"} for 
        points, \code{"l"} for line, or \code{"b"} for both.  If x and y are provided and x is sorted so that a function is plotted, the default is \code{"l"}, or, when x is not sorted, the default is \code{"p"} for point, yielding a scatter plot.  If only the values of x are provided, then a run chart is produced with the default type of \code{"b"} for both points and lines.}
  \item{col.line}{Color of any plotted line segments, with a default of \code{"darkblue"}.}
  \item{col.area}{Color of area under the plotted line segments.  To have a border
        at the bottom and right of a run chart but retain the property of no area color, specify a color of \code{"transparent"}. If the values exhibit a trend and dates are specified with \code{x.start}, the default area color is \code{slategray3}, otherwise there is no default color. }
  \item{col.point}{Color of the border of the plotted points.}
  \item{col.fill}{For plotted points, the interior color of the point.  For a
        scatterplot the default value is transparent.  For a run chart the default value is the color of the point's border, \code{col.point}.}
  \item{col.grid}{Color of the grid lines, with a default of \code{"grey90"}.}
  \item{col.bg}{Color of the plot background.}
  \item{col.box}{Color of border around the plot background, the box, that encloses 
      the plot, with a default of \code{"black"}.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
      axes are to be displayed.}
  \item{xlab}{Label for x-axis. For two variables specified, x and y, if \code{xlab} not
      specified, then the label becomes the name of the corresponding variable. If 
      \code{xy.ticks} is FALSE, then no label is displayed. If no y variable is specified, 
      then \code{xlab} is set to Index unless \code{xlab} has been specified.}
  \item{ylab}{Label for y-axis. If not specified, then the label becomes the name of
      the corresponding variable. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{pch}{The standard plot character, with values defined in help(points). The
       default value is 21, a circle with both a border and filled area, specified here as
       \code{col.point} and \code{col.fill}.  For a scatterplot, \code{col.fill} defaults to
       transparent.  For a run chart, \code{col.fill} defaults to \code{col.point}.}
  \item{cex}{Magnification factor for any displayed points, with default of cex=1.0.}
  \item{center.line}{Plots a dashed line through the middle of a run chart.  The two
      possible values are \code{"mean"} and \code{"median"}.  Provides a centerline for the
     \code{"median"} by default when the values randomly vary about the mean.}
  \item{kind}{Default is \code{"regular"}.  If Likert style response data is plotted, that is, 
      each variable has less than 10 integer values, then by default a bubble plot is plotted
      with the corresponding joint frequency determining the size of the bubble.  A sunflower
      plot can also be requested.}
  \item{x.start}{For Likert style response data, the starting integer value of each axis. Useful
      if the actual data do not include all possible values.}
  \item{x.end}{For Likert style response data, the ending integer value of each axis. Useful
      if the actual data do not include all possible values.}
  \item{size}{Size of the bubbles in a bubble plot of Likert style data.}
  \item{fit.line}{The best fitting line.  Default value is \code{"none"}, with options for 
      \code{"lowess"} and \code{"ls"}.}
  \item{col.fit.line}{Color of the best fitting line, if the \code{fit.line} option is invoked.}
  \item{col.bubble}{Color of the bubbles if a bubble plot of the frequencies is plotted.}
  \item{col.flower}{Color of the flowers if a sunflower plot of the frequencies is plotted.}
  \item{time.start}{Optional starting date for first data value. Format must be 
       "\%Y-\%m-\%d" or "\%Y/\%m/\%d". If using with \code{x.reverse}, the first date is after 
       the data are reverse sorted.}
  \item{time.by}{Accompanies the \code{x.start} specification, the interval to increment the
      date for each sequential data value. A character string, containing one of \code{"day"}, 
      \code{"week"}, \code{"month"} or \code{"year"}. This string can optionally be preceded by 
      a positive or negative integer and a space, or followed by "s", as specified in 
      \code{\link{seq.Date}}.}
  \item{time.reverse}{Reverse the ordering of the dates, particularly when the data are
      listed such that first row of data is the newest. Accompanies the \code{x.start}
      specification.}
  \item{\dots}{Other parameter values for graphics as defined by and then processed 
      by \code{\link{plot}} and \code{\link{par}}, including \code{xlim}, \code{ylim}, \code{lwd}, 
      and \code{cex.lab.}}
}


\details{
Results are based on the standard \code{\link{plot}} and related graphic functions, with the additional provided color capabilities and other options including a center line.  The plotting procedure utilizes ``adaptive graphics'', such that different default values are chosen for different circumstances of the specified plot and data values. The goal is to produce a desired graph from simply relying upon the default values, both of the \code{color.plot} function itself, as well as the base R functions called by \code{color.plot}, such as \code{\link{plot}}. Familiarity with the options permits complete control over the computed defaults, but this familiarity becomes optional if the default values are accepted.

When two variables are specified to plot, if the values of the first variable, \code{x} are unsorted, a scatterplot is produced, that is, a plot with \code{type="p"} for points. By default the points of a scatterplot are not filled. Sorted values of the first of the two specified variables yield a function plot, that is, a plot with \code{type="l"} for a smooth plotted line or curve.

Specifying one variable leads to a run chart, with the values on the horizontal axis automatically generated.  The default is the Index variable, the ordinal position of each data value.  Or, dates on the horizontal axis can be specified from the specified starting date given by \code{x.start} and the accompanying increment as given by \code{x.by}.  If the data values randomly vary about the mean, the default is to plot the mean as the center line of the graph, otherwise the default is to ignore the center line.  The default plot type for the run chart is \code{type="b"}, for both points and the corresponding connected line segments.  The size of the points is automatically reduced according to the number of points of points plotted, and the \code{cex} option can override the computed default.  If the area below the plotted values is specified to be filled in with color, then the default line type changes to \code{type="l"}.

A scatterplot of Likert type data is problematic because there are so few possiblities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 25 possible paired values to plot, so most of the plotted points overlap with others.  In this situation, that is, when there are less than 10 values for each of the two variables, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values.  A sunflower plot can be requested in lieu of the bubble plot.

The default background color of \code{col.bg=ghostwhite} provides a very mild cool tone with a slight emphasis on blue. To provide a warmer tone by slightly enhancing red, try \code{col.bg=snow}. Obtain a very light gray with \code{col.bg=gray99}.  To darken the background gray, try \code{col.bg=gray97} or lower numbers. See the \code{\link{color.show}} function in this package the provides an example of all available named colors.

Commonly used graphical parameters in standard R graphics functions are generally available to \code{color.plot}, such as:

\describe{
\item{pch}{Vector of plotting characters or symbols for the individual points, see \code{\link{points}}.}
\item{lwd}{Line width, see \code{\link{par}}.}
\item{cex}{Numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. This works as a multiple of \code{\link{par}}("cex"). NULL and NA are equivalent to 1.0. Note that this does not affect annotation.}
\item{cex.main, col.lab, font.sub, etc}{Settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}
\item{main}{Title of the graph, see \code{\link{title}}.}
\item{xlim}{The limits of the plot on the x-axis, expressed as c(x1,x2), where x1 and x2 are the limits. Note that x1 > x2 is allowed and leads to a reversed axis.}
\item{ylim}{The limits of the plot on the y-axis.}
}

}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{title}}, \code{\link{par}}, \code{\link{cor.test}}.
}

\examples{
# scatter plot
x <- rnorm(25)
y <- rnorm(25)
# default scatterplot, x is not sorted so type is set to "p"
color.plot(x, y)
# scatterplot, with lowess line added
color.plot(x, y, fit.line="lowess")
# custom scatter plot
color.plot(x, y, col.point="darkred", col.fill="plum")

# function curve
x <- seq(10,500,by=1) 
y <- 18/sqrt(x)
# default function plot,  x is sorted so type set to "l"
color.plot(x, y)
# custom function plot
color.plot(x, y, ylab="My Y", , xlab="My X", col.line="blue", 
  col.bg="snow", col.area="slategray2", col.grid="lightsalmon")
  
# generate data randomly varying about a constant mean
y <- rnorm(25)
# default run chart
color.plot(y)
# customize run chart, pch=24: filled triangle point-up,
color.plot(y, lwd=2, col.point="sienna3", pch=24, 
  col.bg="mintcream", ylim=c(-3.5,3.5), center.line="median")
  
# generate steadily increasing values
y <- sort(rexp(50))
# default run chart
color.plot(y)
# run chart with border around plotted values
color.plot(y, col.area="transparent")
# time series chart, i.e., with dates, and filled area
# with option label for the x-axis
color.plot(y, time.start="2005/09/01", time.by="month")

# plot Likert data, 1 to 7 scale, and get bubble plot
# size of each plotted point depends on the frequency
x1 <- sample(1:7, size=1000, replace=TRUE)
x2 <- sample(1:7, size=1000, replace=TRUE)
color.plot(x1,x2)
# plot Likert data and get sunflower plot with lowess line
color.plot(x1,x2, kind="sunflower.freq", fit.line="lowess")

# modern art
n <- sample(2:30, size=1)
x <- rnorm(n)
y <- rnorm(n)
clr <- colors()
color1 <- clr[sample(1:length(clr), size=1)]
color2 <- clr[sample(1:length(clr), size=1)]
color.plot(x, y, type="l", lty="dashed", lwd=3, col.area=color1, 
   col.line=color2, xy.ticks=FALSE, main="Modern Art", 
   cex.main=2, col.main="lightsteelblue")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }


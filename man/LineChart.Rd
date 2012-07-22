\name{LineChart}
\alias{LineChart}
\alias{lc}

\title{Run Chart or Time-Series Chart}

\description{
Abbreviation: \code{rc}

Plots a line chart, the values of the variable ordered according to their order in the data frame.  Usually this ordering would be an ordering according to time, which yields a run chart. The default run chart provides the index, that is, sequential position, of each value of the variable from 1 to the last value.  Optionally dates can be provided so that a time-series plot is produced. 

For data of one variable exhibiting little trend, the center line is provided for the generation of a run chart, plotting the values of a variable in order of occurrence over time.  When the center line, the median by default, is plotted, the analyses of the number and composition of the individual runs, number of consecutive values above or below the center line, is also displayed.  Also, the defaults change for each of the types of plots.  The intent is to rely on the default values for a relatively sophisticated plot, particularly when compared to the default values of the standard R \code{\link{plot}} function called with a single variable.

}

\usage{
LineChart(y, dframe=mydata, type=NULL, 

         col.line=NULL, col.area=NULL, col.box="black",
         col.pts=NULL, col.fill=NULL, trans.pts=getOption("trans.pts"),
         shape.pts=21, col.grid=NULL, col.bg=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"),

         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         center.line=c("default", "mean", "median", "off"), text.out=TRUE, 

         pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

lc(\ldots)
}

\arguments{
  \item{y}{Variable for which to construct the line chart, values on the vertical axis.}
  \item{dframe}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{type}{Character string that indicates the type of plot, either \code{"p"} for 
        points, \code{"l"} for line, or \code{"b"} for both.  If x and y are provided and 
        x is sorted so that a function is plotted, the default is \code{"l"}, or, when x 
        is not sorted, the default is \code{"p"} for point, yielding a scatter plot.  If
        only the values of x are provided, then a run chart is produced with the default 
        type of \code{"b"} for both points and lines.}
  \item{col.line}{Color of any plotted line segments, with a default of \code{"darkblue"}.}
  \item{col.area}{Color of area under the plotted line segments.  To have a border
        at the bottom and right of a run chart but retain the property of no area color, 
        specify a color of \code{"transparent"}. If the values exhibit a trend and dates 
        are specified with \code{x.start}, the default area color is \code{slategray3}, 
        otherwise there is no default color. }
  \item{col.box}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}
  \item{col.pts}{Color of the border of the plotted points.}
  \item{col.fill}{For plotted points, the interior color of the point.  For a
        scatterplot the default value is transparent.  For a run chart the default value is 
        the color of the point's border, \code{col.pts}.}
  \item{trans.pts}{Transparency of the plotted points, from opaque at 0 to perfectly 
        transparent at 1. Default is 0.6}
  \item{shape.pts}{The standard plot character, with values defined in help(points). The
       default value is 21, a circle with both a border and filled area, specified here as
       \code{col.pts} and \code{col.fill}.  For a scatterplot, \code{col.fill} defaults to
       transparent.  For a run chart, \code{col.fill} defaults to \code{col.pts}.}
  \item{col.grid}{Color of the grid lines, with a default of \code{"grey90"}.}
  \item{col.bg}{Color of the plot background.}
  \item{colors}{Sets the color palette.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis values to be 
        smaller than the axis labels.}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{col.ticks}{Color of the ticks used to label the axis values.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
        axes are to be displayed.}
  \item{xlab}{Label for x-axis. For two variables specified, x and y, if \code{xlab} not
       specified, then the label becomes the name of the corresponding variable. If 
       \code{xy.ticks} is \code{FALSE}, then no label is displayed. If no y variable is specified, 
       then \code{xlab} is set to Index unless \code{xlab} has been specified.}
  \item{ylab}{Label for y-axis. If not specified, then the label becomes the name of
      the corresponding variable. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable labels exist 
      in the data frame \code{mylabels}, then the title is set by default from the corresponding 
      variable labels.}
  \item{cex}{Magnification factor for any displayed points, with default of cex=1.0.}
  \item{x.start}{For Likert style response data, the starting integer value of the x-axis. Useful
      if the actual data do not include all possible values.}
  \item{x.end}{For Likert style response data, the ending integer value of the x-axis. Useful
      if the actual data do not include all possible values.}
  \item{y.start}{For Likert style response data, the starting integer value of the y-axis. Useful
      if the actual data do not include all possible values.}
  \item{y.end}{For Likert style response data, the ending integer value of the y-axis. Useful
      if the actual data do not include all possible values.}
  \item{time.start}{Optional starting date for first data value. Format must be 
       "\%Y-\%m-\%d" or "\%Y/\%m/\%d". If using with \code{x.reverse}, the first date is after 
       the data are reverse sorted. Not needed if data are a time series with
       \code{\link{ts}} function.}
  \item{time.by}{Accompanies the \code{time.start} specification, the interval to increment the
      date for each sequential data value. A character string, containing one of \code{"day"}, 
      \code{"week"}, \code{"month"} or \code{"year"}. This string can optionally be preceded by 
      a positive or negative integer and a space, or followed by "s", as specified in 
      \code{\link{seq.Date}}. Not needed if data are a time series.}
  \item{time.reverse}{When \code{TRUE}, reverse the ordering of the dates, particularly when the 
      data are listed such that first row of data is the newest. Accompanies the \code{time.start}
      specification.}
  \item{center.line}{Plots a dashed line through the middle of a run chart.  The two
      possible values for the line are \code{"mean"} and \code{"median"}.  Provides a centerline
      for the \code{"median"} by default when the values randomly vary about the mean.}
  \item{text.out}{If \code{TRUE}, then display text output in console.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameters such as from \code{\link{par}}.}
}


\details{
OVERVIEW\cr
The line chart is based on the standard R function \code{\link{plot}} when called with only a single variable.

The values on the horizontal axis of the line chart are automatically generated.  The default is the index variable, the ordinal position of each data value, in which case this version of the line chart is a run chart.  Or, dates on the horizontal axis can be specified from the specified starting date given by \code{x.start} and the accompanying increment as given by \code{x.by}, in which case the line chart is typically referred to as a time series chart.  

If the data values randomly vary about the mean, the default is to plot the mean as the center line of the graph, otherwise the default is to ignore the center line.  The default plot type for the line chart is \code{type="b"}, for both points and the corresponding connected line segments.  The size of the points is automatically reduced according to the number of points of points plotted, and the \code{cex} option can override the computed default.  If the area below the plotted values is specified to be filled in with color, then the default line type changes to \code{type="l"}.

DATA\cr
If the variable is in a data frame, the input data frame has the assumed name of \code{mydata}.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the \code{mydata$name} notation.

COLOR THEME\cr
Individual colors in the plot can be manipulated with options such as \code{col.pts} for the color of the plotted points. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option. Or, the color theme can be changed for all subsequent graphical analysis with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in a data frame called \code{mylabels}, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > LineChart(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > LineChart(Y)     # directly reference Y}
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{set}}. 
}


\examples{
# generate 25 random normal data values
y <- rnorm(25)

# default run chart
LineChart(y)
# short name
lc(y)
# compare to standard R plot
plot(y, type="l")

# save run chart to a pdf file
LineChart(y, pdf.file="MyLineChart.pdf")

# LineChart in gray scale
LineChart(y, colors="gray")
  
# customize run chart, pch=24: filled triangle point-up,
LineChart(y, lwd=2, col.pts="sienna3", shape.pts=24, 
  col.bg="mintcream", ylim=c(-3.5,3.5), center.line="median")
  
# generate steadily increasing values
y <- sort(rexp(50))
# default line chart
LineChart(y)
# line chart with border around plotted values
LineChart(y, col.area="transparent")
# time series chart, i.e., with dates, and filled area
# with option label for the x-axis
LineChart(y, time.start="2005/09/01", time.by="month")
# time series chart from a time series object
y.ts <- ts(y, start=c(2005, 9), frequency=12)
LineChart(y.ts)

# LineChart with built-in data set
LineChart(breaks, dframe=warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ line chart }
\keyword{ run chart }
\keyword{ time series chart }


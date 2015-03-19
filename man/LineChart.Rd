\name{LineChart}
\alias{LineChart}
\alias{lc}

\title{Line Chart such as a Run Chart or Time-Series Chart}

\description{
Abbreviation: \code{lc}

Plots a line chart, the values of the variable ordered according to their order in the data frame.  Usually this ordering would be an ordering according to time, which yields a run chart. The default run chart provides the index, that is, sequential position, of each value of the variable from 1 to the last value.  Optionally dates can be provided so that a time-series plot is produced. 

For data of one variable exhibiting little trend, the center line is provided for the generation of a run chart, plotting the values of a variable in order of occurrence over time.  When the center line, the median by default, is plotted, the analyses of the number and composition of the individual runs, number of consecutive values above or below the center line, is also displayed.  Also, the defaults change for each of the types of plots.  The intent is to rely on the default values for a relatively sophisticated plot, particularly when compared to the default values of the standard R \code{\link{plot}} function called with a single variable.

If the provided object to analyze is a set of multiple variables, including an entire data frame, then each non-numeric variable in the data frame is analyzed and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.
}

\usage{
LineChart(x, data=mydata, n.cat=getOption("n.cat"), type=NULL, 

         col.fill=getOption("col.fill.bar"), 
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         col.line=getOption("col.stroke.pt"),

         col.area=NULL, col.box="black",

         shape.pts=21, cex.axis=.85, col.axis="gray30",
         xy.ticks=TRUE, line.width=1.1,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,

         time.start=NULL, time.by=NULL, time.reverse=FALSE,

         center.line=c("default", "mean", "median", "zero", "off"),

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

lc(\ldots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single numerical variable, 
        either within a data frame or as a vector in the user's workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all numerical variables in the specified data
        frame, \code{mydata} by default.}
  \item{data}{Optional data frame that contains the variable(s) of interest,
        default is \code{mydata}.}
  \item{n.cat}{For the analysis of multiple variables, such as a data frame,
        specifies the largest number of unique values of variable of a numeric data type
        for which the variable will be analyzed as a categorical. Set to 0 to turn off.}

  \item{type}{Character string that indicates the type of plot, either \code{"p"} for 
        points, \code{"l"} for line, or \code{"b"} for both. The default is 
        \code{"b"} for both points and lines.}
  \item{col.fill}{For plotted points, the interior color of the point.  For a
        scatterplot the default value is transparent.  For a run chart the default value is 
        the color of the point's border, \code{col.stroke}.}
  \item{col.stroke}{Color of the border of the plotted points.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines, with a default of \code{"grey90"}.}
  \item{col.line}{Color of any plotted line segments, with a default of \code{"darkblue"}.}
  \item{col.area}{Color of area under the plotted line segments.  To have a border
        at the bottom and right of a run chart but retain the property of no area color, 
        specify a color of \code{"transparent"}. If the values exhibit a trend and dates 
        are specified with \code{x.start}, the default area color is \code{slategray3}, 
        otherwise there is no default color. }
  \item{col.box}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}
  \item{shape.pts}{The standard plot character, with values defined in help(points). The
       default value is 21, a circle with both a border and filled area, specified here as
       \code{col.stroke} and \code{col.fill}.    \code{col.fill} defaults to
       \code{col.stroke}.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis values to be 
        smaller than the axis labels.}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
        axes are to be displayed.}
  \item{line.width}{Width of the density lines.}
  \item{xlab}{Label for x-axis. For two variables specified, x and y, if \code{xlab} not
       specified, then the label becomes the name of the corresponding variable. If 
       \code{xy.ticks} is \code{FALSE}, then no label is displayed. If no y variable is specified, 
       then \code{xlab} is set to Index unless \code{xlab} has been specified.}
  \item{ylab}{Label for y-axis. If not specified, then the label becomes the name of
      the corresponding variable. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable labels exist,
      then the title is set by default from the corresponding variable labels.}
  \item{cex}{Magnification factor for any displayed points, with default of cex=1.0.}
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
      for the \code{"median"} by default when the values randomly vary about the mean. A
      value of \code{"zero"} specifies the center line should go through zero.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{set}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected. If there is
       no filetype of \code{.pdf}, the filetype is added to the name.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameters such as from \code{\link{par}}, and col.ticks to set the color of the ticks used to label the axis values.}
}


\details{
OVERVIEW\cr
The line chart is based on the standard R function \code{\link{plot}} when called with only a single variable.

The values on the horizontal axis of the line chart are automatically generated.  The default is the index variable, the ordinal position of each data value, in which case this version of the line chart is a run chart.  Or, dates on the horizontal axis can be specified from the specified starting date given by \code{x.start} and the accompanying increment as given by \code{x.by}, in which case the line chart is typically referred to as a time series chart.  

If the data values randomly vary about the mean, the default is to plot the mean as the center line of the graph, otherwise the default is to ignore the center line.  The default plot type for the line chart is \code{type="b"}, for both points and the corresponding connected line segments.  The size of the points is automatically reduced according to the number of points of points plotted, and the \code{cex} option can override the computed default.  If the area below the plotted values is specified to be filled in with color, then the default line type changes to \code{type="l"}.

DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{mydata}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{col.stroke} for the color of the border of the plotted points. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{set}}. The default color theme is \code{dodgerblue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}, such as \code{"red"} and \code{"green"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors.  

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} does, obtained from the \code{\link{Read}} function.  If the variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

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
# create data frame, mydata, to mimic reading data with Read function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(50), rnorm(50), rnorm(50), rep(c("A","B"),25))
names(mydata) <- c("X","Y","Z","C")

# default run chart
LineChart(Y)
# short name
lc(Y)
# compare to standard R plot
plot(mydata$Y, type="l")

# save run chart to a pdf file
LineChart(Y, pdf.file="MyLineChart.pdf")

# LineChart in gray scale, then back to default "dodgerblue"
set(colors="gray")
LineChart(Y)
set(colors="dodgerblue")
  
# customize run chart with LineChart options
LineChart(Y, line.width=2, col.stroke="sienna3",
  col.area="slategray3", col.bg="mintcream",
  center.line="median")

# customize run chart with R par parameters
# 24 is the R value for a half-triangle pointing up
lc(Y, xlab="My xaxis", ylab="My yaxis", main="My Best Title",
   cex.axis=.7, cex.lab=1.2, cex.main=1.5, col.lab="red", font.main=3,
   ylim=c(-4,4), shape.pts=24)
  
# generate steadily increasing values
Y <- sort(rexp(50))
# default line chart
LineChart(Y)
# line chart with border around plotted values
LineChart(Y, col.area="transparent")
# time series chart, i.e., with dates, and filled area
# with option label for the x-axis
LineChart(Y, time.start="2000/09/01", time.by="3 months")
# time series chart from a time series object
y.ts <- ts(Y, start=c(2000, 9), frequency=4)
LineChart(y.ts)

# LineChart with built-in data set
LineChart(breaks, data=warpbreaks)

# Line charts for all numeric variables in a data frame
LineChart()

# Line charts for all specified numeric variables in a list of variables
# e.g., use the combine or c function to specify a list of variables
LineChart(c(X,Y))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ line chart }
\keyword{ run chart }
\keyword{ time series chart }

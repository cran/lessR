\name{LineChart}

\alias{LineChart}
\alias{lc}

\title{Line Chart such as a Run Chart or Time-Series Chart}

\description{
Abbreviation: \code{lc}

Plots a line chart, the values of the variable ordered according to their order in the data frame.  Usually this ordering would be an ordering according to time, which yields a run chart. The default run chart provides the index, that is, sequential position, of each value of the variable from 1 to the last value.  Optionally dates can be provided so that a time-series plot is produced. 

For data of one variable exhibiting little trend, the center line is provided for the generation of a run chart, plotting the values of a variable in order of occurrence over time_  When the center line, the median by default, is plotted, the analyses of the number and composition of the individual runs, number of consecutive values above or below the center line, is also displayed.  Also, the defaults change for each of the types of plots.  The intent is to rely on the default values for a relatively sophisticated plot, particularly when compared to the default values of the standard R \code{\link{plot}} function called with a single variable.

If the provided object to analyze is a set of multiple variables, including an entire data frame, then each non-numeric variable in the data frame is analyzed and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.
}

\usage{
LineChart(x, data=d, rows=NULL,
          n_cat=getOption("n_cat"), type=NULL, 

         line_color=getOption("pt_color"), area=NULL, 

         shape_pts=21, lab_cex=1.0, axis_cex=0.75,
         axis_text_color=getOption("axis_x_text_color"),

         rotate_x=0, rotate_y=0, offset=.5,

         xy_ticks=TRUE, line_width=1,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL, cex=NULL,

         time_start=NULL, time_by=NULL, time_reverse=FALSE,

         center_line=c("default", "mean", "median", "zero", "off"),

         show_runs=FALSE, eval_df=NULL, quiet=getOption("quiet"),
         width=6, height=6, pdf=FALSE,
         \dots)

lc(\dots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single numerical variable, 
        either within a data frame or as a vector in the user's workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all numerical variables in the specified data
        frame, \code{d} by default.}
  \item{data}{Optional data frame that contains the variable(s) of interest,
        default is \code{d}.}
  \item{rows}{A logical expression that specifies a subset of rows of the data frame
       to analyze.}
  \item{n_cat}{For the analysis of multiple variables, such as a data frame,
        specifies the largest number of unique values of variable of a numeric
        data type for which the variable will be analyzed as a categorical.
        Default is 0.}
  \item{type}{Character string that indicates the type of plot, either \code{"p"} for 
        points, \code{"l"} for line, or \code{"b"} for both. The default is 
        \code{"b"} for both points and lines.}\cr

  \item{line_color}{Color of the plotted line.}
  \item{area}{Color of area under the plotted line segments, which by default is not
        applied, equivalent to a color of \code{"transparent"}.}

  \item{shape_pts}{The standard plot character, with values defined in help(points). The
       default value is 21, a circle with both a border and filled area, specified here as
       \code{color} and \code{fill}.    \code{fill} defaults to
       \code{color}.}
  \item{lab_cex}{Scale magnification factor for axis labels.}
  \item{axis_cex}{Scale magnification factor, which by defaults displays the axis values to be 
        smaller than the axis labels.}
  \item{axis_text_color}{Color of the font used to label the axis values_}
  \item{rotate_x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate_y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The amount of spacing between the axis values and the axis_ Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}
  \item{xy_ticks}{Flag that indicates if tick marks and associated values on the 
        axes are to be displayed.}
  \item{line_width}{Width of the line segments_}
  \item{xlab}{Label for x-axis_ For two variables specified, x and y, if \code{xlab} not
       specified, then the label becomes the name of the corresponding variable. If 
       \code{xy_ticks} is \code{FALSE}, then no label is displayed. If no y variable is specified, 
       then \code{xlab} is set to Index unless \code{xlab} has been specified.}
  \item{ylab}{Label for y-axis_ If not specified, then the label becomes the name of
      the corresponding variable. If \code{xy_ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable labels exist,
      then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below xlab_}
  \item{cex}{Magnification factor for any displayed points, with default of cex=1.0.}
  \item{time_start}{Optional starting date for first data value. Format must be 
       "\%Y-\%m-\%d" or "\%Y/\%m/\%d". If using with \code{x.reverse}, the first date is after 
       the data are reverse sorted. Not needed if data are a time series with
       \code{\link{ts}} function.}
  \item{time_by}{Accompanies the \code{time_start} specification, the interval to increment the
      date for each sequential data value. A character string, containing one of \code{"day"}, 
      \code{"week"}, \code{"month"} or \code{"year"}. This string can optionally be preceded by 
      a positive or negative integer and a space, or followed by "s", as specified in 
      \code{\link{seq.Date}}. Not needed if data are a time series.}
  \item{time_reverse}{When \code{TRUE}, reverse the ordering of the dates, particularly when the 
      data are listed such that first row of data is the newest. Accompanies the \code{time_start}
      specification.}
  \item{center_line}{Plots a dashed line through the middle of a run chart.  The two
      possible values for the line are \code{"mean"} and \code{"median"}.  Provides a centerline
      for the \code{"median"} by default when the values randomly vary about the mean. A
      value of \code{"zero"} specifies the center line should go through zero.}
  \item{show_runs}{If \code{TRUE}, display the individual runs in the run analysis.}
  \item{eval_df}{Determines if to check for existing data frame and
        specified variables. By default is \code{TRUE} 
        unless the \code{shiny} package is loaded then set to \code{FALSE} so
        that Shiny will run. Needs to be set to \code{FALSE} if using
        the pipe \code{\%\>\%} notation.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf}{If \code{TRUE}, the pdf file is to be redirected to a pdf file.}
  \item{\dots}{Other parameters such as from \code{\link{par}}, \code{col.lab},
        \code{sub}, \code{color_sub}, \code{color_ticks} to set the color of the
        ticks used to label the axis values, and
        \code{srt} to rotate the axis value labels.}
}


\details{
OVERVIEW\cr
The line chart is based on the standard R function \code{\link{plot}} when called with only a single variable.

The values on the horizontal axis of the line chart are automatically generated.  The default is the index variable, the ordinal position of each data value, in which case this version of the line chart is a run chart.  Or, dates on the horizontal axis can be specified from the specified starting date given by \code{x.start} and the accompanying increment as given by \code{x.by}, in which case the line chart is typically referred to as a time series chart.  

If the data values randomly vary about the mean, the default is to plot the mean as the center line of the graph, otherwise the default is to ignore the center line.  The default plot type for the line chart is \code{type="b"}, for both points and the corresponding connected line segments_  The size of the points is automatically reduced according to the number of points of points plotted, and the \code{cex} option can override the computed default.  If the area below the plotted values is specified to be filled in with color, then the default line type changes to \code{type="l"}.

DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{d}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{d$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.
       
The \code{rows} parameter subsets rows (cases) of the input data frame according to a logical expression. Use the standard R operators for logical statements as described in \code{\link{Logic}} such as \code{&} for and, \code{|} for or and \code{!} for not, and use the standard R relational operators as described in \code{\link{Comparison}} such as \code{==} for logical equality \code{!=} for not equals, and \code{>} for greater than.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{color}. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{style}}. The default color theme is \code{dodgerblue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{style}}, such as \code{"red"} and \code{"green"}. Use the option \code{style(sub_theme="black")} for a black background and partial transparency of plotted colors.  

For the color options, such as \code{grid_color}, the value of \code{"off"} is the same as \code{"transparent"}.

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} does, obtained from the \code{\link{Read}} function.  If the variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{d}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > LineChart(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > LineChart(Y)     # directly reference Y}
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{style}}. 
}


\examples{
# create data frame, d, to mimic reading data with Read function
# d contains both numeric and non-numeric data
d <- data.frame(rnorm(50), rnorm(50), rnorm(50), rep(c("A","B"),25))
names(d) <- c("X","Y","Z","C")

# default run chart
LineChart(Y)
# short name
lc(Y)

# save run chart to a pdf file
#LineChart(Y, pdf=TRUE)

# LineChart in gray scale, then back to default theme
style("gray")
LineChart(Y)
style()
  
# customize run chart with LineChart options
style(panel_fill="mintcream", color="sienna3")
LineChart(Y, line_width=2, area="slategray3", center_line="median")
style()  # reset style

# customize run chart with R par parameters
# 24 is the R value for a half-triangle pointing up
lc(Y, xlab="My xaxis", ylab="My yaxis", main="My Best Title",
   cex.main=1.5, font.main=3, ylim=c(-4,4), shape_pts=24)
  
# generate steadily increasing values
# get a variable named A in the user workspace
A <- sort(rexp(50))
# default line chart
LineChart(A)
# line chart with border around plotted values
LineChart(A, area="off")
# time series chart, i.e., with dates, and filled area
# with option label for the x-axis
LineChart(A, time_start="2000/09/01", time_by="3 months")
# time series chart from a time series object
y.ts <- ts(A, start=c(2000, 9), frequency=4)
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

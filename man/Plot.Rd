\name{Plot}
\alias{Plot}
\alias{ScatterPlot}
\alias{sp}

\title{Plot One or Two Continuous and/or Categorical Variables}

\description{
Abbreviation: \code{sp}, \code{ScatterPlot}

From the identical syntax, for variables \code{X} and \code{Y}, \code{Plot(X)} or \code{Plot(X,Y)}, by default generates a family of related 1- or 2-variable scatterplots, broadly defined, and related statistical analyses, which result from any combination of continuous or categorical variables: the traditional scatterplot of two continuous variables, a bubble (balloon) scatterplot from two categorical variables, a scatterplot with means at each level of a categorical variable paired with a continuous variable, and a Cleveland dot plot as a scatterplot that pairs a continuous variable with a each unique value of an ID-variable. Summarize univariate distributions with either a 1-dimensional scatterplot of a continuous variable, or with a 1-dimensional bubble plot for a categorical variable as a more compact replacement of the traditional bar chart. From the specification of multiple categorical x-variables, generalize the later to a matrix of 1-dimensional bubble plots here called the bubble plot frequency matrix. Have \code{X} be an R time series variable for a time series chart, or set \code{line.chart} to \code{TRUE} to generate a run chart.

For multiple plots on the same graph, specify a vector of x-variables or y-variables such as \code{Plot(c(X1,X2),Y)} for variables X1 and X2 plotted against Y. Represent the influence of a third variable with the use of \code{by} for a categorical variable or \code{size} for a continuous variable, including the option to display the corresponding value of size for each of the bubbles. By default \code{values} of analysis that are plotted in the coordinate system is \code{data}, or choose other values to plot, which are statistics computed from the data such as the \code{mean}. 
}

\usage{
Plot(x, y=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"),

         values=c("data", "count", "prop", "sum", "mean", "sd", "min", 
                  "median", "max"),

         fill=getOption("fill.pt"),
         stroke=getOption("stroke.pt"),
         bg=getOption("bg"),
         grid=getOption("grid"),
         box=getOption("box"),
         segment=getOption("fill.pt"),
         color=NULL, trans=NULL,

         cex.axis=0.76, axes="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, label.max=20,
         rotate.values=0, offset=0.5, proportion=FALSE,

         size=NULL, shape="circle", means=TRUE,
         sort.yx=FALSE,
         segments.y=FALSE, segments.x=FALSE,

         bubble.scale=0.25, bubble.power=0.6, bubble.text=NULL,
         low.color=NULL, hi.color=NULL,

         smooth=FALSE, smooth.points=100, smooth.trans=0.25,
         smooth.bins=128,

         fit=NULL, stroke.fit=getOption("stroke.bar"),
         se.fit=0,

         ellipse=FALSE, stroke.ellipse=getOption("stroke.pt"),
         fill.ellipse=getOption("fill.ellipse"),

         method="overplot", pt.reg="circle", pt.out="circle",
         out30="firebrick2", out15="firebrick4", new=TRUE,
         boxplot=FALSE,

         line.chart=FALSE, line.width=2, area=FALSE, 
         center.line=c("default", "mean", "median", "zero", "off"),
         show.runs=FALSE, stack=FALSE,

         breaks="Sturges", bin.start=NULL, bin.width=NULL, bin.end=NULL,
         cumul=FALSE,

         digits.d=NULL, quiet=getOption("quiet"),
         width=NULL, height=NULL, pdf.file=NULL, 
         fun.call=NULL, \ldots)

ScatterPlot(\ldots)

sp(\ldots)
}

\arguments{
  \item{x}{If both x and y are specified, then the x-values are the coordinates
        plotted on the horizontal axis. If x is sorted with equal intervals
        separating the values, or is a time series, then the default is to
        join the points with line segments. Specify multiple x-variables or
         multiple y-variables, but not both.}
  \item{y}{Coordinates of points in the plot on the vertical axis.} 
  \item{by}{An optional grouping variable such that the points of all (x,y) pairs are
        plotted in the same plotting symbol and/or same color, with a different symbol
        or symbol and/or color for each group.}
  \item{data}{Optional data frame that contains one or both of the variables of
        interest, default is \code{mydata}.}
  \item{n.cat}{Specifies the largest number of unique values of variable of a
        numeric data type for which the variable will be analyzed as categorical
        so as to generate a bubble plot. Set to 0 to turn off.}\cr

  \item{values}{The plotted values according to their coordinates, data values
        by default. For a categorical variable, if only \code{x} is specified,
        then the statistics \code{"count"} and \code{"prop"} can be specified
        for the categories.
        If there is a second variable, \code{y}, which is continuous, then \code{x}
        for either a categorical variable, or a continuous variable
        with values binned into categories, then can apply \code{"mean"}.}\cr

  \item{fill}{For plotted points, the interior color of the points. By default, is
       a partially transparent version of the border color, \code{stroke}. 
       If y-values are unique, as in a Cleveland dot plot, then no transparency by 
       default as there can be no over-plotting. Remove with \code{fill="off"}.}
  \item{stroke}{Border color of the plotted points. If there is a \code{by} variable,
       specified as a vector, one value for each level of \code{by}. Remove with
       \code{stroke="off"}.}
  \item{bg}{Color of the plot background. Remove with \code{bg="off"}.}
  \item{grid}{Color of the grid lines, a value of "on" restores the color from
        the current \code{theme} if turned off by default as with a Cleveland dot
        plot. Remove with \code{grid="off"}.}
  \item{box}{Color of border around the plot background, the box, that encloses 
        the plot. Remove with \code{box="off"}.}
  \item{segment}{Color of connecting line segments, such as in a frequency
        polygon. Default color is \code{stroke}.  Remove with \code{segment="off"}.}
  \item{color}{Simultaneously specifies both \code{stroke} and \code{fill}, and
       takes precedence over their individually specified values.}
  \item{trans}{Transparency level from 0 (none) to 1 (complete). For plotting data
        values, transparency is 0.5 to allow for overlap of plotted points,
        otherwise set at 0.}\cr

  \item{cex.axis}{Scale magnification factor of the values on the axes.}
  \item{axes}{Color of the font used to label the axis values.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
        axes are to be displayed.}
  \item{xlab}{Label for x-axis. If \code{xlab} is not specified, then the label becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy.ticks} is \code{FALSE}, then no label is displayed.
       If no y variable is specified, then \code{xlab} is set to Index unless \code{xlab}
       has been specified.}
  \item{ylab}{Label for y-axis.  If \code{xlab} is not specified, then the label becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below xlab.}
  \item{value.labels}{Labels for the x-axis on the graph to override 
        existing data values, including factor levels. If the variable is a 
        factor and \code{value.labels} is not specified (is \code{NULL}), then the
        value.labels are set to the factor levels with each space replaced by
        a new line character. If x and y-axes have the same scale, they also apply
        to the y-axis.}
  \item{label.max}{Maximum size of labels for the values of a categorical variable.
        Not a literal maximum as preserving unique values may require a larger number
        of characters than specified.}
  \item{rotate.values}{Degrees that the axis values are rotated, usually to accommodate
        longer values, typically used in conjunction with \code{offset}.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}
  \item{proportion}{Specify proportions, relative frequencies, instead of counts.
        For a two variable bar chart, if \code{TRUE} then to facilitate group
        comparisons, displays the proportion of data values by fill variable within
        each group.}\cr
       
  \item{size}{When set to a constant, the scaling factor for standard points
      (not bubbles) or a line, with default of 1.0 for points and 2.0 for a line.
       Set to 0 to not plot the points or lines. When expressed as a variable in
       which case a bubble plot is activated with the size of each bubble determined
       by the value of \code{bubble.scale}.}
  \item{shape}{The plot character(s). The default value is a circle with both a border
       and filled area, specified with \code{stroke} and \code{fill}.
       Possible values are \code{circle}, \code{square}, \code{diamond},
       \code{triup} (triangle up), \code{tridown} (triangle down), all
       uppercase and lowercase letters, all digits, and most punctuation characters.
       The numbers 21 through 25 as defined by the R \code{\link{points}} function
       also apply. If plotting levels according to \code{by}, then list one shape for 
       each level to be plotted.}
  \item{means}{If the first variable is a factor and the other variable continuous,
       then if \code{TRUE}, by default, plot means with the scatterplot.}
  \item{sort.yx}{Sort the values of y by the values of x, such as for a
        Cleveland dot plot, that is, a numeric x-variable paired with a categorical
        y-variable with unique values. If two x-variables, sort by their difference.}
  \item{segments.y}{For one x-variable, draw line segments from y-axis to plotted point,
        such as for the Cleveland dot plot. For two x-variables, the line segments
        connect the two points.}
  \item{segments.x}{Draw line segments from the x-axis to plotted point.}\cr

  \item{bubble.scale}{Scaling factor of the bubbles in a bubble plot, which
        sets the radius of the largest displayed bubble in inches, with default of
        0.25 inches. Compare to \code{size} for the scaling of regular plotted points
        when set to a constant.}
  \item{bubble.power}{Relative size of the scaling of the bubbles to each other.
        Value of 0.5 scales the bubbles so that the area of each bubble is the value of
        the corresponding sizing variable. Value of 1 scales so the radius of the bubble 
        is the value of the sizing variable, increasing the discrepancy of size
        between the variables. The default value is 0.6.}
  \item{bubble.text}{If \code{TRUE} (or \code{1}), then for a bubble plot, the value
        of the sizing variable for a bubble is displayed in the center of selected bubbles,
        unless the bubble is too small.  If \code{FALSE}, no text is displayed.
        If a number greater than 1, then the text is displayed only for the
        corresponding quantiles, such as just the max and min for a setting of 2,
        unless the bubble is too small.  If not manually specified, the default value is set
        to \code{TRUE} for a categorical \code{x} variable, and 2 otherwise.}
  \item{low.color}{For a categorical variable and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient beginning with this color.}
  \item{hi.color}{For a categorical variables and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient ending with this color.}\cr
  
  \item{smooth}{2-D kernel density plot for two numerical variables. Turned on by 
        with 2500 or more rows of data.}
  \item{smooth.points}{Number of points superimposed on the density plot in the areas of
        the lowest density to help identify outliers, which controls how dark are the
        smoothed points.}
  \item{smooth.trans}{Exponent of the function that maps the density scale to the
        color scale.}
  \item{smooth.bins}{Number of bins in both directions for the density estimation.}\cr

  \item{fit}{The best fitting line.  Default value is \code{FALSE}, with options for 
      \code{"loess"} and for least squares, indicated by \code{"ls"}. Or, if set to 
      \code{TRUE}, then a loess line.}
  \item{stroke.fit}{Color of the best fitting line, if the \code{fit} option
       is invoked.}
  \item{se.fit}{Number of standard errors to plot around the fit. The default
       value of 0 turns off the standard error plot. Can be a vector to display multiple
       ranges.}\cr

  \item{ellipse}{If \code{TRUE}, enclose a scatterplot of only a single x-variable 
        and a single y-variable with the default .95 data ellipse. Or can specify a single
        numeric value greater than 0 and less than 1, or a vector of levels to plot
        multiple ellipses.}
  \item{stroke.ellipse}{Color of the ellipse. If specified, \code{ellipse} is set to
        \code{TRUE}.}
  \item{fill.ellipse}{If \code{TRUE}, fill the ellipse with \code{stroke.ellipse}.
        Usually specify low opacity in the color specification, as shown in the examples. If
        specified, \code{ellipse} is set to \code{TRUE}.}\cr

  \item{method}{Applies to one variable plots. Default is \code{"overplot"}, but can also
       provide \code{"stack"} to stack the points or
       \code{"jigger"} to scramble the points.}
  \item{pt.reg}{For dot plot, type of regular (non-outlier) point. Default is 21, a
        circle with specified fill.}
  \item{pt.out}{For a 1-D scatterplot, type of point for outliers. Default is 19, a
        filled circle.}
  \item{out30}{For a 1-D scatterplot, color of outliers.}
  \item{out15}{For a 1-D scatterplot, color of potential outliers.}
  \item{new}{If \code{FALSE}, then add the 1-D scatterplot to an existing graph.}
  \item{boxplot}{For a 1-variable scatterplot, superimpose a box plot.}\cr
        
  \item{line.chart}{If set to \code{TRUE}, points are plotted in the sequential order in which
        they occurred in the data table, such as when they are ordered by time of collection.
        By default the points are connected by line
        segments to form a run chart. Set by default when the x-values
        are sorted with equal intervals or a single variable is a time series.}
  \item{line.width}{Width of the line segments. Set to zero to remove the line segments.}       
  \item{area}{Color of the fill area under a curve, the area between the curve
        and the axis. Can also be \code{TRUE}, which sets to the fill color
        for points, or a specific color can be specified. Default is \code{TRUE} if 
        multiple time series are plotted.}
  \item{center.line}{Plots a dashed line through the middle of a run chart.  The two
      possible values for the line are \code{"mean"} and \code{"median"}.  Provides a centerline
      for the \code{"median"} by default when the values randomly vary about the mean. A
      value of \code{"zero"} specifies the center line should go through zero.}
  \item{show.runs}{If \code{TRUE}, display the individual runs in the run analysis. Also sets
      \code{line.chart} to \code{TRUE}.}
  \item{stack}{If \code{TRUE}, multiple time plots are stacked on each other with
       \code{area} set to \code{TRUE} by default.}\cr

  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other options 
       provided by the \code{\link{hist}} function.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{bin.end}{Optional specified value that is within the last bin, so the actual endpoint
        of the last bin may be larger than the specified value.}
  \item{cumul}{Specify a cumulative frequency polygon.}\cr

  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{theme}} function.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5 except for
        1-D scatterplots.}
  \item{pdf.file}{Name of the pdf file to if graphics to be redirected to a pdf file.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{sp}.}\cr

  \item{\ldots}{Other parameter values for graphics as defined by and then processed 
      by standard R functions \code{\link{plot}} and \code{\link{par}}, including\cr
      \code{xlim} and \code{ylim} for setting the range of the x and y-axes\cr
      \code{cex.main} for the size of the title\cr
      \code{cex} for the size of the axis value labels\cr
      \code{cex.lab} for the size of the axis labels\cr
      \code{col.lab} for the color of the axis labels\cr
      \code{lty} for line type, such as \code{"solid"}, \code{"dashed"},
      \code{"dotted"}, \code{"dotdash"}\cr
      \code{sub} and \code{col.sub} for a subtitle and its color\cr
      \code{col.lab} for the color of the axis labels\cr
      \code{axes} to set the color of the axis values\cr
      For one continuous variable, parameters from \code{\link{stripchart}}
  }
}


\details{
OUTPUT\cr
Two (or more) numeric variables by default produces a traditional scatterplot, based on the standard R function \code{\link{plot}} or \code{\link{symbol}}, with an analysis of the correlation coefficient including hypothesis test and confidence interval or cross-tabulation table or set of means and other summary statistics. Two categorical variables, such as for Likert-style analysis, produces a bubble plot, in which the size of each plotted point indicates the corresponding joint frequency, and a corresponding cross-tabulation analysis. This analysis is an alternative to the traditional \code{\link{BarChart}}. A categorical variable paired with a numeric variable yields a scatterplot with the means of each level of the categorical variable also plotted, and the summary statistics of the numeric variable for each level of the categorical variable. More information is obtained to list the categorical first in the function call.  If the values of the first variable are numeric and sorted with equal intervals, then points are connected via line segments.  If there is only one variable, a 1-dimensional scatterplot is produced for a numeric variable, based on the standard R function \code{\link{stripchart}}, and a 1-dimensional bubble plot is produced for a factor, with corresponding statistics.

The value labels for each axis can be over-ridden from their values in the data to user supplied values with the \code{value.labels} option. This option is particularly useful for Likert-style data coded as integers. Then, for example, a 0 in the data can be mapped into a "Strongly Disagree" on the plot. These value labels apply to integer categorical variables, and also to factor variables. To enhance the readability of the labels on the graph, any blanks in a value label translate into a new line in the resulting plot. Blanks are also transformed as such for the labels of factor variables. 

DATA\cr
The default input data frame is \code{mydata}.  Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{mydata$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment.

The data values themselves can be plotted, or for a single variable, counts or proportions can be plotted. For a categorical X variable paired with a continuous variable, means and other statistics can be plotted  at each level of the X variable. If the X is continuous, it is binned first, with the standard \code{\link{Histogram}} binning parameters available, such as \code{bin.width}, to override default values. The \code{values} parameter sets the values to plot, with \code{data} the default. For example, requesting \code{count} as the value to plot for a continuous variable generates the scatterplot with the counts plotted against the binned values. By default the connecting line segments are provided, so a frequency polygon results. Turn off the lines by setting \code{line.width=0}.

CATEGORICAL VARIABLES\cr
Categorical variables have relatively few unique data values. The standard and most general way to define a categorical variable is as an R factor, illustrated in the examples for the \code{\link{Transform}} function. \code{lessR} also provides the option of defining an integer variable with equally spaced values as categorical based on the value of \code{n.cat}, which can be set locally or globally with the \code{\link{theme}} function. For example, for a variable with data values from 5-point Likert scale, a value of \code{n.cat} of 5 will define the define the variable as categorical. The default value is 8. To explicitly analyze the values as numerical, set \code{n.cat} to a value lower than 6, usually 0. Can also annotate a graph of the values of an integer categorical variable with \code{value.labels} option.  

A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 26 possible paired values to plot, so most of the plotted points overlap with others.  In this situation, that is, when a single variable or two variables with Likert response scales are specified, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values. A sunflower plot can be requested in lieu of the bubble plot by setting the \code{shape} to \code{"sunflower"}.

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced, that is, a call to the standard R \code{\link{plot}} function with \code{type="p"} for points. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{type="l"}, which connects each adjacent pair of points with a line segment.

Specifying multiple, continuous x variables against a single y variable, or vice versa, results in multiple plots on the same graph. The color of the points of the second variable is the same as that of the first variable, but with a transparent fill. For more than two x-variables, multiple colors are displayed, one for each x-variable.

BUBBLE PLOT FREQUENCY MATRIX (BPFM)\cr
Multiple categorical variables for \code{x} may be specified in the absence of a \code{y} variable. A bubble plot results that illustrates the frequency of each response for each of the variables in a common figure in which the x-axis contains all of the unique labels for all of the variables plotted. Each line of information, the bubbles and counts for a single variable, replaces the standard bar chart in a more compact display. Usually the most meaningful when each variable in the matrix has the same response categories, that is, levels, such as for a set of shared Likert scales. The BPFM is considerably condensed presentation of frequencies for a set of variables than are the corresponding bar charts.

BY VARIABLE\cr
A variable specified with \code{by=} is a grouping variable that specifies that the plot is produced with the points for each group plotted with a different shape and/or color. By default, the shapes vary by group, and the color of the plot symbol remains the same for the groups. The default shapes, in this order, are \code{"circle"}, \code{"diamond"},  \code{"square"}, \code{"triup"} for a triangle pointed up, and \code{"tridown"} for a triangle pointed down.

SIZE VARIABLE\cr
A variable specified with \code{size=} is a numerical variable that activates a bubble plot in which the size of each bubble is determined by the value of the corresponding value of \code{size}, which can be a variable or a constant.

To explicitly vary the shapes, use \code{shape} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{fill}, such as with R standard color names. If \code{fill} is specified without \code{shape}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable. 

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{ellipse} function, written by Duncan Murdoch and E. D. Chow, from the \code{ellipse} package. The axes are automatically lengthened to provide space for the entire ellipse that extends beyond the maximum and minimum data values. Multiple numerical values of \code{ellipse} may also be specified, to obtain multiple ellipses. 

ONE VARIABLE PLOT\cr
The one variable plot is a 1-dimensional scatterplot, that is, a dot chart. For a numerical variable, results are based on the standard \code{\link{stripchart}} function. Colors are provided by default and can also be specified. For gray scale output, potential outliers are plotted with squares and actual outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The definition of outliers are from the R \code{\link{boxplot}} function.  The plot can also be obtained as a bubble plot for a categorical variable.

RUN CHART\cr
Specifying one or more x-variables with no y-variables, and \code{line.chart=TRUE}  plots the x-variables in a run chart, with Index on the x-axis. Index is the ordinal position of each data value, from 1 to the number of values. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function or \code{\link{VariableLabels}}.  If variable labels exist, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output. 

2-D KERNEL DENSITY\cr
With \code{smooth=TRUE}, the R function \code{\link{smoothScatter}} is invoked according to the current color theme. Useful for very large data sets. The \code{smooth.points} parameter plots points from the s of the lowest density. The \code{smooth.bins} parameter specifies the number of bins in both directions for the density estimation. The \code{smooth.trans} parameter specifies the exponent in the function that maps the density scale to the color scale to allow customization of the intensity of the plotted gradient colors. Higher values result in less color saturation, deemphasizing points from regions of lessor density. These parameters are respectively passed directly to the \code{\link{smoothScatter}} \code{nrpoints}, \code{nbin} and \code{transformation} parameters. Grid lines are turned off, but can be displayed by setting the \code{grid} parameter.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{fill} for the interior color of a plotted point. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{theme}}. The default color theme is \code{dodgerblue}. A gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{theme}}, such as \code{"sienna"} and \code{"orange.black"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors. 

Colors can also be changed for individual aspects of a scatterplot as well. To provide a warmer tone by slightly enhancing red, try a background color such as \code{bg="snow"}. Obtain a very light gray with \code{bg="gray99"}.  To darken the background gray, try \code{bg="gray97"} or lower numbers. See the \code{lessR} function \code{\link{showColors}}, which provides an example of all available named colors.

For the color options, such as \code{grid}, the value of \code{"off"} is the same as 
\code{"transparent"}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ADDITIONAL OPTIONS\cr
Commonly used graphical parameters that are available to the standard R function \code{\link{plot}} are also generally available to \code{\link{ScatterPlot}}, such as:

\describe{
\item{cex.main, col.lab, font.sub, etc.}{Settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}
\item{main}{Title of the graph, see \code{\link{title}}.}
\item{xlim}{The limits of the plot on the x-axis, expressed as c(x1,x2), where x1 and x2 are the limits. Note that x1 > x2 is allowed and leads to a reversed axis.}
\item{ylim}{The limits of the plot on the y-axis.}

ONLY VARIABLES ARE REFERENCED\cr
A referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > ScatterPlot(rnorm(50), rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > X <- rnorm(50)   # create vector X in user workspace
    > Y <- rnorm(50)   # create vector Y in user workspace
    > ScatterPlot(X,Y)     # directly reference X and Y}
}

}

\references{
Murdoch, D, and  Chow, E. D. (2013).  \code{ellipse} function from the \code{ellipse} package package. 

Gerbing, D. W. (2013). R Data Analysis without Programming, Chapter 8, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{stripchart}}, \code{\link{title}}, \code{\link{par}}, \code{\link{Correlation}}, \code{\link{theme}}.
}


\examples{
# read the data
mydata <- rd("Employee", format="lessR", quiet=TRUE)
mydata <- Subset(random=.4, quiet=TRUE)  # less computationally intensive

#----------------------------------------------------
# traditional scatterplot with two numeric variables
#----------------------------------------------------

# scatterplot with all defaults
Plot(Years, Salary)
# or use abbreviation sp in place of Plot

# new shape and point size, no grid or background color
Plot(Years, Salary, size=2, shape="diamond", bg="off", grid="off")

# display the value of Pre for the bubbles that have the values of
#   min, median and max
Plot(Years, Salary, size=Pre, bubble.text=3)

# scatterplot, with loess line and filled ellipse with low opacity, .1 
# save scatterplot to a pdf file
Plot(Years, Salary, fit=TRUE, ellipse=TRUE,
   fill.ellipse=rgb(.6,.3,.3,.1), pdf.file="MyScatterPlot.pdf")

# scatterplot with ellipses
Plot(Years, Salary, ellipse=seq(.6,.9))

# scatterplot with three x-variables, plotted against Salary
Plot(c(Pre, Post, Years), Salary)

# increase span (smoothing) from default of .75
# span is a loess parameter and generates a caution that can be
#   ignored that it is not a graphical parameter -- we know that
#Plot(Years, Salary, fit="loess", span=1.25)

# change color theme to gray scale, then back to default
# 2-D kernel density (more useful for larger sample sizes) 
theme(colors="gray")
#Plot(Years, Salary, smoothed=TRUE)
theme(colors="dodgerblue")

# variables of interest are in a data frame not the default mydata
Plot(eruptions, waiting, ellipse=TRUE, data=faithful)


#-----------------------------------------------------------------
# analysis of two numeric variables with a by categorical variable
#-----------------------------------------------------------------

# by variable scatterplot with default point color, vary shapes
Plot(Years, Salary, by=Gender)

# vary both shape and color with a least-squares fit line for each group
Plot(Years, Salary, by=Gender, color=c("darkgreen", "brown"), shape=c("F","M"),
     size=.8, fit="ls")


#--------------------------------------
# analysis of a single numeric variable
#--------------------------------------

# 1-variable scatterplots
# ------------------------
# default 1-variable scatterplot, continuous
Plot(Salary)

# custom colors for outliers
Plot(Salary, pt.reg=23, out15="hotpink", out30="darkred")

# one variable scatterplot with added jitter of points and a boxplot
Plot(Salary, method="jitter", boxplot=TRUE)

# by variable with custom colors, keeps only 1 shape
Plot(Salary, by=Gender, stroke=c("steelblue", "hotpink"))

# binned values to plot counts
# ----------------------------
# bin the values of Salary to plot counts as a frequency polygon
Plot(Salary, values="count")  # bin the values

# time charts
#------------
# run chart, with fill area
Plot(Salary, line.chart=TRUE, area="steelblue")

# two run charts in same plot
# or could do a multivariate time series
Plot(c(Pre, Post), line.chart=TRUE)

# daily time series plot
# create the daily time series from R built-in data set airquality
oz.ts <- ts(airquality$Ozone, start=c(1973, 121), frequency=365)
Plot(oz.ts)

# multiple time series plotted from dates and stacked
date <- seq(as.Date("2013/1/1"), as.Date("2016/1/1"), by = "quarter")
x1 <- rnorm(13, 100, 15)
x2 <- rnorm(13, 100, 15)
x3 <- rnorm(13, 100, 15)
x4 <- rnorm(13, 100, 15)
df <- data.frame(date, x1, x2, x3, x4)
Plot(date, x1:x4, data=df, area=TRUE)


#------------------------------------------
# analysis of a single categorical variable
#------------------------------------------

# default 1-D bubble plot
# frequency plot, in place bar chart 
Plot(Dept)

# abbreviated category labels
Plot(Dept, label.max=2)

# plot of frequencies for each category (level), replaces bar chart 
Plot(Dept, values="count")


#----------------------------------------------------
# scatterplot of numeric against categorical variable 
#----------------------------------------------------

# generate a chart with the plotted mean of each level
Plot(Dept, Salary)

# rotated axis labels and then offset to fit
Plot(Dept, Salary, rotate.values=45, offset=1)


#-------------------
# Cleveland dot plot 
#-------------------

# row.names on the y-axis
Plot(Salary, row.names)

# standard scatterplot
Plot(Salary, row.names, sort.yx=FALSE, segments.y=FALSE, grid="on")

# Cleveland dot plot with two x-variables
Plot(c(Pre, Post), row.names)



#----------------------------------------------------
# analysis of two categorical variables (Likert data)
#----------------------------------------------------
mydata <- rd("Mach4", format="lessR", quiet=TRUE)  # Likert data, 0 to 5
mydata <- Subset(random=.4, quiet=TRUE)  # less computationally intensive

# size of each plotted point (bubble) depends on its joint frequency
# triggered by default when  < n.cat=10 unique values for each variable
Plot(m06, m07)

# use value labels for the integer values
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree")
Plot(m06,  m07, value.labels=LikertCats)

# get correlation analysis instead of cross-tab analysis
Plot(m06, m07, n.cat=2)

# plot Likert data and get sunflower plot with loess line
Plot(m06, m07, shape="sunflower", fit="loess")

# proportions within each level of the other variable
Plot(m06, m07, proportion=TRUE)


#-----------------------------
# Bubble Plot Frequency Matrix
#-----------------------------

Plot(c(m06,m07,m09,m10), value.labels=LikertCats)



#---------------
# function curve
#---------------

x <- seq(10,50,by=2) 
y1 <- sqrt(x)
y2 <- x**.33
# x is sorted with equal intervals so run chart by default
Plot(x, y1)
# custom function plot
Plot(x, y1, ylab="My Y", xlab="My X", main="My Curve", stroke="blue", 
  bg="snow", area="lightsteelblue", grid="lightsalmon")

# multiple plots, need data frame
mydata <- data.frame(x, y1, y2)
Plot(x, c(y1, y2))


#-----------
# modern art
#-----------

clr <- colors()
clr[-(153:353)]  # get rid of most of the grays
n <- sample(2:30, size=1)
x <- rnorm(n)
y <- rnorm(n)
color1 <- clr[sample(1:length(clr), size=1)]
color2 <- clr[sample(1:length(clr), size=1)]
Plot(x, y, line.chart=TRUE, area=color1, stroke=color2,
   xy.ticks=FALSE, main="Modern Art", xlab="", ylab="",
   cex.main=2, col.main="lightsteelblue", n.cat=0)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }




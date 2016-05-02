\name{Plot}
\alias{Plot}
\alias{ScatterPlot}
\alias{sp}

\title{Plot One or Two Continuous and/or Categorical Variables}

\description{
Abbreviation: \code{sp}, \code{ScatterPlot}

From the identical syntax, for variables \code{X} and \code{Y}, of \code{Plot(X)} or \code{Plot(X,Y)}, a family of related 1- or 2-dimensional scatterplots and related statistical analyses result from any combination of continuous or categorical variables: the traditional scatterplot of two continuous variables, a bubble (balloon) scatter plot from two categorical variables, a scatter plot with means at each level of a categorical variable paired with a continuous variable, and a Cleveland dot plot as a scatterplot that pairs a continuous variable with a each unique value of an ID-variable for a data table. For multiple plots on the same graph, specify multiple x-variables or y-variables. Summarize univariate distributions with either a 1-dimensional scatter plot of a continuous variable, or with a 1-dimensional bubble plot for a categorical variable as a more compact replacement of the traditional bar chart. From the specification of multiple categorical x-variables that share the same response scale, generalize the later to a matrix of 1-dimensional bubble plots here called the bubble plot frequency matrix. Choose data as the topic of analysis, or of statistics computed from that data such as the mean, and choose among different geometric objects to plot such as points, lines or bars.
}

\usage{
Plot(x, y=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"),

         topic=c("data", "count", "prop", "mean", "sd", "min", "median", "max",
                 "diff"),
         object=c("point", "line", "both", "bubble", "sunflower", "bar", "off"),

         color.fill=getOption("color.fill.pt"),
         color.stroke=getOption("color.stroke.pt"),
         color.bg=getOption("color.bg"),
         color.grid=getOption("color.grid"),
         color.box=getOption("color.box"),

         color=NULL, color.trans=NULL, color.area=NULL,

         cex.axis=0.76, color.axis="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,

         size=NULL, shape="circle", means=TRUE, 
         sort.yx=FALSE, segments.y=FALSE, segments.x=FALSE,

         bubble.size=0.25, bubble.power=0.6, bubble.counts=TRUE,
         color.low=NULL, color.hi=NULL,

         fit.line=NULL, color.fit.line="gray55",

         ellipse=FALSE, color.ellipse="lightslategray",
         color.fill.ellipse="off", 

         method="overplot", pt.reg="circle", pt.out="circle", 
         color.out30="firebrick2", color.out15="firebrick4", new=TRUE,

         breaks="Sturges", bin.start=NULL, bin.width=NULL, bin.end=NULL,
         prop=FALSE, cumul=c("off", "on", "both"), hist.counts=FALSE,
         color.reg="snow2",

         beside=FALSE, horiz=FALSE, 
         over.grid=FALSE, addtop=0.05, gap=NULL, count.labels=NULL,
         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         digits.d=NULL, quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=NULL, pdf.height=NULL,
         fun.call=NULL, \ldots)

ScatterPlot(\ldots)

sp(\ldots)
}

\arguments{
  \item{x}{If both x and y are specified, then the x-values are the coordinates plotted
        on the horizontal axis.  If x is sorted, then the default is to join the
        points with line segments. Multiple x-variables or y-variables, but not both.}
  \item{y}{Coordinates of points in the plot on the vertical axis.} 
  \item{by}{An optional grouping variable such that the points of all (x,y) pairs are
        plotted in the same plotting symbol and/or same color, with a different symbol
        or symbol and/or color for each group. Applies only to \code{object="point"}
        scatter plots.}
  \item{data}{Optional data frame that contains one or both of the variables of interest, 
        default is \code{mydata}.}
  \item{n.cat}{Specifies the largest number of unique values of variable of a numeric data
        type for which the variable will be analyzed as categorical so as to generate a
        bubble plot. Set to 0 to turn off.}

  \item{topic}{The values that are plotted, which are data values by default, or
        plots a statistic across levels of a categorical
        variable. If only \code{x} is specified, then only \code{"counts"} and
        \code{proportion} apply.
        If \code{x} and \code{y}, then, instead, \code{"mean"}, etc.}
  \item{object}{Geometric object plotted. Default is a
        \code{"point"} scatterplot for numerical variables unless 
        each variable has less than \code{n.cat} integer values, by default 8, when
        a bubble plot is plotted with the corresponding joint frequency determining the
        size of the bubble, or a \code{"line"} is specified by default when the x-values
        are sorted with equal intervals.}

  \item{color.fill}{For plotted points, the interior color of the points. By default, is
       a partially transparent version of the border color, \code{color.stroke}. 
       If y-values are unique, as in a Cleveland dot plot, then no transparency by 
       default as there can be no over-plotting.}
  \item{color.stroke}{Border color of the plotted points. If there is a \code{by} variable,
       specified as a vector, one value for each level of \code{by}.}
  \item{color.bg}{Color of the plot background.}
  \item{color.grid}{Color of the grid lines, with a default of \code{"snow3"}.}
  \item{color.box}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}

  \item{color}{Simultaneously specifies both \code{color.stroke} and \code{color.fill}, and
       takes precedence over their individually specified values.}
  \item{color.trans}{Transparency level from 0 (none) to 1 (complete).}
  \item{color.area}{Color of area under the plotted line segments.}

  \item{cex.axis}{Scale magnification factor of the values on the axes.}
  \item{color.axis}{Color of the font used to label the axis values.}
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
  \item{value.labels}{Labels for the x-axis on the graph to override the
        existing data values, including factor levels. If the variable is a 
        factor and value.labels is not specified (is \code{NULL}), then the
        value.labels are set to the factor levels with each space replaced by
        a new line character. If x and y-axes have the same scale, they also apply
        to the y-axis.}
  \item{rotate.values}{Degrees that the axis values are rotated, usually to accommodate
        longer values, typically used in conjunction with \code{offset}.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}

  \item{size}{Magnification factor for any displayed points or line, with default of 1.0 
        for points and 2.0 for a line. Can also be accomplished with \code{bubble.size}
        with a different metric.}
  \item{shape}{The plot character. The default value is a circle with both a border
       and filled area, specified with \code{color.stroke} and \code{color.fill}.
       Possible values are \code{circle}, \code{square}, \code{diamond},
       \code{triup} (triangle up), \code{tridown} (triangle down), all
       uppercase and lowercase letters, all digits, and most punctuation characters.
       The numbers 21 through 25 as defined by the R \code{\link{points}} function
       also apply.}
  \item{means}{If the first variable is a factor and the other variable continuous,
       then if \code{TRUE}, by default, plot means with the scatter plot.}
  \item{sort.yx}{Sort the values of y by the values of x, such as for a
        Cleveland dot plot, that is, a numeric x variable and categorical y variable
        with unique values.}
  \item{segments.y}{For one x-variable, draw line segments from y-axis to plotted point,
        such as for the Cleveland dot plot. For two x-variables, the line segments
        connect the two points.}
  \item{segments.x}{Draw line segments from x-axis to plotted point.}

  \item{bubble.size}{Absolute size of the bubbles in a bubble plot of Likert-style data,
       with default of 0.25. Setting this value sets default to \code{object="bubble"}.}
  \item{bubble.power}{Relative size of the scaling of the bubbles to each other.
        Value of 0.5 scales the bubbles so that the area of each bubble is the (joint)
        frequency. Value of 1 scales so the radius of the bubble is the frequency. The
        default value is 0.6.}
  \item{bubble.counts}{If \code{TRUE}, then for a bubble plot, the count underlying a
       bubble is displayed in the center of the bubble, unless the bubble is too small.
       Setting this value sets default to \code{object="bubble"}.}
  \item{color.low}{For categorical variables and the resulting bubble plot,
        or a matrix of these plots, allows a color gradient beginning with this color.}
  \item{color.hi}{For categorical variables and the resulting bubble plot,
        or a matrix of these plots, allows a color gradient ending with this color.}

  \item{fit.line}{The best fitting line.  Default value is \code{FALSE}, with options for 
      \code{"loess"} and for least squares, indicated by \code{"ls"}. Or, if set to 
      \code{TRUE}, then a loess line.}
  \item{color.fit.line}{Color of the best fitting line, if the \code{fit.line} option
       is invoked.}

  \item{ellipse}{If \code{TRUE}, enclose a scatterplot with the .95 data ellipse from the
      ellipse package. Or can specify a single numeric value greater than 0 and less than 1,
      or a vector of levels to plot multiple ellipses.}
  \item{color.ellipse}{Color of the ellipse. If specified, \code{ellipse} is set to \code{TRUE}.}
  \item{color.fill.ellipse}{If \code{TRUE}, fill the ellipse with \code{color.ellipse}. Usually
        specify low opacity in the color specification, as shown in the examples. If specified, \code{ellipse} is set to \code{TRUE}.}

  \item{method}{Applies to one variable plots. Default is \code{"overplot"}, but can also
       provide \code{"stack"} to stack the points or
       \code{"jigger"} to scramble the points.}
  \item{pt.reg}{For dot plot, type of regular (non-outlier) point. Default is 21, a
        circle with specified fill.}
  \item{pt.out}{For a 1-D scatterplot, type of point for outliers. Default is 19, a
        filled circle.}
  \item{color.out30}{For a 1-D scatterplot, color of outliers.}
  \item{color.out15}{For a 1-D scatterplot, color of potential outliers.}
  \item{new}{If \code{FALSE}, then add the 1-D scatterplot to an existing graph.}

  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other options 
       provided by the \code{\link{hist}} function.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{bin.end}{Optional specified value that is within the last bin, so the actual endpoint
        of the last bin may be larger than the specified value.}
  \item{prop}{Specify proportions or relative frequencies on the vertical axis. Default
       is \code{FALSE}.}
  \item{cumul}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of \code{"both"} 
        superimposes the regular histogram.}
  \item{hist.counts}{Replaces standard R \code{labels} options, which has multiple
       definitions in R. Specifies to display the count of each bin.}
  \item{color.reg}{The color of the superimposed, regular histogram when \code{cumul="both"}.}

  \item{beside}{For a two variable plot, set to \code{TRUE} for the levels of the
       first variable to be plotted as adjacent bars instead of stacked on each other.}
  \item{horiz}{By default bars are vertical, but can set this option to \code{TRUE}.}
  \item{over.grid}{If \code{TRUE}, plot the grid lines over the histogram.}
  \item{addtop}{When \code{horiz=FALSE}, in the same scale as the vertical axis, puts
       more space between the bars and the top of the plot area, usually to
       accommodate the legend when plotting two variables. now a multiplicative
       factor instead of additive as previous to Version 3.4.5.}
  \item{gap}{Gap between bars.  Provides the value of the \code{space} option from
       the standard R \code{\link{barplot}} function with a default of 0.2 unless two
       variables are plotted and beside=\code{TRUE}, in which case the default is
       c(.1,1).}
  \item{count.labels}{If the name of a variable, this signals that the primary
       variable \code{x} has values that are counts, already tabulated. The
       specified variable here contains the names of the levels of x.}
  \item{legend.title}{Title of the legend, which is usually set by default except when
        raw counts are entered as a matrix.  Then a title must be specified to
        generate a legend.}
  \item{legend.loc}{When plotting two variables, location of the legend, with the
        default in the right margin. Additional options from standard R are
        "topleft", "top", "topright" and others as shown in the help for the
        \code{\link{legend}} function.}
  \item{legend.labels}{When plotting two variables, labels for the legend, which by
       default are the levels for the second or \code{by} variable.}
  \item{legend.horiz}{By default the legend is vertical, but can be changed to
       horizontal.}

  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{theme}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches, defaults to 5.}
  \item{pdf.height}{Height of the pdf file in inches, defaults to 5 except for 1-D scatter plots.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{sp}.}
  \item{\ldots}{Other parameter values for graphics as defined by and then processed 
      by \code{\link{plot}} and \code{\link{par}}, including \code{xlim}, \code{ylim} for setting
      the range of the x and y-axes,
      \code{lty} for line type, \code{sub} and 
      \code{col.sub} for a subtitle, and \code{col.lab} for the color of the 
      axis labels and \code{color.axis} to set the color of the axis values. For
      one variable, parameters from \code{\link{stripchart}}.}
}


\details{
OUTPUT\cr
Two (or more) numeric variables produces a traditional scatter plot, based on the standard R function \code{\link{plot}}, with an analysis of the correlation coefficient including hypothesis test and confidence interval. Two categorical variables, such as for Likert-style analysis, produces a bubble plot, in which the size of each plotted point indicates the corresponding joint frequency, and a corresponding cross-tabulation analysis. This analysis is an alternative to the traditional \code{\link{BarChart}}. A categorical variable paired with a numeric variable yields a scatter plot with the means of each level of the categorical variable also plotted, and the summary statistics of the numeric variable for each level of the categorical variable. More information is obtained to list the categorical first in the function call.  If the values of the first variable are numeric and sorted with equal intervals, then points are connected via line segments.  If there is only one variable, a 1-dimensional scatter plot is produced for a numeric variable, based on the standard R function \code{\link{stripchart}}, and a 1-dimensional bubble plot is produced for a factor, with corresponding statistics.

The value labels for each axis can be over-ridden from their values in the data to user supplied values with the \code{value.labels} option. This option is particularly useful for Likert-style data coded as integers. Then, for example, a 0 in the data can be mapped into a "Strongly Disagree" on the plot. These value labels apply to integer categorical variables, and also to factor variables. To enhance the readability of the labels on the graph, any blanks in a value label translate into a new line in the resulting plot. Blanks are also transformed as such for the labels of factor variables. 

DATA\cr
The default input data frame is \code{mydata}.  Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{mydata$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment. 

CATEGORICAL VARIABLES\cr
Categorical variables have relatively few unique data values. The standard and most general way to define a categorical variable is as an R factor, illustrated in the examples for the \code{\link{Transform}} function. \code{lessR} also provides the option of defining an integer variable with equally spaced values as categorical based on the value of \code{n.cat}, which can be set locally or globally with the \code{\link{theme}} function. For example, for a variable with data values from 5-point Likert scale, a value of \code{n.cat} of 5 will define the define the variable as categorical. The default value is 8. To explicitly analyze the values as numerical, set \code{n.cat} to a value lower than 5, usually 0. Can also annotate a graph of the values of an integer categorical variable with \code{value.labels} option.  

A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 25 possible paired values to plot, so most of the plotted points overlap with others.  In this situation, that is, when a single variable or two variables with Likert response scales are specified, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values. A sunflower plot can be requested in lieu of the bubble plot with the \code{object} option.

ADAPTIVE GRAPHICS\cr
Results for two variables are based on the standard \code{\link{plot}} and related graphic functions, with the additional provided color capabilities and other options including a center line.  The plotting procedure utilizes ``adaptive graphics'', such that \code{ScatterPlot} chooses different default values for different characteristics of the specified plot and data values. The goal is to produce a desired graph from simply relying upon the default values, both of the \code{ScatterPlot} function itself, as well as the base R functions called by \code{ScatterPlot}, such as \code{\link{plot}}. Familiarity with the options permits complete control over the computed defaults, but this familiarity is intended to be optional for most situations.

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced, that is, a call to the standard R \code{\link{plot}} function with \code{type="p"} for points. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{type="l"}, which connects each adjacent pair of points with a line segment.

Specifying multiple-x variables against a single y-variable results in multiple plots on the same graph. The color of the points of the second variable is the same as that of the first variable, but with a transparent fill. For more than two x-variables, multiple colors are displayed, one for each x-variable.

BUBBLE PLOT FREQUENCY MATRIX (BPFM)\cr
Multiple categorical variables for \code{x} may be specified, without specifying a \code{y} variable. A bubble plot results that illustrates the frequency of each response for each of the variables in a common figure. Each line of information, the bubbles and counts for a single variable, replaces the standard bar chart in a more compact display. Each variable in the matrix must have the same number of response categories, that is, levels. If not, then use the factor transformation with the levels option to ensure that the levels are the same for each variable. See the examples the end of the \code{\link{Transform}} function documentation. The BPFM is considerably condensed presentation of frequencies for a set of variables than are the corresponding bar charts.

BY VARIABLE\cr
A variable specified with \code{by=} is a grouping variable that specifies that the plot is produced with the points for each group plotted with a different shape and/or color. By default, the shapes vary by group, and the color of the plot symbol remains the same for the groups. The default shapes, in this order, are \code{"circle"}, \code{"diamond"},  \code{"square"}, \code{"triup"} for a triangle pointed up, and \code{"tridown"} for a triangle pointed down.

To explicitly vary the shapes, use \code{shape} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{color.fill}, such as with R standard color names. If \code{color.fill} is specified without \code{shape}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable. 

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{ellipse} function, written by Duncan Murdoch and E. D. Chow, from the \code{ellipse} package. The axes are automatically lengthened to provide space for the entire ellipse that extends beyond the maximum and minimum data values. Multiple numerical values of \code{ellipse} may also be specified, to obtain multiple ellipses. 

ONE VARIABLE PLOT\cr
The one variable plot is a 1-dimensional scatterplot, that is, a dot chart. For a numerical variable, results are based on the standard \code{\link{stripchart}} function. Colors are provided by default and can also be specified. For gray scale output, potential outliers are plotted with squares and actual outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The definition of outliers are from the R \code{\link{boxplot}} function.  The plot can also be obtained as a bubble plot for a categorical variable.

RUN CHART\cr
Specifying one or more x-variables with no y-variables, and \code{object="line"}  plots the x-variables in a run chart, with Index on the x-axis. Index is the ordinal position of each data value, from 1 to the number of values. 

BINS\cr
Specifying \code{object="bar"} generates a histogram for a continuous variable and a bar chart for a non-numeric variable. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function or \code{\link{VariableLabels}}.  If variable labels exist, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output. 

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{color.fill} for the interior color of a plotted point. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{theme}}. The default color theme is \code{dodgerblue}. A gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{theme}}, such as \code{"sienna"} and \code{"orange.black"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors. 

Colors can also be changed for individual aspects of a scatterplot as well. To provide a warmer tone by slightly enhancing red, try a background color such as \code{color.bg="snow"}. Obtain a very light gray with \code{color.bg="gray99"}.  To darken the background gray, try \code{color.bg="gray97"} or lower numbers. See the \code{lessR} function \code{\link{showColors}}, which provides an example of all available named colors.

For the color options, such as \code{color.grid}, the value of \code{"off"} is the same as 
\code{"transparent"}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

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


#----------------------------------------------------
# traditional scatter plot with two numeric variables
#----------------------------------------------------

# scatterplot with default object of point and default data table mydata
Plot(Years, Salary)
# or use ScatterPlot or sp in place of Plot

# new shape and point size, no grid or background color
Plot(Years, Salary, size=2, shape="diamond", color.bg="off", color.grid="off")

# abbreviated function name
# scatterplot, with loess line and filled ellipse with low opacity, .1 
# save scatterplot to a pdf file
Plot(Years, Salary, fit.line=TRUE, ellipse=TRUE,
   color.fill.ellipse=rgb(.6,.3,.3,.1), pdf.file="MyScatterPlot.pdf")

# scatterplot with many ellipses
Plot(Years, Salary, ellipse=seq(.2,.9, .1))

# scatterplot with three x-variables, plotted against Salary
Plot(c(Pre, Post, Years), Salary)

# increase span (smoothing) from default of .75
# span is a loess parameter and generates a caution that can be
#   ignored that it is not a graphical parameter -- we know that
#Plot(Years, Salary, fit.line="loess", span=1.25)

# change color theme to gray scale, then back to default
theme(colors="gray")
Plot(Years, Salary)
theme(colors="dodgerblue")

# variables of interest are in a data frame which is not the default mydata
Plot(eruptions, waiting, ellipse=TRUE, data=faithful)

# by variable scatterplot with default point color, vary shapes
Plot(Years, Salary, by=Gender)
# by variable with values of Gender for plotting symbols
# reduce the size of the plotted symbols with size<1
Plot(Years, Salary, by=Gender, shape=c("F","M"), size=.6)
# vary both shape and color 
Plot(Years, Salary, by=Gender, color=c("darkgreen", "brown"), shape=c("F","M"))
# stroke and fill colors set, with a least-squares fit line for each group
Plot(Years, Salary, by=Gender, color=c("darkgreen", "brown"), fit.line="ls")


#--------------------------------------
# analysis of a single numeric variable
#--------------------------------------

# default dot plot (1-variable scatter plot, continuous)
Plot(Salary)
# dot plot with custom colors for outliers
Plot(Salary, pt.reg=23, color.out15="hotpink", color.out30="darkred")
# one variable scatterplot with added jitter of points
Plot(Salary, method="jitter")
# by variable dot plot with custom colors, keeps only 1 shape
Plot(Salary, by=Gender, color.stroke=c("steelblue", "hotpink"))
# line chart, with both line and point activated
Plot(Salary, object="both")

# default histogram
Plot(Salary, object="bar")
# or, specify a bin parameter, which also sets the object to "bar"
Plot(Salary, bin.width=5000)

# 1-D run chart instead of bubble plot by specifying line
Plot(Salary, object="line")
# two 1-D run charts in same plot
Plot(c(Pre, Post), object="line")


#------------------------------------------
# analysis of a single categorical variable
#------------------------------------------

# Default 1-D bubble plot
# frequency plot, in place bar chart 
Plot(Dept)
# plot of frequencies for each category (level), replaces bar chart 
Plot(Dept, topic="count")


#----------------------------------------------------
# scatterplot of numeric against categorical variable 
#----------------------------------------------------

# generates a means chart
Plot(Dept, Salary)
# rotated axis labels and then offset to fit
Plot(Dept, Salary, rotate.values=45, offset=1)
# just plot means
Plot(Dept, Salary, topic="mean")
# bar plot of means
Plot(Dept, Salary, object="bar", topic="mean")


#----------------------------------------------------
# analysis of two categorical variables (Likert data)
#----------------------------------------------------
mydata <- rd("Mach4", format="lessR", quiet=TRUE)  # Likert data, 0 to 5

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
Plot(m06, m07, object="sunflower", fit.line="loess")

# two variable bar chart, default is topic="count" for object="bar"
Plot(m06, m07, object="bar")


#-----------------------------
# Bubble Plot Frequency Matrix
#-----------------------------

# generate a table of frequency distributions for multiple categorical
#   variables witht he same response scale
# specify a range of x-variables, no y-variable
# each row is a bubble plot of frequencies for a single variable
Plot(c(m06,m07,m09,m10), rotate=25, offset=1)
# for each bubble, lighten fill color, make border black
Plot(m06:m12, color.fill=rgb(.094,.455,.804,alpha=.45), color.stroke="black")
# color range
Plot(c(m06,m07,m09,m10), color.low="lemonchiffon2", color.hi="lightsteelblue2")
# create BPFM for entire Mach IV scale with labels, store as a pdf file
Plot(m01:m20, value.labels=LikertCats, pdf.file="MachFreqs.pdf")


#-------------------
# Cleveland dot plot 
#-------------------
mydata <- rd("Employee", format="lessR", quiet=TRUE)

# row.names on the y-axis
Plot(Salary, row.names)
# with options
Plot(Salary, row.names, sort.yx=TRUE, segments.y=TRUE, color.bg="off", color.grid="off")
# Cleveland dot plot with two x-variables
Plot(c(Pre, Post), row.names, segments.y=TRUE, color.bg="off", color.grid="off")


#---------------
# function curve
#---------------

x <- seq(10,50,by=1) 
y1 <- sqrt(x)
y2 <- x**.33
# x is sorted with equal intervals so object set to line by default
Plot(x, y1)
# custom function plot
Plot(x, y1, ylab="My Y", xlab="My X", main="My Curve", color.stroke="blue", 
  color.bg="snow", color.area="lightsteelblue", color.grid="lightsalmon")
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
Plot(x, y, object="line", color.area=color1, color.stroke=color2,
   xy.ticks=FALSE, main="Modern Art", xlab="", ylab="",
   cex.main=2, col.main="lightsteelblue", n.cat=0)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }


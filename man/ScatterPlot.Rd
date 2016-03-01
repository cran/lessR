\name{ScatterPlot}
\alias{ScatterPlot}
\alias{sp}
\alias{Plot}

\title{Scatterplot for One or Two Variables}

\description{
Abbreviation: \code{sp}, \code{Plot}

Generates a scatter plot and corresponding statistics for one or two variables with an optional categorical by (grouping) variable, any combination of numeric or categorical, or a bubble plot frequency matrix (BPFM), a set of multiple bubble plots, one each for a set of categorical variables all with common responses. The categorical variables are variables with relatively few unique data values, formally defined as factors, or as integer variables. Color is provided according to the default color theme, which can be changed with the \code{\link{set}} function, or by changing the color of individual components of the graph such as grid lines, transparency, etc. The two variable scatter plots may include one or more data ellipses and a best fit line. Any provided variable labels, from \code{\link{Read}} or  \code{\link{VariableLabels}}, serve as labels for the axis or axes of the graph.
}

\usage{
ScatterPlot(x, y=NULL, by=NULL, data=mydata, type=NULL,
         n.cat=10, digits.d=NULL,

         col.fill=getOption("col.fill.pt"),
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),

         col.area=NULL, col.box="black",

         cex.axis=0.75, col.axis="gray30",
         xy.ticks=TRUE,
         xlab="", ylab=NULL, main=NULL, sub=NULL, cex=NULL,
         value.labels=NULL, rotate.values=0, offset=0.5,

         kind=c("default", "regular", "bubble", "sunflower"),

         fit.line=NULL, col.fit.line="grey55",

         shape.pts="circle", method="overplot", means=TRUE,

         bubble.size=0.25, bubble.counts=TRUE,
         col.low=NULL, col.hi=NULL,

         ellipse=FALSE, col.ellipse="lightslategray",
         col.fill.ellipse="transparent", 

         pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", new=TRUE,

         diag=FALSE, col.diag=par("fg"), lines.diag=FALSE,

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=NULL, pdf.height=NULL,
         fun.call=NULL, \ldots)

sp(\ldots)

Plot(\ldots)
}

\arguments{
  \item{x}{If both x and y are specified, then the x values are plotted on the horizontal 
        axis.  If x is sorted, then the points are joined by line segments by default.
        If only x is specified with no y, then these x values are plotted as a dot chart,
        a one-dimensional scatterplot. Multiple x variables (with no y variable) generate
        a matrix of bubble plots for the frequency of each level of each variable.}
  \item{y}{Coordinates of points in the plot on the vertical axis.}
  \item{by}{An optional grouping variable such that the points of all (x,y) pairs are
        plotted in the same plotting symbol and/or same color, with a different symbol
        or symbol and/or color for each group. Applies only to \code{kind="regular"}
        scatter plots.}
  \item{data}{Optional data frame that contains one or both of the variables of interest, 
        default is \code{mydata}.}
  \item{type}{Character string that indicates the type of plot, either \code{"p"} for 
        points, \code{"l"} for line, or \code{"b"} for both.  If x and y are provided and 
        x is sorted so that a function is plotted, the default is \code{"l"}, or, when x 
        is not sorted, the default is \code{"p"} for point, yielding a scatterplot.}
  \item{n.cat}{Specifies the largest number of unique values of variable of a numeric data
        type for which the variable will be analyzed as categorical so as to generate a
        bubble plot. Set to 0 to turn off. Unlike other \code{lessR} functions, defaults
        to 10 instead of 0 unless has been reset globally with function \code{set} to
        a value other than 0.}
  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{col.fill}{For plotted points, the interior color of the points. By default, is
       a partially transparent version of the border color, \code{col.stroke}. Does not
       apply if there is a \code{by} variable, which relies upon the default.} 
  \item{col.stroke}{Border color of the plotted points. If there is a \code{by} variable,
       specified as a vector, one value for each level of \code{by}.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines, with a default of \code{"grey90"}.}
  \item{col.area}{Color of area under the plotted line segments.}
  \item{col.box}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis values to be 
        smaller than the axis labels.}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
        axes are to be displayed.}
  \item{xlab}{Label for x-axis. For two variables specified, x and y, if \code{xlab} not
       specified, then the label becomes the name of the corresponding variable. If 
       \code{xy.ticks} is \code{FALSE}, then no label is displayed. If no y variable
       is specified, then \code{xlab} is set to Index unless \code{xlab} has been specified.}
  \item{ylab}{Label for y-axis. If not specified, then the label becomes the name of
      the corresponding variable. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below xlab.}
  \item{cex}{Magnification factor for any displayed points, with default of cex=1.0.}
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
  \item{kind}{Default is \code{"default"}, which becomes a \code{"regular"} scatterplot for 
      most data.  If Likert style response data is plotted, that is, 
      each variable has less than 10 integer values, then instead by default a bubble plot is 
      plotted with the corresponding joint frequency determining the size of the bubble.  A 
      sunflower plot can also be requested.}
  \item{fit.line}{The best fitting line.  Default value is \code{FALSE}, with options for 
      \code{"loess"} and for least squares, indicated by \code{"ls"}. Or, if set to 
      \code{TRUE}, then a loess line.}
  \item{col.fit.line}{Color of the best fitting line, if the \code{fit.line} option is invoked.}
  \item{shape.pts}{The standard plot character, with values defined in \code{\link{points}}. 
       The default value is 21, a circle with both a border and filled area, specified here
       with \code{col.pts} and \code{col.fill}.}
  \item{bubble.size}{Size of the bubbles in a bubble plot of Likert style data, with default
       of 0.25.}
  \item{bubble.counts}{If \code{TRUE}, then for a bubble plot, the count underlying a
       bubble is displayed in the center of the bubble, unless the bubble is too small.}
  \item{col.low}{For categorical variables and the resulting bubble plot,
        or a matrix of these plots, allows a color gradient beginning with this color.}
  \item{col.hi}{For categorical variables and the resulting bubble plot,
        or a matrix of these plots, allows a color gradient ending with this color.}
  \item{method}{Applies to one variable plots. Default is \code{"overplot"}, but can also
       provide \code{"stack"} to stack the points or
       \code{"jigger"} to scramble the points.}
  \item{means}{If the first variable is a factor, then plot means with the scatter plot.}
  \item{ellipse}{If \code{TRUE}, enclose a scatterplot with the .95 data ellipse from the
      ellipse package. Or can specify a single numeric value greater than 0 and less than 1,
      or a vector of levels to plot multiple ellipses.}
  \item{col.ellipse}{Color of the ellipse.}
  \item{col.fill.ellipse}{If \code{TRUE}, fill the ellipse with \code{col.ellipse}. Usually
        specify low opacity in the color specification, as shown in the examples.}
  \item{pt.reg}{For dot plot, type of regular (non-outlier) point. Default is 21, a circle with
                specified fill.}
  \item{pt.out}{For a dot plot, type of point for outliers. Default is 19, a filled circle.}
  \item{col.out30}{For a dot plot, color of outliers.}
  \item{col.out15}{For a dot plot, color of potential outliers.}
  \item{new}{If \code{FALSE}, then add the dot plot to an existing graph.}
  \item{diag}{Applies just to scatter plots of 2 numeric variables. If \code{TRUE}, then add
        a diagonal line to a 2-dimensional scatter plot.}
  \item{col.diag}{Color of diagonal line if \code{diag=TRUE}.}
  \item{lines.diag}{If \code{diag=TRUE}, then if \code{lines.diag=TRUE}, each point
       in the scatter plot is connected to the diagonal line with a line segment, and
       both axes are scaled in the same units.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{set}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches, defaults to 5.}
  \item{pdf.height}{Height of the pdf file in inches, defaults to 5 except for 1-D scatter plots.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{sp}.}
  \item{\ldots}{Other parameter values for graphics as defined by and then processed 
      by \code{\link{plot}} and \code{\link{par}}, including \code{xlim}, \code{ylim},
      \code{lwd}, and \code{cex} to specify a magnification factor for the plotting symbol,
      \code{col.lab}, \code{sub}, \code{col.sub}, \code{col.ticks} to set the color
      of the ticks used to label the axis values, \code{srt} to rotate the axis value labels.
      For one variable, parameters from \code{\link{stripchart}}.}
}


\details{
OUTPUT\cr
Two numeric variables produces a traditional scatter plot, based on the standard R function \code{\link{plot}}, with an analysis of the correlation coefficient including hypothesis test and confidence interval. Two categorical variables, such as for Likert style analysis, produces a bubble plot, in which the size of each plotted point indicates the corresponding joint frequency, and a corresponding cross-tabulation analysis. This analysis is an alternative to the traditional \code{\link{BarChart}}. A categorical variable paired with a numeric variable yields a scatter plot with the means of each level of the categorical variable also plotted, and the summary statistics of the numeric variable for each level of the categorical variable. More information is obtained to list the categorical first in the function call.  If the values of the first variable are numeric and sorted with equal intervals, then points are connected via line segments.  If there is only one variable, a 1-dimensional scatter plot is produced for a numeric variable, based on the standard R function \code{\link{stripchart}}, and a 1-dimensional bubble plot is produced for a factor, with corresponding statistics.

The value labels for each axis can be over-ridden from their values in the data to user supplied values with the \code{value.labels} option. This option is particularly useful for Likert style data coded as integers. Then, for example, a 0 in the data can be mapped into a "Strongly Disagree" on the plot. These value labels apply to integer categorical variables, and also to factor variables. To enhance the readability of the labels on the graph, any blanks in a value label translate into a new line in the resulting plot. Blanks are also transformed as such for the labels of factor variables. 

DATA\cr
The default input data frame is \code{mydata}.  Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{mydata$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment. 

ADAPTIVE GRAPHICS\cr
Results for two variables are based on the standard \code{\link{plot}} and related graphic functions, with the additional provided color capabilities and other options including a center line.  The plotting procedure utilizes ``adaptive graphics'', such that \code{ScatterPlot} chooses different default values for different characteristics of the specified plot and data values. The goal is to produce a desired graph from simply relying upon the default values, both of the \code{ScatterPlot} function itself, as well as the base R functions called by \code{ScatterPlot}, such as \code{\link{plot}}. Familiarity with the options permits complete control over the computed defaults, but this familiarity is intended to be optional for most situations.

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced, that is, a call to the standard R \code{\link{plot}} function with \code{type="p"} for points. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{type="l"}, which connects each adjacent pair of points with a line segment.

BUBBLE PLOT FREQUENCY MATRIX (BPFM)\cr
A range of categorical variables for \code{x} may be specified, without specifying a \code{y} variable. A bubble plot results that illustrates the frequency of each response for each of the variables in a common figure. Each line of information, the bubbles and counts for a single variable, replaces the standard bar chart in a more compact display. Each variable in the matrix must have the same number of response categories, that is, levels. If not, then use the factor transformation with the levels option to ensure that the levels are the same for each variable. See the examples the end of the \code{\link{Transform}} function documentation. The BPFM is considerably condensed presentation of frequencies for a set of variables than are the corresponding bar charts.

BY VARIABLE\cr
A variable specified with \code{by=} is a grouping variable that specifies that the plot is produced with the points for each group plotted with a different shape and/or color. By default, the shapes vary by group, and the color of the plot symbol remains the same for the groups. The default shapes, in this order, are \code{"circle"}, \code{"diamond"},  \code{"square"}, \code{"triup"} for a triangle pointed up, and \code{"tridown"} for a triangle pointed down.

To explicitly vary the shapes, use \code{shape.pts} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{col.pts}, such as with R standard color names. If \code{col.pts} is specified without \code{shape.pts}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable. 

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape.pts}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{ellipse} function, written by Duncan Murdoch and E. D. Chow, from the \code{ellipse} package. The axes are automatically lengthened to provide space for the entire ellipse that extends beyond the maximum and minimum data values. Multiple numerical values of \code{ellipse} may also be specified, to obtain multiple ellipses. 

ONE VARIABLE PLOT\cr
The one variable plot is a 1-dimensional scatterplot, that is, a dot chart. For a numerical variable, results are based on the standard \code{\link{stripchart}} function. Colors are provided by default and can also be specified. For gray scale output, potential outliers are plotted with squares and actual outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The definition of outliers are from the R \code{\link{boxplot}} function.  The plot can also be obtained as a bubble plot for a categorical variable.

LIKERT DATA\cr
A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 25 possible paired values to plot, so most of the plotted points overlap with others.  In this situation, that is, when there are less than 10 values for each of the two variables, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values. The value of 10 is the default local value of \code{n.cat}, which can be set to any specified value.  A sunflower plot can be requested in lieu of the bubble plot with the \code{kind} option.

DIAGONAL\cr
Useful particularly when comparing pre- and post- scores on some assessment, a diagonal line that runs from the lower-left corner of the graph to the upper-right corner represents the values of no change from a value on the x-axis that equals the corresponding value on the y-axis, where the pre and post scores are equal. Points on either side of that diagonal indicate \code{+} or \code{-} change. To provide this line, specify \code{diag=TRUE}, which will apply only to scatter plots with two numeric, non-categorical, variables. When so specified, for each data coordinate, a vertical line is drawn from the diagonal of no change to the point, unless \code{lines.diag} is set to \code{FALSE}. If \code{diag=TRUE}, then the axes limits are set so that each axis has the same beginning and ending point. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output. For more information, see \code{\link{Read}}.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{col.fill} for the interior color of a plotted point. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{set}}. The default color theme is \code{dodgerblue}. A gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}, such as \code{"sienna"} and \code{"orange.black"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors. 

Colors can also be changed for individual aspects of a scatterplot as well. To provide a warmer tone by slightly enhancing red, try \code{col.bg=snow}. Obtain a very light gray with \code{col.bg=gray99}.  To darken the background gray, try \code{col.bg=gray97} or lower numbers. See the \code{lessR} function \code{\link{showColors}}, which provides an example of all available named colors.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ADDITIONAL OPTIONS\cr
Commonly used graphical parameters that are available to the standard R function \code{\link{plot}} are also generally available to \code{\link{ScatterPlot}}, such as:

\describe{
\item{lwd}{Line width, see \code{\link{par}}.}
\item{cex}{Numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. This works as a multiple of \code{\link{par}}("cex"). NULL and NA are equivalent to 1.0. Note that this does not affect annotation.}
\item{cex.main, col.lab, font.sub, etc.}{Settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}
\item{main}{Title of the graph, see \code{\link{title}}.}
\item{xlim}{The limits of the plot on the x-axis, expressed as c(x1,x2), where x1 and x2 are the limits. Note that x1 > x2 is allowed and leads to a reversed axis.}
\item{ylim}{The limits of the plot on the y-axis.}

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

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
\code{\link{plot}}, \code{\link{stripchart}}, \code{\link{title}}, \code{\link{par}}, \code{\link{Correlation}}, \code{\link{set}}.
}


\examples{
# read the data
mydata <- rd("Employee", format="lessR", quiet=TRUE)


# default scatterplot, x is not sorted so type is set to "p"
# although data not attached, access each variable directly by its name
ScatterPlot(Years, Salary)

# compare to standard R plot, which requires the mydata$ notation
plot(mydata$Years, mydata$Salary)

# abbreviated function name
# scatterplot, with loess line and filled ellipse with low opacity, .1 
# save scatterplot to a pdf file
sp(Years, Salary, fit.line=TRUE, ellipse=TRUE,
   col.fill.ellipse=rgb(.6,.3,.3,.1), pdf.file="MyScatterPlot.pdf")

# scatterplot with many ellipses
sp(Years, Salary, ellipse=seq(.2,.9, .1))

# increase span (smoothing) from default of .75
# span is a loess parameter and generates a caution that can be
#   ignored that it is not a graphical parameter -- we know that
#ScatterPlot(x, y, fit.line="loess", span=1.25)

# custom scatterplot, with diagonal line, connecting line segments
# also red axis labels
ScatterPlot(Years, Salary, col.stroke="darkred", col.fill="plum",
   diag=TRUE, col.lab="red")

# scatterplot with a gray scale color theme 
#   or, use set(colors="gray") to invoke for all subsequent analyses
#   until reset back to default color of "dodgerblue"
set(colors="gray")
ScatterPlot(Years, Salary)
set(colors="dodgerblue")

# by variable scatterplot with default point color, vary shapes
ScatterPlot(Years, Salary, by=Gender)

# by variable scatterplot with custom colors, keeps only 1 shape
ScatterPlot(Years, Salary, by=Gender, col.stroke=c("steelblue", "hotpink"))

# by variable with values of Gender for plotting symbols
# reduce the size of Gender the plotted symbols with cex<1
ScatterPlot(Years, Salary, by=Gender, shape.pts=c("M","F"), cex=.6)

# vary both shape and color 
ScatterPlot(Years, Salary, by=Gender, col.stroke=c("steelblue", "hotpink"),
            shape.pts=c("M","F"))

# Default dot plot (1-variable scatter plot)
ScatterPlot(Salary)
# dot plot with custom colors for outliers
ScatterPlot(Salary, pt.reg=23, col.out15="hotpink", col.out30="darkred")
# one variable scatterplot with added jitter of points
ScatterPlot(Salary, method="jitter")
# by variable dot plot with custom colors, keeps only 1 shape
ScatterPlot(Salary, by=Gender, col.stroke=c("steelblue", "hotpink"))

# scatterplot of continuous Y against categorical X
# generates a means chart
ScatterPlot(Dept, Salary)
# rotated axis labels and then offset to fit
sp(Dept, Salary, rotate.values=45, offset=1)
# for this purpose, improved version of standard R stripchart
stripchart(mydata$Salary ~ mydata$Dept, vertical=TRUE)

# scatter (bubble) plot of two categorical variables
sp(Gender, Dept)

# read Likert data, 0 to 5 scale
mydata <- rd("Mach4", format="lessR", quiet=TRUE)
# size of each plotted point (bubble) depends on its joint frequency
# triggered by default when  < n.cat=10 unique values for each variable
ScatterPlot(m06, m07)
# use value labels for the integer values
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree")
sp(m06, m07, value.labels=LikertCats)
# get correlation analysis instead of cross-tab analysis
ScatterPlot(m06, m07, n.cat=2)
# plot Likert data and get sunflower plot with loess line
ScatterPlot(m06, m07, kind="sunflower", fit.line="loess")
# compare to usual scatterplot of Likert data, transparency helps
plot(mydata$m06, mydata$m07)
ScatterPlot(m06, m07, kind="regular", cex=3)

# generate a Bubble Plot Frequency Matrix (BPFM)
# specify a range of x-variables, no y-variable
# each line is a bubble plot of frequencies for a single variable
sp(c(m06,m07,m09,m10), rotate=25, offset=1)
# for each bubble, lighten fill color, make border black
sp(m06:m12, col.fill=rgb(.094,.455,.804,alpha=.45), col.stroke="black")
# color range
sp(c(m06,m07,m09,m10), col.low="lemonchiffon2", col.hi="lightsteelblue2")
# create BPFM for entire Mach IV scale with labels, store as a pdf file
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree")
sp(m01:m20, value.labels=LikertCats, pdf.file="MachFreqs.pdf")

# function curve
x <- seq(10,500,by=1) 
y <- 18/sqrt(x)
# x is sorted with equal intervals so type set to "l" for line
# can use the names Plot or ScatterPlot, here Plot is more appropriate
Plot(x, y)
# custom function plot
Plot(x, y, ylab="My Y", xlab="My X", col.stroke="blue", 
  col.bg="snow", col.area="lightsteelblue", col.grid="lightsalmon")

# modern art
n <- sample(2:30, size=1)
x <- rnorm(n)
y <- rnorm(n)
clr <- colors()
color1 <- clr[sample(1:length(clr), size=1)]
color2 <- clr[sample(1:length(clr), size=1)]
ScatterPlot(x, y, type="l", lty="dashed", lwd=3, col.area=color1, 
   col.stroke=color2, xy.ticks=FALSE, main="Modern Art", 
   cex.main=2, col.main="lightsteelblue", kind="regular",
   n.cat=0)


# -----------------------------------------------
# variables in a different data frame than mydata
# -----------------------------------------------

# variables of interest are in a data frame which is not the default mydata
ScatterPlot(eruptions, waiting, data=faithful)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }


\name{ScatterPlot}
\alias{ScatterPlot}
\alias{sp}
\alias{Plot}
\alias{DotPlot}
\alias{dp}

\title{Scatterplot for One (Dot Plot) or Two Variables}

\description{
Abbreviation: \code{sp}

Generates scatter plots for one or two variables. For two variables also produces an analysis of the correlation coefficient.  If the values of the first specified value are sorted, then points are connected via line segments. The first variable can be numeric or a factor.  The second variable must be numeric. For Likert style response data of two variables, so that each value has less than 10 unique integer values, the points in the plot are transformed into a bubble plot with the size of each bubble, i.e., point, determined by the corresponding joint frequency. An alternate name for \code{ScatterPlot} is just \code{Plot}. 

One enhancement over the standard R  \code{\link{plot}} function is the automatic inclusion of color.  The color of the line segments and/or the points, background, area under the plotted line segments, grid lines, and border can each be explicitly specified, with default colors provided by one of the pre-defined color themes as defined by the \code{\link{set}} function.  

If a scatterplot of two numeric variables is displayed, then the corresponding correlation coefficient as well as the hypothesis test of zero population correlation and the 95\% confidence interval are also displayed.  The same numeric values of the standard R function \code{\link{cor.test}} function are generated, though in a more readable format.  Also, an option for the .95 data ellipse from John Fox's \code{car} package can enclose the points of the scatterplot.

For one variable, based on the standard R function \code{\link{stripchart}}, plots a one dimensional scatterplot, that is, a dot chart, also called a strip chart. Also identifies outliers according to the criteria specified by a box plot and displays the summary statistics for the variable. The dot plot is also invoked with the function names \code{DotPlot} or just \code{dp}, which are just alternate names for \code{ScatterPlot} when a single variable is referenced.
}

\usage{
ScatterPlot(x, y=NULL, by=NULL, data=mydata, type=NULL, n.cat=getOption("n.cat"),

         col.fill=getOption("col.fill.pt"),
         col.stroke=getOption("col.stroke.pt"),
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),

         col.area=NULL, col.box="black",

         shape.pts="circle", cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, cex=NULL,

         kind=c("default", "regular", "bubble", "sunflower"),

         fit.line=c("none", "loess", "ls"), col.fit.line="grey55",

         bubble.size=.25, method="stack",

         ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE, 

         pt.reg="circle", pt.out="circle", 
         col.out30="firebrick2", col.out15="firebrick4", new=TRUE,

         diag=FALSE, col.diag=par("fg"), lines.diag=TRUE, 

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

sp(\ldots)

Plot(\ldots)

DotPlot(\ldots)
dp(\ldots)
}

\arguments{
  \item{x}{If both x and y are specified, then the x values are plotted on the horizontal 
        axis.  If x is sorted, then the points are joined by line segments by default.
        If only x is specified with no y, then these x values are plotted as a dot chart,
        a one-dimensional scatterplot.}
  \item{y}{Coordinates of points in the plot on the vertical axis.}
  \item{by}{An optional grouping variable such that the points of all (x,y) pairs are
        plotted in the same plotting symbol and/or same color, with a different symbol
        or symbol and/or color for each group. Applies only to \code{kind="regular"}
        scatterplots.}
  \item{data}{Optional data frame that contains one or both of the variables of interest, 
        default is \code{mydata}.}
  \item{type}{Character string that indicates the type of plot, either \code{"p"} for 
        points, \code{"l"} for line, or \code{"b"} for both.  If x and y are provided and 
        x is sorted so that a function is plotted, the default is \code{"l"}, or, when x 
        is not sorted, the default is \code{"p"} for point, yielding a scatterplot.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest number
        of unique values of variable of a numeric data type for which the variable will
        be analyzed as categorical. Set to 0 to turn off.}
  \item{col.fill}{For plotted points, the interior color of the points. By default, is
       a partially transparent version of the border color, \code{col.stroke}. Does not
       apply if there is a \code{by} variable, which relies upon the default.} 
  \item{col.stroke}{Border color of the plotted points. If there is a \code{by} variable,
       specified as a vector, one value for each level of \code{by}.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines, with a default of \code{"grey90"}.}
  \item{shape.pts}{The standard plot character, with values defined in \code{\link{points}}. 
       The default value is 21, a circle with both a border and filled area, specified here
       with \code{col.pts} and \code{col.fill}.}
  \item{col.area}{Color of area under the plotted line segments.}
  \item{col.box}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}
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
  \item{main}{Label for the title of the graph.  If the corresponding variable labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{cex}{Magnification factor for any displayed points, with default of cex=1.0.}
  \item{kind}{Default is \code{"default"}, which becomes a \code{"regular"} scatterplot for 
      most data.  If Likert style response data is plotted, that is, 
      each variable has less than 10 integer values, then instead by default a bubble plot is 
      plotted with the corresponding joint frequency determining the size of the bubble.  A 
      sunflower plot can also be requested.}
  \item{fit.line}{The best fitting line.  Default value is \code{"none"}, with options for 
      \code{"loess"} and \code{"ls"}.}
  \item{col.fit.line}{Color of the best fitting line, if the \code{fit.line} option is invoked.}
  \item{bubble.size}{Size of the bubbles in a bubble plot of Likert style data.}
  \item{method}{Applies to one variable plots. Default is \code{"stack"}, but can also provide
       \code{"jigger"} to scramble the points.}
  \item{ellipse}{If \code{TRUE}, enclose a scatterplot with the .95 data ellipse from the car package.}
  \item{col.ellipse}{Color of the ellipse.}
  \item{fill.ellipse}{If \code{TRUE}, fill the ellipse with a translucent shade of \code{col.ellipse}.}
  \item{pt.reg}{For dot plot, type of regular (non-outlier) point. Default is 21, a circle with
                specified fill.}
  \item{pt.out}{For a dot plot, type of point for outliers. Default is 19, a filled circle.}
  \item{col.out30}{For a dot plot, color of outliers.}
  \item{col.out15}{For a dot plot, color of potential outliers.}
  \item{new}{If \code{FALSE}, then add the dot plot to an existing graph.}
  \item{diag}{Applies just to scatter plots of 2 numeric variables. If \code{TRUE}, then add
        a diagonal line to a 2-dimensional scatter plot.}
  \item{col.diag}{Color of diagonal line if \code{diag=TRUE}.}
  \item{lines.diag}{If \code{lines.diag=TRUE}, then if \code{diag=TRUE}, each point is
        connected to the diagonal line with a line segment.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{set}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\ldots}{Other parameter values for graphics as defined by and then processed 
      by \code{\link{plot}} and \code{\link{par}}, including \code{xlim}, \code{ylim}, \code{lwd}, 
      and \code{cex} to specify a magnification factor for the plotting symbol. For one variable,
      parameters from \code{\link{stripchart}}. For type of correlation from \code{\link{cor.test}},
      \code{method="spearman"} and \code{method="kendall"}.}
}


\details{
DATA\cr
The default input data frame is \code{mydata}.  Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{mydata$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment. 

ADAPTIVE GRAPHICS\cr
Results for two variables are based on the standard \code{\link{plot}} and related graphic functions, with the additional provided color capabilities and other options including a center line.  The plotting procedure utilizes ``adaptive graphics'', such that \code{ScatterPlot} chooses different default values for different characteristics of the specified plot and data values. The goal is to produce a desired graph from simply relying upon the default values, both of the \code{ScatterPlot} function itself, as well as the base R functions called by \code{ScatterPlot}, such as \code{\link{plot}}. Familiarity with the options permits complete control over the computed defaults, but this familiarity is intended to be optional for most situations.

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced, that is, a call to the standard R \code{\link{plot}} function with \code{type="p"} for points. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{type="l"}, which connects each adjacent pair of points with a line segment.

BY VARIABLE\cr
A variable specified with \code{by=} is a grouping variable that specifies that the plot is produced with the points for each group plotted with a different shape and/or color. By default, the shapes vary by group, and the color of the plot symbol remains the same for the groups. The default shapes, in this order, are \code{"circle"}, \code{"diamond"},  \code{"square"}, \code{"triup"} for a triangle pointed up, and \code{"tridown"} for a triangle pointed down.

To explicitly vary the shapes, use \code{shape.pts} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{col.pts}, such as with R standard color names. If \code{col.pts} is specified without \code{shape.pts}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable. 

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape.pts}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{dataEllipse} function, written by Georges Monette and John Fox, from the \code{car} package. Usually the minimum and maximum values of the axes should be manually extended beyond their default to accommodate the entire ellipse. To accomplish this extension, use the \code{xlim} and \code{ylim} options, such as \code{xlim=c(30,350)}.  Obtaining the desired axes limits may involve multiple runs of the \code{ScatterPlot} function. To provide more control over the display of the data ellipse beyond the provided \code{col.ellipse} and \code{fill.ellipse} options, run the \code{dataEllipse} function directly with the \code{plot.points=FALSE} option following \code{ScatterPlot} with \code{ellipse=FALSE}, the default.

ONE VARIABLE PLOT\cr
The one variable plot is a one variable scatterplot, that is, a dot chart. Results are based on the standard \code{\link{stripchart}} function. Colors are provided by default and can also be specified. For gray scale output, potential outliers are plotted with squares and actual outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The definition of outliers are from the R \code{\link{boxplot}} function.

LIKERT DATA\cr
A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 25 possible paired values to plot, so most of the plotted points overlap with others.  In this situation, that is, when there are less than 10 values for each of the two variables, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values.  A sunflower plot can be requested in lieu of the bubble plot.

DIAGONAL\cr
Useful particularly when comparing pre- and post- scores on some assessement, a diagonal line that runs from the lower-left corner of the graph to the upper-right corner represents the values of no change from a value on the x-axis that equals the corresponding value on the y-axis, where the pre and post scores are equal. Points on either side of that diagonal indicate \code{+} or \code{-} change. To provide this line, specify \code{diag=TRUE}, which will apply only to scatter plots with two numeric, non-categorical, variables. When so specified, for each data coordinate, a vertical line is drawn from the diagonal of no change to the point, unless \code{lines.diag} is set to \code{FALSE}. If \code{diag=TRUE}, then the axes limits are set so that each axis has the same beginning and ending point. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output. For more information, see \code{\link{Read}}.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{col.bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}, such as \code{"red"} and \code{"green"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors. 

Colors can also be changed for individual aspects of a scatterplot as well. To provide a warmer tone by slightly enhancing red, try \code{col.bg=snow}. Obtain a very light gray with \code{col.bg=gray99}.  To darken the background gray, try \code{col.bg=gray97} or lower numbers. See the \code{lessR} function \code{\link{showColors}} which provides an example of all available named colors.

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
}

}

\references{
Monette, G. and Fox, J., \code{dataEllipse} function from the \code{car} package.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{stripchart}}, \code{\link{title}}, \code{\link{par}}, \code{\link{Correlation}}, \code{\link{set}}.
}


\examples{
# scatterplot
# create simulated data, no population mean difference
# X has two values only, Y is numeric
# put into a data frame, required for formula version
n <- 12
Gender <- sample(c("Women", "Men"), size=n, replace=TRUE)
x <- round(rnorm(n=n, mean=50, sd=10), 2)
y <- round(rnorm(n=n, mean=50, sd=10), 2)
z <- round(rnorm(n=n, mean=50, sd=10), 2)
mydata <- data.frame(Gender,x,y,z)
rm(Gender); rm(x); rm(y); rm(z)

# default scatterplot, x is not sorted so type is set to "p"
# although data not attached, access each variable directly by its name
ScatterPlot(x, y)

# short name with Spearman correlation instead of Pearson
sp(x,y, method="spearman")

# compare to standard R plot, which requires the mydata$ notation
plot(mydata$x, mydata$y)

# save scatterplot to a pdf file
ScatterPlot(x, y, pdf.file="MyScatterScatterPlot.pdf")

# scatterplot, with ellipse and extended axes to accommodate the ellipse
ScatterPlot(x, y, ellipse=TRUE, xlim=c(20,80), ylim=c(20,80))

# scatterplot, with loess line 
ScatterPlot(x, y, fit.line="loess")

# increase span (smoothing) from default of .75
ScatterPlot(x, y, fit.line="loess", span=1.25)

# custom scatterplot, with diagonal line, connecting line segments
ScatterPlot(x, y, col.stroke="darkred", col.fill="plum", diag=TRUE)

# scatterplot with a gray scale color theme 
#   or, use set(colors="gray") to invoke for all subsequent analyses
#   until reset back to default color of "blue"
set(colors="gray")
ScatterPlot(x, y)
set(colors="blue")

# by variable scatterplot with default point color, vary shapes
ScatterPlot(x,y, by=Gender)

# by variable scatterplot with custom colors, keeps only 1 shape
ScatterPlot(x,y, by=Gender, col.stroke=c("steelblue", "hotpink"))

# by variable with values of Gender for plotting symbols
# reduce the size of Gender the plotted symbols with cex<1
ScatterPlot(x, y, by=Gender, shape.pts=c("M","F"), cex=.6)

# vary both shape and color 
ScatterPlot(x, y, by=Gender, col.stroke=c("steelblue", "hotpink"),
            shape.pts=c("M","F"))

# Default dot plot
ScatterPlot(y)

# Dot plot with custom colors for outliers
ScatterPlot(y, pt.reg=23, col.out15="hotpink", col.out30="darkred")

# one variable scatterplot with added jitter of points
ScatterPlot(x, method="jitter", jitter=0.05)

# by variable dot plot with custom colors, keeps only 1 shape
ScatterPlot(x, by=Gender, col.stroke=c("steelblue", "hotpink"))

# bubble plot of simulated Likert data, 1 to 7 scale
# size of each plotted point (bubble) depends on its joint frequency
# triggered by default when  < 10 unique values for each variable
x1 <- sample(1:7, size=100, replace=TRUE)
x2 <- sample(1:7, size=100, replace=TRUE)
ScatterPlot(x1,x2)

# compare to usual scatterplot of Likert data, transparency helps
plot(x1,x2)
ScatterPlot(x1,x2, kind="regular", cex=3)

# plot Likert data and get sunflower plot with loess line
ScatterPlot(x1,x2, kind="sunflower", fit.line="loess")

# scatterplot of continuous Y against categorical X, a factor
Pain <- sample(c("None", "Some", "Much", "Massive"), size=25, replace=TRUE)
Pain <- factor(Pain, levels=c("None", "Some", "Much", "Massive"), ordered=TRUE)
Cost <- round(rnorm(25,1000,100),2)
ScatterPlot(Pain, Cost)

# for this purpose, improved version of standard R stripchart
stripchart(Cost ~ Pain, vertical=TRUE)

# function curve
x <- seq(10,500,by=1) 
y <- 18/sqrt(x)
# x is sorted with equal intervals so type set to "l" for line
# can use Plot or ScatterPlot, here Plot seems more appropriate
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
# although data not attached, access the variable directly by its name
data(dataEmployee)
ScatterPlot(Years, Salary, by=Gender, data=dataEmployee)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ dotplot }
\keyword{ color }
\keyword{ grouping variable }




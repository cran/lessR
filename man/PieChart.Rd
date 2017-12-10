\name{PieChart}
\alias{PieChart}
\alias{pc}

\title{Pie Chart}

\description{
Abbreviation: \code{pc}

Plots a pie chart of a categorical variable. The default chart is a doughnut or ring version of a pie chart, that is, a hole in the middle of the pie. For counts, either entered directly or tabulated, also displays the frequency table for the variable with the corresponding chi-square inferential analysis. Real numbers can also be entered directly.
}

\usage{
PieChart(x, y=NULL, data=mydata, 

         radius=1, hole=0.65, hole.fill=getOption("panel.fill"),

         fill=NULL, low.fill=NULL, hi.fill=NULL,
         colors=c("rainbow", "terrain", "heat"),
         random.color=FALSE,

         clockwise=FALSE, init.angle=ifelse (clockwise, 90, 0), 
         density=NULL, angle=45, border="black", lty="solid",
         edges=200, 

         main=NULL, cex=1, cex.main=1,

         quiet=getOption("quiet"),
         width=5, height=5, pdf.file=NULL,
         \dots)

pc(\ldots)
}

\arguments{
  \item{x}{For each level of this variable, x, display the frequencies.}
  \item{y}{Numeric variable that sets the areas of the pie. If not specified,
        then its value is the frequency of each category of \code{x},
        automatically tabulated.}
  \item{data}{Optional data frame that contains the variable(s) of interest,
        default is \code{mydata}.}\cr

  \item{radius}{The pie is drawin in a box with sides that range from -1 to 1,
        so the maximum value of the radius without truncating the pie is 1.}
  \item{hole}{The proportion of the radius that defines the inner hole
        for what is called a doughnut or hole plot. To show the full pie,
        set to \code{FALSE}.}
  \item{hole.fill}{Fill color of the hole, which is the same color as
       \code{panel.fill} as set by the color theme or individually with
       the \code{\link{style}} function.} \cr


  \item{fill}{Specified color of each slice. The
       \code{lessR} function \code{\link{showColors}} provides examples of all
       R named colors.}
  \item{low.fill}{Only when the variable is an ordered factor, sets the color
        for the lowest level of the factor in the resulting ordered progression
        of colors.}
  \item{hi.fill}{Only when the variable is an ordered factor, sets the color
        for the highest level of the factor in the resulting ordered progression
         of colors.}
  \item{colors}{Optional palettes that provide more options.}
  \item{random.color}{Randomizes the order of the colors within the chosen color
        palette.
        When \code{TRUE}, each call of the same function call generally yields
        different colors of the slices}\cr

  \item{clockwise}{Default value of \code{FALSE} specifies to draw the
        slices counter-clockwise, otherwise clockwise.}
  \item{init.angle}{Starting angle (in degrees) for the slices. For counter-clockwise
       the default value is 0 (3 o'clock), otherwise 90 (12 o'clock).}
  \item{density}{Density of shading lines, in lines per inch. Default value is
       \code{NULL}, that is, no shading lines.}
  \item{angle}{Angle of shading lines in degrees.}
  \item{border}{Border color of slides and the pie, can be a vector to customize
        the color for each slice.}
  \item{lty}{Type of line for each slice, such as "solid", the default.
        Can be a vector.}
  \item{edges}{Approximation of a circle with a polygon of specified edges.}\cr


  \item{main}{Title of graph. Set the color with \code{main.color} with
       the \code{\link{style}} function.}
  \item{cex}{Magnification factor of labels relative to 1.}
  \item{cex.main}{Magnification factor of title relative to 1.}\cr

  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf.file}{Name of the pdf file to if graphics to be redirected to a
        pdf file.}\cr

  \item{...}{Other parameter values for graphics as defined processed 
      by \code{\link{pie}} and \code{\link{par}} for general graphics,
      which includes \code{radius} of the pie, and \code{color.main} for
      the title of the graph.}
}


\details{
OVERVIEW\cr
Plot a pie chart with default colors, presumably with a relatively small number of values for each variable.  By default, colors are selected for the slices, background and grid lines, all of which can be customized. The basic computations of the chart are provided with the standard R functions \code{\link{pie}} and \code{\link{chisq.test}} and the \code{lessR} function  \code{\link{chisq.test}}. A minor modification of the original \code{\link{pie}} code provides for the hole in the middle of the pie,, the default doughnut or ring chart.

COLORS\cr
1. For gray scale, such as for the default \code{lightbronze} color theme as well as the \code{gray} and \code{white} color themes, colors are automatically generated from the lightest to the darkest displayed levels of gray. For other color themes a range of up to 8 hues are displayed for the slices based on the HCL colorspace with a chroma of 50 and a luminance of 70, so as to have the colors at equal perceived brightness. Three more built-in R color palettes are also available by setting \code{color} to one of \code{"rainbow"}, \code{"heat"} and \code{"terrain"}.  The most vivid of all the palettes is \code{"rainbow"}.\cr
2. The desired colors can be explicitly specified with the \code{fill} option, which overrides any other color options. When plotting one variable, include one color in this color list, the color used for all of the slices  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in\cr \code{fill=c("coral3","seagreen3")}.

When plotting ordered factor then neither of the two standard color palettes are used.  Instead, the resulting slice colors for each level of the ordered factor are also ordered in a progression of colors. The default progression is based on the first color of either the regular, vivid or gray color palettes, but this can be changed with the \code{low.fill} and \code{hi.fill} options, or individually specify the color of each piece with the \code{fill} option. A specified palette can, for example, be from light to dark of the same hue, or from a light color of one hue to a dark color of another hue. Each color value can be specified with a color name, or with a specification with the \code{\link{rgb}} function.  See the examples below.  

Use the \code{\link{showColors}} function in this package to get, for each color:  name,  sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values. Or use \code{\link{showWheel}} to get color palates of colors in the HCL color space to maintain equal perceived brightness across the colors.

STATISTICS\cr
In addition to the pie chart, descriptive and optional inferential statistics are also presented.  First, for integer variables such as counts, the frequency table with proportions is displayed. Second, the corresponding chi-square test is also displayed. For real valued variables read from a data frame, the summary statistics such as the mean are reported.

PDF OUTPUT\cr
Because \code{lessR} functions generate their own graphics calls, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > PieChart(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > PieChart(Y)     # directly reference Y}
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{pie}}, \code{\link{chisq.test}}.
}


\examples{
# ---------------------------------------------------------
# generate some data in data frame mydata for two variables 
# ---------------------------------------------------------

# Pain is an ordered factor, Gender is an unordered factor
# Place in data frame mydata to simulate reading with rad
Pain <- sample(c("None", "Some", "Much", "Massive"), size=50, replace=TRUE)
Pain <- factor(Pain, levels=c("None", "Some", "Much", "Massive"), ordered=TRUE)
Gender <- sample(c("Male", "Female"), size=50, replace=TRUE)
Gender <- factor(Gender)
mydata <- data.frame(Pain, Gender)
rm(Pain); rm(Gender)


# --------------------------------------------------------
# pie (doughnut) chart from the data for a single variable
# --------------------------------------------------------

# for each level of Pain, display the frequencies
# Pain is an ordered factor, so the slice colors are ordered
PieChart(Pain)
# short name
pc(Pain)
# compare to standard R pie chart, which requires mydata$ reference
pie(table(mydata$Pain))

# standard pie chart with no hole
pc(Pain, hole=0)

# Gender is unordered, so a different color for each slice
PieChart(Gender)

# specify a unique slice color for each of the two slices
PieChart(Gender, fill=c("pink","lightblue"))

# specify the colors from the R palette rainbow.colors
PieChart(Gender, colors="rainbow")


# ------------------------------
# pie chart directly from counts
# ------------------------------

# from vector
# pie chart of one variable with three levels
# enter counts as a vector with the combine function, c
# must supply the level names and variable name
City <- c(206, 94, 382)
names(City) <- c("LA","Chicago","NY")
pc(City, main="Employees in Each City")

# counts from data frame
x <- c("ACCT", "ADMN", "FINC", "MKTG", "SALE")
y <- c(5, 6, 4, 6, 15)
mydata <- data.frame(x,y)
names(mydata) <- c("Dept", "Count")
PieChart(Dept, Count)

# real numbers from data frame 
x <- c("ACCT", "ADMN", "FINC", "MKTG", "SALE")
y <- c(86208.42, 29808.29, 42305.52, 75855.81, 65175.51)
mydata <- data.frame(x,y)
names(mydata) <- c("Dept", "Salary")
pc(Dept, Salary)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pie chart }
\keyword{ color }





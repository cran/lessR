\name{pc}
\alias{pc}

\title{Pie Chart}

\description{
Plots a pie chart with default colors from a variety of different types of data. Also displays the frequency table for the variable and provides the corresponding chi-square inferential analysis.
}

\usage{
pc(x, dframe=mydata, 
        colors=c("relaxed", "vivid", "gray", "rainbow", "terrain", "heat"),
        random.col=FALSE,
        col.slices=NULL, col.low=NULL, col.hi=NULL,
        text.out=TRUE, main=NULL, \ldots)
}

\arguments{
  \item{x}{For each level of this variable, x, display the frequencies.}
  \item{dframe}{Optional data frame that contains the variable(s) of interest, default is 
        \code{mydata}.}
  \item{colors}{Sets the intensity of the default color palette for the slices and the 
        background, when the second variable is not ordered, otherwise is ignored.  The default of
        \code{FALSE} sets more pastel colors.}
  \item{random.col}{Randomizes the order of the colors within the chosen color palette, when 
        the second variable is not ordered, otherwise is ignored. When \code{TRUE},
        each run of the same function call generally yields different colors of the slices}
  \item{col.slices}{Specified color of each slice.}
  \item{col.low}{Only when the variable is an ordered factor, sets the color for the lowest level 
        of the factor in the resulting ordered progression of colors.}
  \item{col.hi}{Only when the variable is an ordered factor, sets the color for the highest level 
        of the factor in the resulting ordered progression of colors.}
  \item{text.out}{If \code{TRUE}, then display frequency table, chi-square analysis and sample size information.}
  \item{main}{Title of graph.}
  \item{\dots}{Other parameter values for graphics as defined 
      by \code{\link{pie}} and  \code{\link{par}} including \code{col.main}, etc.}
}


\details{
OVERVIEW\cr
Plot a pie chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the slices, background and gridlines, all of which can be customized. The basic computations of the chart are provided with the standard R functions \code{\link{pie}} and \code{\link{chisq.test}} and the \code{lessR} function  \code{\link{chisq.test}}.

COLORS\cr
There are three ways to override the default colors.\cr
1. There are two pre-defined color palettes, each with 7 colors.  The default palette provides lighter, more pastel colors.  The vivid palette, activated by \code{color="vivid"}, provides brighter colors with a brighter background (cornsilk1). A third color palette, set by \code{color="gray"}, provides an ordered gray scale. Three more built-in R color palettes are also available by setting \code{color} to one of \code{"rainbow"}, \code{"heat"} and \code{"terrain"}.  The most vivid of all the palettes is \code{"rainbow"}.\cr
2. The order of the colors within the chosen palette can be randomized with the \code{random.col="TRUE"} option. For example, when this option is activated each of the seven colors in a palette has a 1/7 chance of appearing as the first color, the only color used in the plot of a single variable. When invoked for a  \code{colors="gray"}, the order from light to dark will generally be lost, which may be desirable if the categories do not represent an ordered factor.\cr
3. The desired colors can be explicitly specified with the \code{col.pieces} option, which overrides any other color options. When plotting one variable, include one color in this color list, the color used for all of the slices  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in\cr \code{col.pieces=c("coral3","seagreen3")}.

The default colors in one of the two provided color palettes can be viewed, in the order in which they are displayed, by running the corresponding two lines of R code, first for the default colors and second for the vivid colors:\cr\cr
clr <- c("slategray3", "bisque3", "darksalmon", "darkolivegreen3", "thistle", "azure3", "moccasin")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)\cr
\cr
clr <- c("coral3", "seagreen3", "maroon3", "dodgerblue3", "purple3", "turquoise3", "yellow3")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)

When plotting ordered factor then neither of the two standard color palettes are used.  Instead, the resulting slice colors for each level of the ordered factor are also ordered in a progression of colors. The default progression is based on the first color of either the regular, vivid or gray color palettes, but this can be changed with the \code{col.low} and \code{col.hi} options, or individually specify the color of each piece with the \code{col.piece} option. A specified palette can, for example, be from light to dark of the same hue, or from a light color of one hue to a dark color of another hue. Each color value can be specified with a color name, or with a specification with the \code{\link{rgb}} function.  See the examples below.  

Use the \code{\link{color.show}} function in this package to get, for each color:  name,  sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values.

STATISTICS\cr
In addition to the pc, descriptive and optional inferential statistics are also presented.  First, the frequency table with proportions is displayed. Second, the corresponding chi-square test is also displayed.

VARIABLE LABELS\cr
A labels data frame named \code{mylabels}, obtained from the \code{\link{rad}} function, can provide the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the corresponding variable label is listed as the lable for the horiztonal axis unless xlab is specified in the function call. If there are two variables to plot, the title of the resulting plot is based on the two variable labels, unless a specific title is listed with the \code{main} option. The varible label is also listed in the text output, next to the variable name. If the analysis is for two variables, then labels for both variables are included. 
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
Pain <- sample(c("None", "Some", "Much", "Massive"), size=25, replace=TRUE)
Pain <- factor(Pain, levels=c("None", "Some", "Much", "Massive"), ordered=TRUE)
Gender <- sample(c("Male", "Female"), size=25, replace=TRUE)
Gender <- factor(Gender)
mydata <- data.frame(Pain, Gender)
rm(Pain); rm(Gender)


# --------------------------------------------
# pie chart from the data for a single variable
# --------------------------------------------

# for each level of Pain, display the frequencies
# Pain is an ordered factor, so the slice colors are ordered
pc(Pain)
# compare to standard R pie chart, which requires mydata$ reference
pie(table(mydata$Pain))

# Gender is unordered, so a different color for each slice
pc(Gender)

# specify a unique slice color for each of the two slices
pc(Gender, col.slices=c("pink","lightblue"))

# specify the colors from the R palatte rainbow.colors
pc(Gender, colors="rainbow")


# ------------------------------
# pie chart directly from counts
# ------------------------------

# pie chart of one variable with three levels
# enter counts as a vector with the combine function, c
# must supply the level names and variable name
City <- c(206, 94, 382)
names(City) <- c("LA","Chicago","NY")
pc(City, main="Employees in Each City")

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pie chart }
\keyword{ color }

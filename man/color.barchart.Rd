\name{color.barchart}
\alias{color.barchart}
\alias{color.barchart.default}
\alias{color.barchart.data.frame}

\title{Bar Chart of One or Two Variables with Color}

\description{
Using the standard R functions \code{\link{barplot}},  \code{\link{chisq.test}} and, for two variables, \code{\link{legend}}, plots a bar chart of one or two variables with default colors, including background color and gridlines in addition to the bars. Also displays the frequency table for one or two variables and optionally provides the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies.

For the analysis of a single variable, the cell proportions are also provided.  For the analysis of two variables, also provided are the proportions within each cell, within each column and within each row.   

Unlike \code{\link{barplot}}, the variable(s) can be entered directly into the function call without first converting to a table.  If two variables are plotted, a legend is automatically provided.

If the provided object for which to calculate the bar chart is a data frame, then a bar chart is calculated for each non-numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.}

\usage{
color.barchart(x=NULL, \ldots)

\method{color.barchart}{data.frame}(x, \dots)

\method{color.barchart}{default}(x, y=NULL, col.bars=NULL, border="black", 
         col.bg="ghostwhite", col.grid="grey90", beside=TRUE, 
         over.grid=FALSE, prop=FALSE, xlab=NULL, legend.title=NULL, 
         legend.loc=NULL, legend.labels=NULL, legend.horiz=FALSE, 
         vivid=FALSE, random.col=FALSE, col.low="slategray2", 
         col.hi="slategray4", addtop=1, horiz=FALSE, chisq=FALSE, \ldots)
}

\arguments{
  \item{x}{For each level of this variable, x, display the frequencies.}
  \item{y}{For each level of the first variable, x, display the frequencies at each 
           level of this second variable, y.}
  \item{col.bars}{Specified bar colors.}
  \item{border}{Color of the border of the bars.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{beside}{When plotting two variables, set to true for the levels fo the first 
        variable to be plotted as adjacent bars instead of stacked on each other. Unlike the standard R function,\code{\link{barplot}}, the default is TRUE. }
  \item{over.grid}{If true, plot the grid lines over the histogram.}
  \item{prop}{Specify proportions or relative frequencies on the vertical axis. Default
       is \code{FALSE}. Only valid for the analysis of a single variable.}
  \item{xlab}{Label for x-axis. Defaults to variable name.}
  \item{legend.title}{Title of the legend, which is usually set by default except when
        raw counts are entered as a matrix.  Then a title must be specified to generate a legend.}
  \item{legend.loc}{When plotting two variables, location of the legend. For vertical bars, usually choose
        from among "topleft", "top", and "topright". See help for \code{\link{legend}} function for more options.}
  \item{legend.labels}{When plotting two variables, labels for the legend, which by default
        are the levels for the first variable.}
  \item{legend.horiz}{By default the legend is vertical, but can be changed to horizontal.}
  \item{vivid}{Sets the intensity of the default color palette for the bars and the 
        background, when the second variable is not ordered, otherwise is ignored.  The default of \code{"FALSE"} sets more pastel colors.}
  \item{random.col}{Randomizes the order of the colors within the chosen color palette, when 
        the second variable is not ordered, otherwise is ignored. When \code{TRUE},
        each run of the same function call generally yields different colors of the bars.}
  \item{col.low}{Only when the second variable is an ordered factor, sets the color for the lowest level 
        of the factor in the resulting ordered progression of colors.}
  \item{col.hi}{Only when the second variable is an ordered factor, sets the color for the highest level 
        of the factor in the resulting ordered progression of colors.}
  \item{horiz}{By default bars are vertical, but can set this option to TRUE.}
  \item{addtop}{In the same scale as the vertical axis, puts more space between the bars 
        and the top of the plot area, usually to accomodate the legend when plotting two 
        variables.}
  \item{chisq}{Provide a chi-square analysis. Default is \code{FALSE}.}
  \item{\dots}{Other parameter values for graphics as defined 
      by \code{\link{barplot}}, \code{\link{legend}}, and  \code{\link{par}} including \code{space} for one variable only,
      and \code{cex.lab}, \code{col.main}, etc.}
}


\details{
OVERVIEW\cr
Plot a bar chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the bars, background and gridlines, all of which can be customized.

A frequency bar is produced for each level of the first and perhaps only variable. When two variables are plotted, the bar for each level of the first variable is replaced with a group of bars, one for each level of the second variable.

The form of the entered data, the first variable x and optionally a second variable, y, is flexible.  The data may be entered as factors, numeric values, characters, a matrix or even as a table as with the standard R \code{\link{barplot}} function. The data may be entered and the resulting frequencies computed, or the frequencies can be entered directly.  The most natural type of data to enter, when entering the variables, is to enter factors.  Plus, the bar colors for a second variable which is an ordered factor are also ordered in a corresponding progression.

COLORS\cr
There are three ways to override the default colors.\cr
1. There are two pre-defined color palettes, each with 7 colors.  The default palette provides lighter, more pastel colors.  The vivid palette, activated by the \code{vivid=TRUE} option, provides brighter colors with a brighter background (cornsilk1).\cr
2. The order of the colors within the chosen palette can be randomized with the \code{random.col="TRUE"} option. For example, when this option is activated each of the seven colors in a palette has a 1/7 chance of appearing as the first color, the only color used in the plot of a single variable, and the color used for the first bar in each group for a plot of two variables.\cr
3. The desired colors can be explicitly specified with the \code{col.bars} option, which overrides any other bar color options. When plotting one variable, include one color in this color list, the color used for all of the bars.  When plotting two variables, usually the list of colors includes the same number of elements as the number of levels for the second variable.  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in \code{col.bars=c("coral3","seagreen3")} for a second variable with two levels. When two variables are plotted, if there are fewer specified colors than the levels of the second variable, the remaining colors are selected from the remaining colors in the activated palette.

The colors in one of the two provided color palettes can be viewed, in the order in which the are by default displayed, by running the corresponding two lines of R code, first for the default colors and second for the vivid colors:\cr\cr
clr <- c("slategray3", "bisque3", "darksalmon", "darkolivegreen3", "thistle", "azure3", "moccasin")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)\cr
\cr
clr <- c("coral3", "seagreen3", "maroon3", "dodgerblue3", "purple3", "turquoise3", "yellow3")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)

When plotting one ordered factor, or when plotting two variables, and the second variable is an ordered factor, then neither of the two standard color palettes are used.  Instead, the resulting bar colors for each level of the ordered factor are also ordered in a progression of colors. The default progression is from light to dark shades of red, but this can be changed with the \code{col.low} and \code{col.hi} options, or individually specify the color of each bar with the \code{col.bars} option. A specified palette can, for example, be from light to dark of the same hue, or from a light color of one hue to a dark color of another hue. Each color value can be specified with a color name, or with a specification with the \code{\link{rgb}} function.  See the examples below.  

Use the \code{\link{color.show}} function in this package to get, for each color:  name,  sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values.

LEGEND\cr
When two variables are plotted, a legend is produced, with values for each level of the second variable.  By default, the location is top center. This position can be changed with the \code{legend.loc} option, which accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend. Sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  This option only applies for the default vertical bars. Also, the legend is displayed vertically by default, but can be changed to horizontal with the \code{legend.horiz} option.

ENTER COUNTS DIRECTLY\cr
Instead of calculating the counts from the data, the counts can be entered directly.  For two variables, enter the counts as a matrix and include the \code{xlab} option to label the horizontal axis, such as with the name of the variable.  Also include the \code{legend.title} option to provide a legend.  See the examples below.

STATISTICS\cr
In addition to the barchart, descriptive and optional inferential statistics are also presented.  First, the frequency table for one variable or the joint frequency table for two variables is displayed. Second, the corresponding chi-square test is also displayed if the \code{chisq} option is set to \code{TRUE}.
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{barplot}}, \code{\link{legend}}.
}


\examples{
# --------------------------------------------
# barchart from the data for a single variable
# --------------------------------------------

# generate some random data values for two variables
# Pain is an ordered factor, Gender is an unordered factor
Pain <- sample(c("None", "Some", "Much", "Massive"), size=25, replace=TRUE)
Pain <- factor(Pain, levels=c("None", "Some", "Much", "Massive"), ordered=TRUE)
Gender <- sample(c("Male", "Female"), size=25, replace=TRUE)
Gender <- factor(Gender)

# for each level of Pain, display the frequencies
# Pain is an ordered factor, so the bar colors are ordered
color.barchart(Pain)

# Gender is unordered, so one color used for all the bars
color.barchart(Gender)

# specify a unique bar color for each of the two bars
# also display narrower bars, the default space between is .2
color.barchart(Gender, col.bars=c("rosybrown","tan"), space=.6)


# ----------------------------------------
# barchart from the data for two variables
# ----------------------------------------

# at each level of Pain, show the frequencies of the Gender levels
color.barchart(Pain, Gender, chisq=TRUE)

# at each level of Pain, show the frequencies of the Gender levels
# display more vivid colors, randomize from the palette the bar colors
color.barchart(Pain, Gender, vivid=TRUE, random.col=TRUE)

# at each level of Gender, show the frequencies of the Pain levels
# Pain levels are ordered, so the corresponding colors are also ordered 
color.barchart(Gender, Pain)

# specify an ordered blue palette of colors for the ordered levels of Pain
# only works when the variable is an ordered factor
# colors can be named or customized with rgb function
color.barchart(Gender, Pain, col.low="azure", col.hi=rgb(100,110,200,max=255))

# define custom color gradient within each group of bars
# applies to both ordered and unordered factors
color.barchart(Gender, Pain, col.bars=c("thistle1","thistle2","thistle3","thistle4"))

# display only two stacked bars, Female and Male
# the levels of Pain are included within each respective bar
color.barchart(Gender, Pain, beside=FALSE, legend.horiz=TRUE, addtop=5)

# horizontal bar chart of two variables
color.barchart(Gender, Pain, horiz=TRUE, legend.loc="bottomright")

# many options, including those from par: col.main, col.axis, col.lab, cex.lab
# for more info on these graphic options, enter:  help(par)
color.barchart(Pain, Gender, col.bars=c("coral3","seagreen3"), 
  legend.loc="topleft", legend.labels=c("The Girls", "The Boys"), 
  xlab="Pain Level", main="Gender for Different Pain Levels", 
  col.bg="wheat1", col.grid="wheat3", col.main="wheat4", 
  col.axis="wheat4", col.lab="wheat4", cex.lab=1.2)
  

# ----------------------------
# can enter many types of data
# ----------------------------

# generate and enter integer data
X1 <- sample(4, size=100, replace=TRUE)
X2 <- sample(4, size=100, replace=TRUE)
color.barchart(X1)
color.barchart(X1,X2)

# generate and enter character string data
# that is, without first converting to a factor
Travel <- sample(c("Bike", "Bus", "Drive", "Walk"), size=25, replace=TRUE)
color.barchart(Travel)

# can enter the table counts directly, as in standard R barplot function
# provide a label for the different levels
color.barchart(table(Pain), xlab="Pain")


# ---------------------------------------------
# multiple bar charts across multiple variables
# ---------------------------------------------

# read data into data frame called mydata
rad("http://web.pdx.edu/~gerbing/data/employees2.csv")

# bar charts for all non-numeric variables in the data frame called mydata
color.barchart()

# bar charts for all numeric variables in the data frame called mydata
#  written to the file indicated on output
color.barchart(col.box="palegreen1", col.point="plum")

# Use the subset function to specify a variable list
color.barchart(subset(mydata, select=c(Gender,Dept)))


# ------------------------------
# bar chart directly from counts
# ------------------------------

# barchart of one variable with three levels
# enter counts as a vector with the combine function, c
# must supply the level names and variable name
City <- c(206, 94, 382)
names(City) <- c("LA","Chicago","NY")
color.barchart(City, main="Employees in Each City")

# barchart of two variables
# two Quality levels, the rows
# three Supplier levels, the columns
# enter counts row by row, then form the table with rbind function
# must supply the level (value) names and variable names
# chart is Row Variable, analyzed at each level of Column Variable
row1 <- c(19, 16, 23) 
row2 <- c(6, 6, 8) 
mytable <- rbind(row1, row2)
rownames(mytable) <- c("Pass", "Defective")
colnames(mytable) <- c("Acme, Inc", "Nuts, Inc", "Bolts, Inc")
color.barchart(mytable, xlab="Supplier", legend.title="Quality")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }

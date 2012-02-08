\name{bc}
\alias{bc}
\alias{bc.default}
\alias{bc.data.frame}
\alias{color.barchart}

\title{Bar Chart of One or Two Variables with Color}

\description{
Plots a bar chart of one or two variables with default colors, including background color and gridlines, from a variety of different types of data. Also displays the frequency table for one or two variables and optionally provides the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies.

For the analysis of a single variable, the cell proportions are also provided unless there are more than 10 unique values  If all values are unique, then only the value names are listed.  For the analysis of two variables, also provided are the proportions within each cell, within each column and within each row.   

Unlike the standard R function, \code{\link{barplot}}, the variable(s) can be entered directly into the function call without first converting to a table.  If two variables are plotted, a legend is automatically provided.

If the provided object for which to calculate the bar chart is a data frame, then a bar chart is calculated for each non-numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{
bc(x=NULL, by=NULL, dframe=mydata, ncut=4, \ldots)

\method{bc}{data.frame}(x, ncut, \dots)

\method{bc}{default}(x, by=NULL,

         col.bars=NULL, col.border="black", col.bg="ghostwhite",
         col.grid="grey86", random.col=FALSE,
         colors=c("relaxed", "vivid", "gray", "rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, brief=TRUE, prop=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL,

         beside=FALSE, col.low=NULL, col.hi=NULL,

         legend.title=NULL, legend.loc=NULL, legend.labels=NULL,
         legend.horiz=FALSE,
         
         chisq=TRUE, graph=TRUE, \ldots)

color.barchart(\ldots)
}

\arguments{
  \item{x}{For each level of this variable, x, display the frequencies.}
  \item{by}{For each level of the first variable, x, display the frequencies at each 
           level of this second variable, y.}
  \item{dframe}{Optional data frame that contains the variable(s) of interest, default is 
        \code{mydata}.}
  \item{ncut}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be anlayzed as a categorical. Set to 0 to turn off.}
  \item{col.bars}{Specified bar colors.}
  \item{col.border}{Color of the border of the bars. Specify NA to remove the border}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{colors}{Sets the intensity of the default color palette for the bars and the 
        background, when the second variable is not ordered, otherwise is ignored.  The default of
        \code{FALSE} sets more pastel colors.}
  \item{random.col}{Randomizes the order of the colors within the chosen color palette, when 
        the second variable is not ordered, otherwise is ignored. When \code{TRUE},
        each run of the same function call generally yields different colors of the bars.}
  \item{horiz}{By default bars are vertical, but can set this option to \code{TRUE}.}
  \item{over.grid}{If \code{TRUE}, plot the grid lines over the histogram.}
  \item{addtop}{When \code{horiz=FALSE}, in the same scale as the vertical axis, puts more space 
        between the bars and the top of the plot area, usually to accommodate the legend when 
        plotting two variables.}
  \item{gap}{Gap between bars.  Provides the value of the \code{space} option from the
        standard R \code{\link{barplot}} function with a default of 0.2 unless two variables
        are plotted and beside=\code{TRUE}, in which case the default is c(.1,1).}
  \item{brief}{If \code{FALSE}, then the tables of conditional proportions are displayed.}
  \item{prop}{Display proportions instead raw frequencies.}
  \item{xlab}{Label for x-axis, more generally the label for the values which could be on 
        the vertical axis for a two variable plot if \code{horiz=TRUE}. Defaults to variable name.}
  \item{ylab}{Label for y-axis, more generally the frequency axis. Defaults to Frequency.}
  \item{main}{Title of graph.}
  \item{beside}{When plotting two variables, set to \code{TRUE} for the levels fo the first 
        variable to be plotted as adjacent bars instead of stacked on each other.}
  \item{col.low}{Only when the variable is an ordered factor, sets the color for the lowest level 
        of the factor in the resulting ordered progression of colors.}
  \item{col.hi}{Only when the variable is an ordered factor, sets the color for the highest level 
        of the factor in the resulting ordered progression of colors.}
  \item{legend.title}{Title of the legend, which is usually set by default except when
        raw counts are entered as a matrix.  Then a title must be specified to generate a legend.}
  \item{legend.loc}{When plotting two variables, location of the legend. For vertical bars, 
        usually choose from among "topleft", "top", and "topright". See help for \code{\link{legend}} 
        function for more options.}
  \item{legend.labels}{When plotting two variables, labels for the legend, which by default
        are the levels for the second or \code{by} variable.}
  \item{legend.horiz}{By default the legend is vertical, but can be changed to horizontal.}
  \item{chisq}{Provide a chi-square analysis.}
  \item{graph}{Provide a graph. Default is \code{TRUE}.}
  \item{\dots}{Other parameter values for graphics as defined 
      by \code{\link{barplot}}, \code{\link{legend}}, and  \code{\link{par}} including \code{space} for one variable only,
      and \code{cex.lab}, \code{col.main}, etc.}
}


\details{
OVERVIEW\cr
Plot a bar chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the bars, background and gridlines, all of which can be customized. The basic computations of the chart are provided with the standard R functions \code{\link{barplot}},  \code{\link{chisq.test}} and, for two variables, \code{\link{legend}}.

A frequency bar is produced for each level of the first and perhaps only variable. When two variables are plotted, the bar for each level of the first variable is replaced with a group of bars, one for each level of the second variable.

The form of the entered data, the first variable x and optionally a second variable, y, is flexible.  The data may be entered as factors, numeric values, characters, or a matrix. The data may be entered and the resulting frequencies computed, or the frequencies can be entered directly.  The most natural type of data to enter, when entering the variables, is to enter factors.  Plus, the bar colors for a second variable which is an ordered factor are also ordered in a corresponding progression.

DATA FRAME ACCESS\cr
If the variable is in a data frame, the input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the mydata$name notation. If two variables are specified, both variables should be in the data frame, or one of the variables is in the data frame and the other in the global environment. 

COLORS\cr
There are three ways to override the default colors.\cr
1. There are two pre-defined color palettes, each with 7 colors.  The default palette provides lighter, more pastel colors.  The vivid palette, activated by \code{colors="vivid"}, provides brighter colors with a brighter background (cornsilk1). A third color palette, set by \code{colors="gray"}, provides an ordered gray scale. Three more built-in R color palettes are also available by setting \code{colors} to one of \code{"rainbow"}, \code{"heat"} and \code{"terrain"}.  The most vivid of all the palettes is \code{"rainbow"}.\cr
2. The order of the colors within the chosen palette can be randomized with the \code{random.col="TRUE"} option. For example, when this option is activated each of the seven colors in a palette has a 1/7 chance of appearing as the first color, the only color used in the plot of a single variable, and the color used for the first bar in each group for a plot of two variables. When invoked for a  \code{color="gray"}, the order from light to dark will generally be lost, which may be desirable if the categories do not represent an ordered factor.\cr
3. The desired colors can be explicitly specified with the \code{col.bars} option, which overrides any other bar color options. When plotting one variable, include one color in this color list, the color used for all of the bars.  When plotting two variables, usually the list of colors includes the same number of elements as the number of levels for the second variable.  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in\cr \code{col.bars=c("coral3","seagreen3")} for a second variable with two levels. When two variables are plotted, if there are fewer specified colors than the levels of the second variable, the remaining colors are selected from the remaining colors in the activated palette.

The default colors in one of the two provided color palettes can be viewed, in the order in which they are displayed, by running the corresponding two lines of R code, first for the default colors and second for the vivid colors:\cr\cr
clr <- c("slategray3", "bisque3", "darksalmon", "darkolivegreen3", "thistle", "azure3", "moccasin")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)\cr
\cr
clr <- c("coral3", "seagreen3", "maroon3", "dodgerblue3", "purple3", "turquoise3", "yellow3")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)

When plotting one ordered factor, or when plotting two variables, and the second variable is an ordered factor, then neither of the two standard color palettes are used.  Instead, the resulting bar colors for each level of the ordered factor are also ordered in a progression of colors. The default progression is based on the first color of either the regular, vivid or gray color palettes, but this can be changed with the \code{col.low} and \code{col.hi} options, or individually specify the color of each bar with the \code{col.bars} option. A specified palette can, for example, be from light to dark of the same hue, or from a light color of one hue to a dark color of another hue. Each color value can be specified with a color name, or with a specification with the \code{\link{rgb}} function.  See the examples below.  

Use the \code{\link{color.show}} function in this package to get, for each color:  name,  sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values.

LEGEND\cr
When two variables are plotted, a legend is produced, with values for each level of the second variable.  By default, the location is top center. This position can be changed with the \code{legend.loc} option, which accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend. Sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  This option only applies for the default vertical bars. Also, the legend is displayed vertically by default, but can be changed to horizontal with the \code{legend.horiz} option.

ENTER COUNTS DIRECTLY\cr
Instead of calculating the counts from the data, the counts can be entered directly.  For two variables, enter the counts as a matrix and include the \code{xlab} option to label the horizontal axis, such as with the name of the variable.  Also include the \code{legend.title} option to provide a legend.  See the examples below.

STATISTICS\cr
In addition to the barchart, descriptive and optional inferential statistics are also presented.  First, the frequency table for one variable or the joint frequency table for two variables is displayed. Second, the corresponding chi-square test is also displayed by default. If \code{brief=FALSE}, then the cell proportions are displayed, as well as based on row sums and column sums.

VARIABLE LABELS\cr
A labels data frame named \code{mylabels}, obtained from the \code{\link{rad}} function, can provide the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the corresponding variable label is listed as the lable for the horiztonal axis unless xlab is specified in the function call. If there are two variables to plot, the title of the resulting plot is based on the two variable labels, unless a specific title is listed with the \code{main} option. The varible label is also listed in the text output, next to the variable name. If the analysis is for two variables, then labels for both variables are included. 
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{barplot}}, \code{\link{legend}}.
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
# barchart from the data for a single variable
# --------------------------------------------

# for each level of Pain, display the frequencies
# Pain is an ordered factor, so the bar colors are ordered
bc(Pain)
# compare to standard R bar plot, which requires mydata$ reference
barplot(table(mydata$Pain))

# Gender is unordered, so one color used for all the bars
bc(Gender)

# specify a unique bar color for each of the two bars
bc(Gender, col.bars=c("pink","lightblue"))


# ----------------------------------------
# barchart from the data for two variables
# ----------------------------------------

# at each level of Pain, show the frequencies of the Gender levels
bc(Pain, by=Gender)

# at each level of Pain, show the frequencies of the Gender levels
# display more vivid colors, randomize from the palette the bar colors
bc(Pain, by=Gender, colors="vivid", random.col=TRUE)

# at each level of Gender, show the frequencies of the Pain levels
# Pain levels are ordered, so the corresponding colors are also ordered 
bc(Gender, by=Pain)

# specify an ordered blue palette of colors for the ordered levels of Pain
# only works when the variable is an ordered factor
# colors can be named or customized with rgb function
bc(Gender, by=Pain, col.low="azure", col.hi=rgb(100,110,200,max=255))

# define custom color gradient within each group of bars
# applies to both ordered and unordered factors
bc(Gender, by=Pain, col.bars=c("thistle1","thistle2","thistle3","thistle4"))

# display bars beside each other instead of stacked, Female and Male
# the levels of Pain are included within each respective bar
bc(Gender, by=Pain, beside=TRUE, legend.horiz=TRUE)

# horizontal bar chart of two variables
bc(Gender, by=Pain, horiz=TRUE, legend.loc="bottomright")

# many options, including those from par: col.main, col.axis, col.lab, cex.lab
# for more info on these graphic options, enter:  help(par)
bc(Pain, by=Gender, col.bars=c("coral3","seagreen3"), 
  legend.loc="topleft", legend.labels=c("The Girls", "The Boys"), 
  xlab="Pain Level", main="Gender for Different Pain Levels", 
  col.bg="wheat1", col.grid="wheat3", col.main="wheat4", 
  col.axis="wheat4", col.lab="wheat4", cex.lab=1.2)
  
# get a wider vertical left margin by using R par function with mar option
# particularly useful for horizontal bar charts with horizontal labels (las=1)
# move from default 4 lines to 7 lines, second arg of mar option
par(mar=c(5, 7, 4, 2) + 0.1)
bc(Pain, horiz=TRUE, las=1)
# go back to the default settings for subsequent graphs
par(mar=c(5, 4, 4, 2) + 0.1)


# ---------------------------------------------
# multiple bar charts across multiple variables
# ---------------------------------------------

# bar charts for all non-numeric variables in the data frame called mydata
#   and all numeric variables with a small number of values, < ncut
bc()

# bar chart of all relevant variables with a color randomly chosen
#  from the default color palette of eight colors
bc(random.col=TRUE)

# Use the subset function to specify a variable list
mysub <- subset(mydata, select=c(Pain))
bc(dframe=mysub)


# ----------------------------
# can enter many types of data
# ----------------------------

# generate and enter integer data
X1 <- sample(1:4, size=100, replace=TRUE)
X2 <- sample(1:4, size=100, replace=TRUE)
bc(X1)
bc(X1,X2)

# generate and enter type double data
X1 <- sample(c(1,2,3,4), size=100, replace=TRUE)
X2 <- sample(c(1,2,3,4), size=100, replace=TRUE)
bc(X1)
bc(X1, by=X2)

# generate and enter character string data
# that is, without first converting to a factor
Travel <- sample(c("Bike", "Bus", "Drive", "Walk"), size=25, replace=TRUE)
bc(Travel)


# ------------------------------
# bar chart directly from counts
# ------------------------------

# barchart of one variable with three levels
# enter counts as a vector with the combine function, c
# must supply the level names and variable name
City <- c(206, 94, 382)
names(City) <- c("LA","Chicago","NY")
bc(City, main="Employees in Each City", addtop=10)

# barchart of two variables
# two Quality levels, the rows
# three Supplier levels, the columns
# enter counts row by row, then form the table with rbind function
# must supply the level (value) names and the variable names
# chart presented as Row Variable, analyzed at each level of Column Variable
row1 <- c(19, 16, 23) 
row2 <- c(6, 6, 8) 
mytable <- rbind(row1, row2)
rownames(mytable) <- c("Pass", "Defective")
colnames(mytable) <- c("Acme, Inc", "Nuts, Inc", "Bolts, Inc")
bc(mytable, xlab="Supplier", legend.title="Quality")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }

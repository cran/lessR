\name{BarChart}
\alias{BarChart}
\alias{bc}

\title{Bar Chart of One or Two Variables}

\description{
Abbreviation: \code{bc}

Plots a bar chart of one or two variables with default colors, including background color and grid lines, from a variety of different types of data. Also displays the frequency table for one or two variables and optionally provides the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies. 

For the analysis of a single variable, the cell proportions are also provided unless there are more than 10 unique values.  If all values are unique, then only the value names are listed.  For the analysis of two variables, also provided are the proportions within each cell, within each column and within each row.   

Unlike the standard R function, \code{\link{barplot}}, the variable(s) can be entered directly into the function call without first converting to a table.  If two variables are plotted, a legend is automatically provided in the right margin.

If the provided object for which to calculate the bar chart is a data frame, then a bar chart is calculated for each non-numeric variable in the data frame and the results written to a pdf file in the current working directory. If the provided object is a variable list that is a subset of a given data frame, then the full data frame is reduced to the subset data frame and then analyzed.  The name of this file and its path are specified in the output.
}

\usage{
BarChart(x=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 

         col.fill=NULL, col.stroke="black",
         col.bg=getOption("col.bg"),
         col.grid=getOption("col.grid"),
         random.col=FALSE,
         colors=c("rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, prop=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL,
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",

         beside=FALSE, col.low=NULL, col.hi=NULL, count.levels=NULL,

         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         quiet=getOption("quiet"),
         pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots) 

bc(\ldots)
}

\arguments{
  \item{x}{For each level of this variable display the frequencies.}
  \item{by}{For each level of the first variable, x, display the frequencies at each 
       level of this second variable, y.}
  \item{data}{Optional data frame that contains the variables of interest, default
       is \code{mydata}.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest
       number of unique values of variable of a numeric data type for which the
       variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{col.fill}{Specified bar colors.}
  \item{col.stroke}{Color of the border of the bars. Specify NA to remove the border}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{colors}{Optional palettes that provide more options.}
  \item{random.col}{For two variable plots, when \code{TRUE}, randomizes the order
       of the colors within the chosen color palette, when the second variable is
       not ordered, otherwise is ignored. Each analysis generally yields different
       colors of the bars.}
  \item{horiz}{By default bars are vertical, but can set this option to \code{TRUE}.}
  \item{over.grid}{If \code{TRUE}, plot the grid lines over the histogram.}
  \item{addtop}{When \code{horiz=FALSE}, in the same scale as the vertical axis, puts
       more space between the bars and the top of the plot area, usually to
       accommodate the legend when plotting two variables.}
  \item{gap}{Gap between bars.  Provides the value of the \code{space} option from
       the standard R \code{\link{barplot}} function with a default of 0.2 unless two
       variables are plotted and beside=\code{TRUE}, in which case the default is
       c(.1,1).}
  \item{prop}{Display proportions instead raw frequencies.}
  \item{xlab}{Label for x-axis, more generally the label for the values which could
       be on the vertical axis for a two variable plot if \code{horiz=TRUE}. Defaults
       to variable name.}
  \item{ylab}{Label for y-axis, more generally the frequency axis. Defaults to
       Frequency.}
  \item{main}{Title of graph.}
  \item{cex.axis}{Scale magnification factor, which by default displays the axis 
       values to be smaller than the axis labels.}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{col.ticks}{Color of the ticks used to label the axis values.}
  \item{beside}{For a two variable plot, set to \code{TRUE} for the levels of the
       first variable to be plotted as adjacent bars instead of stacked on each other.}
  \item{col.low}{Only when the variable is an ordered factor, sets the color for the
       lowest level of the factor in the resulting ordered progression of colors.}
  \item{col.hi}{Only when the variable is an ordered factor, sets the color for the
       highest level of the factor in the resulting ordered progression of colors.}
  \item{count.levels}{If the name of a variable, this signals that the primary
       variable \code{x} has values that are counts, already tabulated, and that the
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
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{set}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for graphics as defined 
      by \code{\link{barplot}}, \code{\link{legend}}, and  \code{\link{par}} including
      \code{space} for one variable only, and \code{cex.lab}, \code{col.main}, etc.}
}


\details{
OVERVIEW\cr
Plot a bar chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the bars, background and grid lines, all of which can be customized. The basic computations of the chart are provided with the standard R functions \code{\link{barplot}},  \code{\link{chisq.test}} and, for two variables, \code{\link{legend}}. Horizontal bar charts, specified by \code{horiz=TRUE}, list the value labels horizontally and automatically extend the left margin to accommodate both the value labels and the variable label.

The form of the entered data, the first variable x and optionally a second variable, y, is flexible.  The data may be entered as factors, numeric values, characters, or a matrix. The data may be entered and the resulting frequencies computed, or the frequencies can be entered directly.  The most natural type of data to enter, when entering the variables, is to enter factors.  Plus, the bar colors for a second variable which is an ordered factor are also ordered in a corresponding progression.

DATA\cr
The data may either be a vector from the global enviornment, the user's workspace, as illustrated in the examples below, or a variable in a data frame. The default input data frame is \code{mydata}.  Specify a different data frame name with the \code{data} option.  Regardless of its name, the variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function.

If the name of vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed. If two variables are specified, both variables should be in the data frame, or one of the variables is in the data frame and the other in the global environment. 

To obtain a bar chart of each numerical variable in the \code{mydata} data frame, use \code{bar chart()}.  Or, for a data frame with a different name, insert the name between the parentheses. To analyze a subset of the variables in a data frame, specify the list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

COLORS\cr
For a one variable plot, the color of the bars is set by the current color theme according to the function \code{\link{set}}. When two variables are plotted, a set of arbitrary colors represent the various levels of the second variable. Currently transparency effects are not incorporated.\cr
There are three ways to override the default colors.\cr
1. There are two pre-defined color palettes, each with 7 colors.  Three more built-in R color palettes are also available by setting \code{colors} to one of \code{"rainbow"}, \code{"heat"} and \code{"terrain"}.  The most vivid of all the palettes is \code{"rainbow"}.\cr
2. The order of the colors within the chosen palette can be randomized with the \code{random.col="TRUE"} option. For example, when this option is activated each of the seven colors in a palette has a 1/7 chance of appearing as the first color, the only color used in the plot of a single variable, and the color used for the first bar in each group for a plot of two variables. When invoked for a  \code{color="gray"}, the order from light to dark will generally be lost, which may be desirable if the categories do not represent an ordered factor.\cr
3. The desired colors can be explicitly specified with the \code{col.bars} option, which overrides any other bar color options. When plotting one variable, include one color in this color list, the color used for all of the bars.  When plotting two variables, usually the list of colors includes the same number of elements as the number of levels for the second variable.  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in\cr \code{col.bars=c("coral3","seagreen3")} for a second variable with two levels. When two variables are plotted, if there are fewer specified colors than the levels of the second variable, the remaining colors are selected from the remaining colors in the activated palette.

The default colors in one of the two provided color palettes can be viewed, in the order in which they are displayed, by running the corresponding two lines of R code, for the default colors:\cr\cr
clr <- c("slategray3", "bisque3", "darksalmon", "darkolivegreen3", "thistle", "azure3", "moccasin")\cr
barplot(rep(1,7), names=clr, col=clr, border=NA, space=.1)

When plotting one ordered factor, or when plotting two variables, and the second variable is an ordered factor, then neither of the two standard color palettes are used.  Instead, the resulting bar colors for each level of the ordered factor are also ordered in a progression of colors. The default progression is based on the first color of either the regular or gray color palettes, but this can be changed with the \code{col.low} and \code{col.hi} options, or individually specify the color of each bar with the \code{col.bars} option. A specified palette can, for example, be from light to dark of the same hue, or from a light color of one hue to a dark color of another hue. Each color value can be specified with a color name, or with a specification with the \code{\link{rgb}} function.  See the examples below.  

Use the \code{\link{showColors}} function in this package to get, for each color:  name, sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values.

LEGEND\cr
When two variables are plotted, a legend is produced, with values for each level of the second variable.  By default, the location is placed in the right margin of the plot. This position can be changed with the \code{legend.loc} option, which, in addition to the \code{lessR} option of \code{right.margin}, accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend. 

If the default right margin is retained, variable labels are also accommodated for the legend title.  To conserve horizontal space, the variable label is listed in multiple lines if needed. The legend title is constructed by forming lines of maximum length of 12 characters, with multiple words per line if possible. Any single word in the label of more than 12 characters is abbreviated to 12 characters with the R \code{\link{abbreviate}} function. Also, any value labels are abbreviated to a maximum of 6 characters.

If the legend is not in the right margin, sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  This option only applies for the default vertical bars. Also, the legend is displayed vertically by default, but can be changed to horizontal with the \code{legend.horiz} option.

ENTER COUNTS DIRECTLY\cr
Instead of calculating the counts from the data, the counts can be entered directly.  For two variables, enter the counts as a matrix and include the \code{xlab} option to label the horizontal axis, such as with the name of the variable.  Also include the \code{legend.title} option to provide a legend.  See the examples below.

Or, include the tabulated counts as the data which is read into R. If \code{count.levels} is not \code{NULL}, then it is presumed to be a valid variable name.  As such, it indicates that the primary variable, \code{x} consists of values already tabulated, that is, counts, and is ready to be plotted directly.  The value for \code{count.levels} specifies the label for each level of \code{x}.

STATISTICS\cr
In addition to the barchart, descriptive and optional inferential statistics are also presented.  First, the frequency table for one variable or the joint frequency table for two variables is displayed. Second, the corresponding chi-square test is also displayed by default. 

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is listed as the label for the horizontal axis unless xlab is specified in the function call. If there are two variables to plot, the title of the resulting plot is based on the two variable labels, unless a specific title is listed with the \code{main} option. The variable label is also listed in the text output, next to the variable name. If the analysis is for two variables, then labels for both variables are included. 

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.
}

\value{
Invisibily returns a contingency table from the R \code{\link{table}} function, an object of class "table", an array of integer values.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{barplot}}, \code{\link{table}}, \code{\link{legend}}.
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
BarChart(Pain)
# short name
bc(Pain)
# save the frequencies for later analysis
myCount <- BarChart(Pain)
# compare to standard R bar plot, which requires mydata$ reference
barplot(table(mydata$Pain))

# Gender is unordered, so one color used for all the bars
BarChart(Gender)

# specify a unique bar color for each of the two bars
BarChart(Gender, col.fill=c("palegreen3","tan"))

# automatically provide horizontal value labels 
#   and adjust left margin as needed
BarChart(Pain, horiz=TRUE)


# ----------------------------------------
# barchart from the data for two variables
# ----------------------------------------

# at each level of Pain, show the frequencies of the Gender levels
BarChart(Pain, by=Gender)

# at each level of Pain, show the frequencies of the Gender levels
# set for all subsequent analyses for all lessR graphic functions
set(colors="gray")
BarChart(Pain, by=Gender)

# at each level of Gender, show the frequencies of the Pain levels
# Pain levels are ordered, so the corresponding colors are also ordered 
# color theme set to red with ghost effects
set(colors="red", ghost=TRUE)
BarChart(Gender, by=Pain)

# specify an ordered blue palette of colors for the ordered levels of Pain
# only works when the variable is an ordered factor
# colors can be named or customized with rgb function
BarChart(Gender, by=Pain, col.low="azure", col.hi=rgb(100,110,200,max=255))

# define custom color gradient within each group of bars
# applies to both ordered and unordered factors
BarChart(Gender, by=Pain, col.fill=c("thistle1","thistle2","thistle3","thistle4"))

# display bars beside each other instead of stacked, Female and Male
# the levels of Pain are included within each respective bar
BarChart(Gender, by=Pain, beside=TRUE, legend.horiz=TRUE)

# horizontal bar chart of two variables, put legend on the top
BarChart(Gender, by=Pain, horiz=TRUE, legend.loc="top")

# many options, including those from par: col.main, col.axis, col.lab, cex.lab
# for more info on these graphic options, enter:  help(par)
BarChart(Pain, by=Gender, col.fill=c("coral3","seagreen3"), 
  legend.loc="topleft", legend.labels=c("Girls", "Boys"), 
  xlab="Pain Level", main="Gender for Different Pain Levels", 
  col.bg="wheat1", col.grid="wheat3", col.main="wheat4", 
  col.axis="wheat4", col.lab="wheat4", cex.lab=1.2)


# ---------------------------------------------
# multiple bar charts across multiple variables
# ---------------------------------------------

# bar charts for all non-numeric variables in the data frame called mydata
#   and all numeric variables with a small number of values, < n.cat
BarChart()

# Use the subset function to specify a variable list
mydata <- Read("Employee", format="lessR", quiet=TRUE)
BarChart(c(Dept, Gender))


# ----------------------------
# can enter many types of data
# ----------------------------

# generate and enter integer data
X1 <- sample(1:4, size=100, replace=TRUE)
X2 <- sample(1:4, size=100, replace=TRUE)
BarChart(X1)
BarChart(X1,X2)

# generate and enter type double data
X1 <- sample(c(1,2,3,4), size=100, replace=TRUE)
X2 <- sample(c(1,2,3,4), size=100, replace=TRUE)
BarChart(X1)
BarChart(X1, by=X2)

# generate and enter character string data
# that is, without first converting to a factor
Travel <- sample(c("Bike", "Bus", "Car", "Motorcycle"), size=25, replace=TRUE)
BarChart(Travel, horiz=TRUE)


# ------------------------------
# bar chart directly from counts
# ------------------------------

# barchart of one variable with three levels
# enter counts as a vector with the combine function, c
# must supply the level names and variable name
City <- c(206, 94, 382)
names(City) <- c("LA","Chicago","NY")
BarChart(City, main="Employees in Each City", addtop=10)

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
BarChart(mytable, xlab="Supplier", legend.title="Quality")

# counts are in the data file to be read directly
mydata <- read.csv(text="
Dept, Count
ACCT,5
ADMN,6
FINC,4
MKTG,6
SALE,15", header=TRUE)
# use count.levels to indicate the label for each corresponding count
BarChart(Count, count.levels=Dept)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }



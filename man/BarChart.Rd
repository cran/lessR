\name{BarChart}
\alias{BarChart}
\alias{bc}

\title{Bar Chart for One or Two Variables}

\description{
Abbreviation: \code{bc}

Plots a bar chart, one categorical variable and one numeric variable, potentially with a second categorical variable with a provided legend. By default the numeric variable is the computed frequency of values in each category, with default colors for one or two variables. The numeric variable can be entered as the y-variable, in which case it could be counts or a non-integer variable. Also displays the frequency table for one or two variables, Cramer's V association, and the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies. Unlike the standard R function, \code{\link{barplot}}, the variable name(s) can be entered directly into the function call without first converting to a table. Trellis graphics activated by specifying a \code{by1} variable. 

If the provided object to analyze is a set of multiple variables, including an entire data frame, then a bar chart is calculated for each non-numeric variable in the data frame and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.
}

\usage{
BarChart(x=NULL, y=NULL, data=mydata, n.cat=getOption("n.cat"), 

        by=NULL, by1=NULL,
        n.row=NULL, n.col=NULL, aspect="fill",

        colors=c("rainbow", "terrain", "heat"),

        horiz=FALSE, addtop=0.05,
        gap=NULL, proportion=FALSE,

        beside=FALSE, low.fill=NULL, hi.fill=NULL, 

        legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
        legend.horiz=FALSE, 

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,

        cex.names=0.70,
        value.labels=NULL, label.max=20,

        quiet=getOption("quiet"),
        width=5, height=4.5, pdf.file=NULL, \ldots) 

bc(\ldots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single variable, either
        within a data frame or as a vector in the users workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all non-numerical variables in the specified data
        frame, \code{mydata} by default.}
  \item{y}{Numeric variable for which the values are displayed on the vertical
        axis. If not specified, then its value is the frequency of each
        category or joint category, automatically computed.}
  \item{data}{Optional data frame that contains the variables of interest, default
       is \code{mydata}. Can contain data from which frequencies are computed, or
       can contain values to plot on the y-axis instead of counts.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest
       number of unique values of variable of a numeric data type for which the
       variable will be analyzed as a categorical. Default is 0.}\cr

  \item{by}{A categorical variable to provide a scatterplot for
        each level of the numeric primary variables \code{x} and
        \code{y} on the \emph{same} plot, which applies to the panels of
        a Trellis plot if \code{by1} is specified.}
  \item{by1}{A categorical variable called a conditioning variable that
        activates \bold{Trellis graphics}, from the lattice package, to provide
        a separate scatterplot (panel) of numeric primary variables \code{x}
        and \code{y} for each level of the variable.}
  \item{n.row}{Optional specification for the number of rows in the layout
        of a multi-panel display with Trellis graphics. Need not specify
        \code{n.col}.}
  \item{n.col}{Optional specification for the number of columns in the
        layout of a multi-panel display with
        Trellis graphics. Need not specify \code{n.row}. If set to 1, then
        the strip that labels each group locates to the left of each plot
        instead of the top.}
  \item{aspect}{Lattice parameter for the aspect ratio of the panels,
        defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to "bank" to 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr

  \item{colors}{Optional palettes that provide pre-defined color gradients,
        which include values of \code{"heat"}, \code{"rainbow"} and
        \code{"terrain"}.}\cr
        
  \item{horiz}{By default bars are vertical, but can set to \code{TRUE}.}
  \item{addtop}{When \code{horiz=FALSE}, in the same scale as the vertical axis, puts
       more space between the bars and the top of the plot area, usually to
       accommodate the legend when plotting two variables. Now a multiplicative
       factor instead of additive as previous to Version 3.4.5.}
  \item{gap}{Gap between bars.  Provides the value of the \code{space} option from
       the standard R \code{\link{barplot}} function with a default of 0.2 unless two
       variables are plotted and beside=\code{TRUE}, in which case the default is
       c(.1,1).}
  \item{proportion}{Display proportions instead raw frequencies. For two-variable
        plots, display the column proportions, that is, a proportional stacked 
        bar graph.}\cr
        
  \item{beside}{For a two variable plot, set to \code{TRUE} for the levels of the
       first variable to be plotted as adjacent bars instead of stacked on each other.}
  \item{low.fill}{Only when the variable is an ordered factor, sets the color for the
       lowest level of the factor in the resulting ordered progression of colors.}
  \item{hi.fill}{Only when the variable is an ordered factor, sets the color for the
       highest level of the factor in the resulting ordered progression of colors.}
       
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
       horizontal.}\cr
        
  \item{xlab}{Label for \code{x}-axis. If \code{xlab} is not specified, then the label
       becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy.ticks} is \code{FALSE}, then no label is displayed.
       If no y variable is specified, then \code{xlab} is set to Index unless
       \code{xlab} has been specified.}
  \item{ylab}{Label for \code{y}-axis.  If \code{xlab} is not specified, then
       the label becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable
       labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below \code{xlab}.}\cr

  \item{cex.names}{Scale magnification factor for the categories.
        To rotate the axis values, use
        \code{rotate.x}, \code{rotate.y}, and \code{offset} from the
        \code{\link{style}} function.}
  \item{value.labels}{Labels for the \code{x}-axis on the graph to override 
        existing data values, including factor levels. If the variable is a 
        factor and \code{value.labels} is not specified (is \code{NULL}), then the
        value.labels are set to the factor levels with each space replaced by
        a new line character. If \code{x} and \code{y}-axes have the same scale,
        they also apply to the \code{y}-axis.}
  \item{label.max}{Maximum size of labels for the values of a variable.
        Not a literal maximum as preserving unique values may require a larger number
        of characters than specified.}\cr


  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{style}} function.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf.file}{Indicate to direct pdf graphics to the specified name of
        the pdf file.}

  \item{\dots}{Other parameter values for graphics as defined 
      by \code{\link{barplot}}, \code{\link{legend}}, and \code{\link{par}} including
      \code{xlim} and \code{ylim} for setting the range of the x and y-axes\cr
      \code{cex.main} for the size of the title\cr
      \code{col.main} for the color of the title\cr
      \code{cex} for the size of the axis value labels\cr
      \code{col.lab} for the color of the axis labels\cr
      \code{"dotted"}, \code{"dotdash"}\cr
      \code{sub} and \code{col.sub} for a subtitle and its color\cr
      \code{col.lab} for the color of the axis labels\cr
      \code{las=3} to reorient vertical axis labels\cr 
      \code{space} for one variable only}
}


\details{
OVERVIEW\cr
Plot a bar chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the bars, background and grid lines, all of which can be customized. The basic computations of the chart are provided with the standard R functions \code{\link{barplot}},  \code{\link{chisq.test}} and, for two variables, \code{\link{legend}}. Horizontal bar charts, specified by \code{horiz=TRUE}, list the value labels horizontally and automatically extend the left margin to accommodate both the value labels and the variable label. 

The form of the entered data, the first variable x and optionally a second variable, y, is flexible.  The data may be entered as factors, numeric values, characters, or a matrix. The data may be entered and the resulting frequencies computed, or the frequencies can be entered directly.  The most natural type of data to enter, when entering the variables, is to enter factors.  Plus, the bar colors for a second variable which is an ordered factor are also ordered in a corresponding progression.

DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or a variable in a data frame. The default input data frame is \code{mydata}.  Specify a different data frame name with the \code{data} option.  Regardless of its name, the variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function.

If the name of vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed. If two variables are specified, both variables should be in the data frame, or one of the variables is in the data frame and the other in the global environment. 

To obtain a bar chart of each numerical variable in the \code{mydata} data frame, invoke \code{Bar chart()}.  Or, for a data frame with a different name, insert the name between the parentheses. To analyze a subset of the variables in a data frame, specify the variable list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

COLORS\cr
For a one variable plot, the color of the bars is set by the current color theme according to the function \code{\link{style}}, which includes the default color theme \code{lightbronze}. When two variables are plotted, a set of arbitrary colors represent the various levels of the second variable. Transparency effects are only incorporated for a one variable bar plot in a single color.\cr
There are two ways to override the default colors.\cr
1. There are two pre-defined color palettes, each with 7 colors.  Three more built-in R color palettes are also available by setting \code{colors} to one of \code{"rainbow"}, \code{"heat"} and \code{"terrain"}.  The most vivid of all the palettes is \code{"rainbow"}.\cr
2. The desired colors can be explicitly specified with the \code{color.bars} option, which overrides any other bar color options. When plotting one variable, include one color in this color list, the color used for all of the bars.  When plotting two variables, usually the list of colors includes the same number of elements as the number of levels for the second variable.  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in\cr \code{color.bars=c("coral3","seagreen3")} for a second variable with two levels. When two variables are plotted, if there are fewer specified colors than the levels of the second variable, the remaining colors are selected from the remaining colors in the activated palette.

For the color options, such as \code{grid.color}, the value of \code{"off"} is the same as 
\code{"transparent"}.

When plotting one ordered factor, or when plotting two variables, and the second variable is an ordered factor, then neither of the two standard color palettes are used.  Instead, the resulting bar colors for each level of the ordered factor are also ordered in a progression of colors. The default progression is based on the first color of either the regular or gray color palettes, but this can be changed with the \code{low.fill} and \code{hi.fill} options, or individually specify the color of each bar with the \code{color.bars} option. A specified palette can, for example, be from light to dark of the same hue, or from a light color of one hue to a dark color of another hue. Each color value can be specified with a color name, or with a specification with the \code{\link{rgb}} function.  See the examples below.  

The \code{\link{showColors}} function in this package provides, for each color:  name, sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values.

LEGEND\cr
When two variables are plotted, a legend is produced, with values for each level of the second or \code{by} variable.  By default, the location is placed in the right margin of the plot. This position can be changed with the \code{legend.loc} option, which, in addition to the \code{lessR} option of \code{right.margin}, accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend. 

If the default right margin is retained, variable labels are also accommodated for the legend title.  To conserve horizontal space, the variable label is listed in multiple lines if needed. The legend title is constructed by forming lines of maximum length of 12 characters, with multiple words per line if possible. Any single word in the label of more than 12 characters is abbreviated to 12 characters with the R \code{\link{abbreviate}} function. Also, any value labels are abbreviated to a maximum of 6 characters.

If the legend is not in the right margin, sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  This option only applies for the default vertical bars. Also, the legend is displayed vertically by default, but can be changed to horizontal with the \code{legend.horiz} option.

LONG CATEGORY NAMES\cr
For many plots, the names of the categories tend to be long. To adjust the plot for these long names, they can be rotated using the \code{rotate.x} and \code{rotate.y} parameters, in conjunction with \code{offset}, from the \code{\link{style}} function. The \code{offset} parameter moves the category name out from the axis to compensate for the rotation. The changes from \code{\link{style}} persist until further changes. To reset to the default after obtaining an analysis, use \code{style()}.


ENTER NUMERIC VARIABLE DIRECTLY\cr
Instead of calculating the counts from the data, the counts, or the values of any other numerical variable, can be entered directly as the \code{y}-variable, in addition to the \code{x}-variable, and perhaps a \code{by}-variable. See the examples below, under "bar chart directly from data".

Or, include the already tabulated counts as the data which is read into R, either as a matrix or a data frame.

STATISTICS\cr
In addition to the bar chart, descriptive and optional inferential statistics are also presented.  First, the frequency table for one variable or the joint frequency table for two variables is displayed. Second, the corresponding Cramer's V and chi-square test are also displayed by default. 

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is listed as the label for the horizontal axis unless xlab is specified in the function call. If there are two variables to plot, the title of the resulting plot is based on the two variable labels, unless a specific title is listed with the \code{main} option. The variable label is also listed in the text output, next to the variable name. If the analysis is for two variables, then labels for both variables are included. 

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > BarChart(cut(rnorm(50), breaks=seq(-5,5)))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- cut(rnorm(50), breaks=seq(-5,5))   # create vector Y in user workspace
    > BarChart(Y)     # directly reference Y}
}

\value{
If the analysis is of a single categorical variable, a list is invisibly returned with two tables, the frequencies and the proportions, respectively named \code{freq} and \code{prop}, and also the frequency table converted to a data frame, named \code{freq.df} for input into other functions, including \code{ggplot2}.  If two categorical variables are analyzed, then nothing is returned.
}


\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 4, NY: Routledge.
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


# ---------------------------------------------
# bar chart from the data for a single variable
# ---------------------------------------------

# for each level of Pain, display the frequencies
# Pain is an ordered factor, so the bar colors are ordered
BarChart(Pain)
# short name
bc(Pain)
# save and then display the frequencies for later analysis
myCount <- BarChart(Pain)
myCount
# rotate and offset the axis labels
style(rotate.x=45, offset=1)
BarChart(Pain)
style()
# compare to standard R bar plot, which requires mydata$ reference
barplot(table(mydata$Pain))

# Gender is unordered, so one color used for all the bars
BarChart(Gender)

# column proportions instead of frequencies
BarChart(Gender, proportion=TRUE)

# specify a unique bar color for each of the two bars, then reset theme
style(bar.fill=c("palegreen3","tan"))
BarChart(Gender)
style()

# automatically provide horizontal value labels 
#   and adjust left margin as needed
BarChart(Pain, horiz=TRUE)

# manage size of horizontal value labels
myd <- Read("Cars93", format="lessR")
bc(Make, horiz=TRUE, label.max=4, data=myd)


# -----------------------------------------
# bar chart from the data for two variables
# -----------------------------------------

# at each level of Pain, show the frequencies of the Gender levels
BarChart(Pain, by=Gender)

# at each level of Pain, show the row proportions of the Gender levels
#   i.e., proportional stacked bar graph
BarChart(Pain, by=Gender, proportion=TRUE)

# at each level of Gender, show the frequencies of the Pain levels
# Pain levels are ordered, so the corresponding colors are also ordered 
# color theme set to gray
style("gray")
BarChart(Gender, by=Pain)
style("sienna")

# specify an ordered blue palette of colors for the ordered levels of Pain
# only works when the variable is an ordered factor
# colors can be named or customized with rgb function
BarChart(Gender, by=Pain, low.fill="azure", hi.fill=rgb(100,110,200,max=255))

# display bars beside each other instead of stacked, Female and Male
# the levels of Pain are included within each respective bar
BarChart(Gender, by=Pain, beside=TRUE, legend.horiz=TRUE)

# horizontal bar chart of two variables, put legend on the top
BarChart(Gender, by=Pain, horiz=TRUE, legend.loc="top")

# many options, including those from par: col.main, col.lab, lab.cex
# for more info on these graphic options, enter:  help(par)
style(fill=c("coral3","seagreen3"), lab.color="wheat4", lab.cex=1.2,
  panel.fill="wheat1", trans=.7)
BarChart(Pain, by=Gender, 
   legend.loc="topleft", legend.labels=c("Girls", "Boys"), 
   xlab="Pain Level", main="Gender for Different Pain Levels", 
   value.labels="wheat4", 
   col.main="wheat4")
style()


# ---------------------------------------------
# multiple bar charts across multiple variables
# ---------------------------------------------

# bar charts for all non-numeric variables in the data frame called mydata
#   and all numeric variables with a small number of values, < n.cat
# BarChart()



# ----------------------------
# can enter many types of data
# ----------------------------

# generate and enter integer data
X1 <- sample(1:4, size=100, replace=TRUE)
X2 <- sample(1:4, size=100, replace=TRUE)
BarChart(X1)
BarChart(X1, by=X2)

# generate and enter type double data
X1 <- sample(c(1,2,3,4), size=100, replace=TRUE)
X2 <- sample(c(1,2,3,4), size=100, replace=TRUE)
BarChart(X1)
BarChart(X1, by=X2)

# generate and enter character string data
# that is, without first converting to a factor
Travel <- sample(c("Bike", "Bus", "Car", "Motorcycle"), size=25, replace=TRUE)
BarChart(Travel, horiz=TRUE)


# ----------------------------
# bar chart directly from data
# ----------------------------


# include a y-variable, here Salary, in the data table to read directly
mydata <- read.csv(text="
Dept, Salary
ACCT,51792.78
ADMN,71277.12
FINC,59010.68
MKTG,60257.13
SALE,68830.06", header=TRUE)
BarChart(Dept, Salary)

# specify two variables for a two variable bar chart
# also specify a y-variable to provide the counts directly
mydata <- read.csv(text="
Dept,Gender,Count
ACCT,F,3
ACCT,M,2
ADMIN,F,4
ADMIN,M,2
FINC,F,1
FINC,M,3
MKTG,F,5
MKTG,M,1
SALE,F,5
SALE,M,10", header=TRUE)
BarChart(Dept, Count, by=Gender)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }




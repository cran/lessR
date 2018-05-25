\name{BarChart}
\alias{BarChart}
\alias{bc}

\title{Bar Chart for One or Two Variables}

\description{
Abbreviation: \code{bc}

Plots a bar chart, one categorical variable, \code{x} and one numeric variable \code{y}, as well as an optional second categorical variable \code{by} with a provided legend. By default the numeric variable is the computed frequency of values in each category, with default colors for one or two variables, which can be replaced with custom color scales. The numeric variable can be entered as the \code{y}-variable, in which case it could be a non-integer variable. Also displays the frequency table for one or two variables, Cramer's V association, and the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies. Activate Trellis graphics  by specifying a \code{by1} variable. If the provided object to analyze is a set of multiple variables, including an entire data frame, then a bar chart is calculated for each non-numeric variable in the data frame. 
}

\usage{
BarChart(x=NULL, y=NULL, by=NULL, data=mydata,
        n.cat=getOption("n.cat"), 

        by1=NULL, n.row=NULL, n.col=NULL, aspect="fill",

        horiz=FALSE, addtop=0.05, gap=NULL,
        proportion=FALSE, beside=FALSE,
        scale.y=NULL,

        fill=getOption("bar.fill.discrete"),
        color=getOption("bar.color"),
        trans=getOption("trans.bar.fill"),

        legend.title=NULL, legend.loc="right.margin",
        legend.labels=NULL, legend.horiz=FALSE, 

        value.labels=NULL,
        rotate.x=getOption("rotate.x"),
        offset=getOption("offset"),
        break.x=TRUE, sort.x=c("off", "down", "up"),

        label.max=100, out.size=80,

        values=getOption("values"),
        values.color=getOption("values.color"), 
	values.cex=getOption("values.cex"),
        values.digits=getOption("values.digits"),
        values.pos=getOption("values.pos"),

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        xlab.adj=0, ylab.adj=0,
        bm.adj=0, lm.adj=0, tm.adj=0, rm.adj=0,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        eval.df=NULL, quiet=getOption("quiet"),
        width=6.5, height=6, pdf=NULL, \ldots) 

bc(\ldots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single variable, either
        within a data frame or as a vector in the users workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all non-numerical variables in the specified data
        frame, \code{mydata} by default.\cr
        To manage large category values, unless \code{break.x} is \code{FALSE},
        any space in each category value is
        converted to new line for the corresponding axis label in the plot.
        To keep two (small) words on the same line, replace the space that
        separates them with an underscore, which is converted to a blank
        for the corresponding axis label.}
  \item{y}{Numeric variable for which the values are displayed on the vertical
        axis. If not specified, then its value is the frequency of each
        category or joint category, automatically tabulated.}
  \item{by}{A categorical variable to provide a bar chart for
        each level of the numeric primary variables \code{x} and
        \code{y} on the \emph{same} plot, which applies to the panels of
        a Trellis plot if \code{by1} is specified.}
  \item{data}{Optional data frame that contains the variables of interest, default
       is \code{mydata}. Can contain data from which frequencies are computed, or
       can contain values to plot on the y-axis instead of counts.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest
       number of unique values of variable of a numeric data type for which the
       variable will be analyzed as a categorical. Default is 0.}\cr

  \item{by1}{A categorical variable called a conditioning variable that
        activates \bold{Trellis graphics}, from the lattice package, to provide
        a separate bar chart (panel) of numeric primary variables \code{x}
        and \code{y} for each level of the variable.}
  \item{n.row}{Optional specification for the number of rows in the layout
        of a multi-panel display with Trellis graphics. Need not specify
        \code{n.col}.}
  \item{n.col}{Optional specification for the number of columns in the
        layout of a multi-panel display with
        Trellis graphics. Need not specify \code{n.row}. If set to 1, then
        the strip that labels each group locates to the left of each plot
        instead of the top.}
  \item{aspect}{Lattice parameter for the aspect ratio of the panels in
        a Trellis plot (multi-panel display), defined as height divided by
        width. The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to "bank" to 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr
        
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
        bar graph.}
  \item{beside}{For a two variable plot, set to \code{TRUE} for the levels of the
       first variable to be plotted as adjacent bars instead of stacked on each
       other.}
  \item{scale.y}{If specified, a vector of three values that define the
        numerical values of the y-axis, the numerical axis, within the bounds of
        plot region: starting, ending and number of intervals.}\cr

  \item{fill}{Fill color of the bars. 
        Can explicitly choose \code{"grays"}
        or \code{"hcl"} colors, or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}.
        Default is
        \code{bar.color} from the lessR \code{\link{style}} function.}
  \item{color}{Border color of the bars, can be a vector
        to customize the color for each bar. Default is
        \code{bar.color} from the lessR \code{\link{style}} function.}
  \item{trans}{Transparency factor of the area of each slice.
        Default is
        \code{trans.bar.fill} from the lessR \code{\link{style}} function.}\cr
       
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

  \item{value.labels}{For factors, default is the factor labels, and for 
        character variables, default is the character values.
        Or, provide labels for the \code{x}-axis on the graph to override 
        these values. If the variable is a 
        factor and \code{value.labels} is not specified (is \code{NULL}), then the
        value.labels are set to the factor levels with each space replaced by
        a new line character. If \code{x} and \code{y}-axes have the same scale,
        they also apply to the \code{y}-axis. Control the plotted size 
        with \code{axis.cex} and \code{axis.x.cex} from the lessR
        \code{\link{style} function.}}
  \item{rotate.x}{Degrees that the axis values for the category values
        axis are rotated, usually to accommodate longer values,
        typically used in conjunction with \code{offset}. When equal 90
        the value labels are perpendicular to the x-axis and a different
        algorithm places the labels so that \code{offset} is not needed.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}
  \item{break.x}{Replace spaces in the category values with a new line
       and replace underscores with a blank.}
  \item{sort.x}{Sort the categories by their frequency for one variable and by
        the column sums if a \code{by} variable. Not applicable to Trellis plots.
        By default \code{"off"}, or sort descending \code{"down"} or
        ascending \code{"up"}.}\cr

  \item{label.max}{To improve readability, the maximum size of the value
        labels before the labels are abbreviated for 
        text output only. Not a literal maximum as preserving unique
        values may require a larger number of characters than specified.}
  \item{out.size}{To improve the readability of the frequency distribution 
       of a single variable displayed at the console,
       the maximum number of characters on a
       line of output at the console for one variable before the frequency
       distribution is written vertically.}\cr

  \item{values}{If not the default value of \code{"off"}, adds the
        numerical results to the plot according to \code{"\%"}, 
        \code{"prop"} or \code{"input"}, that is, percentages,
         proportions, or the value from which the slices are plotted,
         such as tabulated counts if \code{y} is not specified,
         or the value of \code{y} if the plotted values are provided. If
         any other \code{values} parameter is specified, default is set
         to \code{"\%"}. When a \code{by} variable is included applies
         to stacked bar charts only.}
  \item{values.color}{Color of the plotted text. Could be a vector to specify
        a unique color for each value. If fewer colors are specified than
        the number of categories, the colors are recycled.}
  \item{values.cex}{Character expansion factor, the size, of the plotted text, 
        for which the default value is 0.95.}
  \item{values.digits}{Number of decimal digits for which to display the values.
        Default is 0, round to the nearest integer, for \code{"\%"}
        and 2 for \code{"prop"}.}
  \item{values.pos}{Position of the plotted text. Default is inside the bar,
        or, if \code{"out"}, as part of the label for each value
        outside of the bar. Only applicable if no \code{by} variable. }\cr        

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
  \item{main}{Label for the title of the graph.
       Can set size with \code{main.cex} and color with \code{main.color} from the
       lessR \code{\link{style}} function.}
  \item{sub}{Sub-title of graph, below \code{xlab}.}\cr

  \item{xlab.adj}{Adjustment to the provided computation for the
       x-axis category labels, indicated in approximate inches.
       + values move the label up. This and
       the following not applicable to Trellis graphics.}
  \item{ylab.adj}{Adjustment to the provided computation for the
       y-axis category labels. + values move the label up,
       indicated in approximate inches.}
  \item{bm.adj}{Adjustment to the provided computation for the
       bottom margin, which also adjusts the x-label position in inches.
       + values move the margin up.}
  \item{lm.adj}{Adjustment to the provided computation for the
       left margin, which also adjusts the y-label position in inches.
       + values move the margin left.}
  \item{tm.adj}{Adjustment to the provided computation for the
       top margin in inches.}
  \item{rm.adj}{Adjustment to the provided computation for the
       right margin in inches.}\cr

  \item{add}{\bold{Draw one or more objects}, text or a geometric figures,
       on the plot.
       Possible values are any text to be written, the first argument, which is
       \code{"text"}, or, to indicate a figure,
       \code{"rect"} (rectangle), \code{"line"}, \code{"arrow"},
       \code{"v.line"} (vertical line), and \code{"h.line"} (horizontal line).
       The value \code{"means"} is short-hand for vertical and horizontal lines
       at the respective means. Does not apply to Trellis graphics.
       Customize with parameters such as \code{add.fill} and \code{add.color}
       from the \code{\link{style}} function.}
  \item{x1}{First x coordinate to be considered for each object.
           All coordinates vary from -1 to 1.}
  \item{y1}{First y coordinate to be considered for each object.}
  \item{x2}{Second x coordinate to be considered for each object.
        Only used for \code{"rect"}, \code{"line"} and \code{arrow}.}
  \item{y2}{Second y coordinate to be considered for each object.
        Only used for \code{"rect"}, \code{"line"} and \code{arrow}.}\cr

  \item{eval.df}{Determines if to check for existing data frame and
        specified variables. By default is \code{TRUE} 
        unless the \code{shiny} package is loaded then set to \code{FALSE} so
        that Shiny will run. Needs to be set to \code{FALSE} if using
        the pipe \code{\%\>\%} notation.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{style}} function.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf}{If \code{TRUE}, indicate to direct pdf graphics to 
       for each specified variable to a pdf file named BarChart_name.pdf
       where \code{name} is the variable name.}

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
For a one variable plot, set the default color of the bars by the current color theme according to \code{bar.fill} argument of the function \code{\link{style}}, which includes the default color theme \code{lightbronze}, or set the bar color with the \code{fill} parameter, which can use color sequences generated by \code{\link{getColors}} function. When two variables are plotted, by default a set of discrete colors represent the levels of the second variable. Transparency effects are only incorporated for a one variable bar plot in a single color.

To override the default colors, set the \code{fill} parameter, either in the
call to BarChart or with the \code{\link{style}} function to implement the 
change indefinitely, along with a large number of related style choices.

Set \code{fill} to a single color or a color range, of which there are many possibilities. For \code{"colors"} colors of the same chroma and luminance set \code{fill} to multiple colors all with the same saturation and brightness. Also available are the pre-specified R color schemes \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also provide pre-defined color ranges \code{"blues"}, \code{"reds"} and \code{"greens"}, or generate Can also generate custom colors, such as from the lessR function \code{\link{getColors}}. Or, specify the colors manually. For example, for a two-level \code{by} variable, could set \code{fill} to \code{c("coral3","seagreen3")}.

The \code{\link{showColors}} function in this package provides, for each color name, sample color swatch, and corresponding rgb specification.  For a very small number of levels, such as two, it is may be desirable to specify the low and high values to not be closer to each other than the default values.

LEGEND\cr
When two variables are plotted, a legend is produced, with values for each level of the second or \code{by} variable.  By default, the location is placed in the right margin of the plot. This position can be changed with the \code{legend.loc} option, which, in addition to the \code{lessR} option of \code{right.margin}, accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend. 

If the default right margin is retained, variable labels are also accommodated for the legend title.  To conserve horizontal space, the variable label is listed in multiple lines if needed. The legend title is constructed by forming lines of maximum length of 12 characters, with multiple words per line if possible. Any single word in the label of more than 12 characters is abbreviated to 12 characters with the R \code{\link{abbreviate}} function. Also, any value labels are abbreviated to a maximum of 6 characters.

If the legend is not in the right margin, sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  This option only applies for the default vertical bars. Also, the legend is displayed vertically by default, but can be changed to horizontal with the \code{legend.horiz} option.

LONG CATEGORY NAMES\cr
For many plots, the names of the categories tend to be long. To adjust the plot for these long names, they can be rotated using the \code{rotate.x} and \code{rotate.y} parameters, in conjunction with \code{offset}, from the \code{\link{style}} function. The \code{offset} parameter moves the category name out from the axis to compensate for the rotation. The changes from \code{\link{style}} persist until further changes. To reset to the default after obtaining an analysis, use \code{style()}.

Also, the following codes are used to adjust line spacing:\cr
1. Any space in a category name is converted to a new line.\cr
2. If the space should not be converted to a new line, the replace with an underscore, which will display as a space and no line break.\cr

For the text output at the console, can specify the maximum number of characters in a label with \code{labels.max}. Longer value names are abbreviated to the specified length. This facilitates reading cross-tab tables. Also, a provided table pairs the abbreviated names with the actual names. For one variable frequency distributions, \code{out.size} provides the maximum number of characters for the text output before the horizontal display of the frequency distribution is shifted to a vertical presentation. 


ENTER NUMERIC VARIABLE DIRECTLY\cr
Instead of calculating the counts from the data, the counts, or the values of any other numerical variable, can be entered directly as the \code{y}-variable, in addition to the \code{x}-variable, and perhaps a \code{by}-variable. See the examples below, under "bar chart directly from data".

Or, include the already tabulated counts as the data which is read into R, either as a matrix or a data frame.

STATISTICS\cr
In addition to the bar chart, descriptive and optional inferential statistics are also presented.  First, the frequency table for one variable or the joint frequency table for two variables is displayed. Second, the corresponding Cramer's V and chi-square test are also displayed by default. 

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is listed as the label for the horizontal axis unless xlab is specified in the function call. If there are two variables to plot, the title of the resulting plot is based on the two variable labels, unless a specific title is listed with the \code{main} option. The variable label is also listed in the text output, next to the variable name. If the analysis is for two variables, then labels for both variables are included. 

PDF OUTPUT\cr
To obtain pdf output, set the \code{pdf} option to \code{TRUE}, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > BarChart(cut(rnorm(50), breaks=seq(-5,5)))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- cut(rnorm(50), breaks=seq(-5,5))   # create vector Y in user workspace
    > BarChart(Y)     # directly reference Y}
}

\value{
If the analysis is of a single categorical variable, a list is invisibly returned with two tables, the frequencies and the proportions, respectively named \code{freq} and \code{prop}, and also the frequency table converted to a data frame, named \code{freq.df} for input into other functions, including \code{ggplot2}.  If two categorical variables are analyzed, then a data frame of the cross-tabulation table is returned.
}


\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 4, NY: Routledge.
}

\author{
David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})
}

\seealso{
\code{\link{getColors}}, \code{\link{barplot}}, \code{\link{table}}, \code{\link{legend}}.
}


\examples{

# get the data
mydata <- rd("Employee", format="lessR")

# --------------------------------------------------------
# bar chart from tabulating the data for a single variable
# --------------------------------------------------------

# for each level of Dept, display the frequencies
BarChart(Dept)
# short name
# bc(Dept)
# save the frequencies for later analysis
myCount <- BarChart(Dept)
# display the frequencies
myCount
# rotate and offset the axis labels, sort categories by frequencies
BarChart(Dept, rotate.x=45, offset=1, sort.x="down")

# set bars to a single color of blue
BarChart(Dept, fill="blue", trans=0.3)
# same as an explicit call to getColors
BarChart(Dept, fill="colors", trans=0.3)
# with an explicit call to getColors to darken the colors,
#   do a lower value of luminance, set to l=50
BarChart(Dept, fill=getColors(l=50), color="lightgray", trans=0.4)
# or just set the sub.theme to "colors"
style(sub.theme="colors")
BarChart(Dept)
# same as the following, where the ordered color matches the theme
style(bar.fill.discrete="colors", bar.fill.ordered="blues",
      bar.color="off", values="\%")
# reset the style
style()

# slightly transparent hcl colors with no bar borders
bc(Dept, fill=getColors(n=5), color="off", trans=.15)

# unique bar color for each of the two bars
BarChart(Gender, fill=c("palegreen3","tan"))

# column proportions instead of frequencies
BarChart(Gender, proportion=TRUE)

myd <- Read("Cars93", format="lessR")
# perpendicular labels
bc(Make, rotate.x=90, data=myd)
# manage size of horizontal value labels
bc(Make, horiz=TRUE, label.max=4, data=myd)


# ----------------------------------------------------
# bar chart from tabulating the data for two variables
# ----------------------------------------------------

# at each level of Dept, show the frequencies of the Gender levels
BarChart(Dept, by=Gender)

# at each level of Dept, show the row proportions of the Gender levels
#   i.e., proportional stacked bar graph
# display the relevant percentages directly on the bars
BarChart(Dept, by=Gender, proportion=TRUE, values="\%")

# at each level of Gender, show the frequencies of the Dept levels
# display percentages directly on the bars
BarChart(Gender, by=Satisfaction, fill="reds", values="\%")

# specify two fill colors for Gender
BarChart(Dept, by=Gender, fill=getColors(c("deepskyblue", "black")))

# specify an ordered customized blue palette of colors for Dept
# colors can be named or customized with rgb function
# here "azure" is a single color instead of a range (ends in s)
BarChart(Gender, by=Dept, 
         fill=getColors("azure", end.clr=rgb(100,110,200,max=255)))

# display bars beside each other instead of stacked, Female and Male
# the levels of Dept are included within each respective bar
BarChart(Gender, by=Dept, beside=TRUE, legend.horiz=TRUE)

# horizontal bar chart of two variables, put legend on the top
BarChart(Gender, by=Dept, horiz=TRUE, legend.loc="top")

# many options, including those from par: col.main, col.lab, lab.cex
# for more info on these graphic options, enter:  help(par)
# here fill is set in the style function instead of BarChart
#   along with the others
style(fill=c("coral3","seagreen3"), lab.color="wheat4", lab.cex=1.2,
      panel.fill="wheat1", main.color="wheat4")
BarChart(Dept, by=Gender, 
         legend.loc="topleft", legend.labels=c("Girls", "Boys"), 
         xlab="Dept Level", main="Gender for Different Dept Levels", 
         value.labels=c("None", "Some", "Much", "Ouch!"))
style()


# -----------------------------------------------------------------
# multiple bar charts tabulated from data across multiple variables
# -----------------------------------------------------------------

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


# -----------
# annotations
# -----------

mydata <- rd("Employee", format="lessR")

# Place a message in the center of the plot
# Use \n to indicate a new line
BarChart(Dept, add="Employees by\nDepartment", x1=3, y1=10)

# Use style to change some parameter values
style(add.trans=.8, add.fill="gold", add.color="gold4", add.lwd=0.5)
# Add a rectangle around the message centered at <3,10>
BarChart(Dept, add=c("rect", "Employees by\nDepartment"),
                     x1=c(2,3), y1=c(11, 10), x2=4, y2=9)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }




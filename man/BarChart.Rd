\name{BarChart}
\alias{BarChart}
\alias{bc}

\title{Bar Chart for One or Two Variables}

\description{
Abbreviation: \code{bc}

Plots a bar chart, one categorical variable, \code{x} and one numeric variable \code{y}, as well as an optional second categorical variable \code{by} with a provided legend. By default the numeric variable is the computed frequency of values in each category, with default colors for one or two variables, which can be replaced with custom color scales. The numeric variable can be entered as the \code{y}-variable, in which case it could be a non-integer variable. Also displays the frequency table for one or two variables, Cramer's V association, and the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies. Activate Trellis graphics  by specifying a \code{by1} variable. If the provided object to analyze is a set of multiple variables, including an entire data frame, then a bar chart is calculated for each non-numeric variable in the data frame.
}

\usage{
BarChart(x=NULL, y=NULL, by=NULL, data=d, rows=NULL,
        theme=getOption("theme"), n.cat=getOption("n.cat"),
        one.plot=NULL,

        by1=NULL, n.row=NULL, n.col=NULL, aspect="fill",

        horiz=FALSE, beside=FALSE, gap=NULL,
        proportion=FALSE, scale.y=NULL,

        fill=NULL,
        color=getOption("bar.color.discrete"),
        trans=getOption("trans.bar.fill"),
        fill.split=NULL,

        legend.title=NULL, legend.position="right.margin",
        legend.labels=NULL, legend.horiz=FALSE,
        legend.size=NULL,

        value.labels=NULL,
        rotate.x=getOption("rotate.x"),
        offset=getOption("offset"),
        break.x=TRUE, sort=c("0", "-", "+"),

        label.max=100, out.size=80,

        values=NULL,
        values.color=getOption("values.color"),
        values.size=getOption("values.size"),
        values.digits=getOption("values.digits"),
        values.position=getOption("values.position"),
        values.cut=NULL,

        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        lab.adj=c(0,0), margin.adj=c(0,0,0,0), addtop=0.05,

        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        eval.df=NULL, quiet=getOption("quiet"),
        width=6.5, height=6, pdf=FALSE, \ldots)

bc(\ldots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single variable, either
        within a data frame or as a vector in the users workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all non-numerical variables in the specified data
        frame, \code{d} by default.\cr
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
       is \code{d}. Can contain data from which frequencies are computed, or
       can contain values to plot on the y-axis instead of counts.}
  \item{rows}{A logical expression that specifies a subset of rows of the data frame
       to analyze.}
  \item{theme}{Color theme for this analysis.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest
       number of unique values of variable of a numeric data type for which the
       variable will be analyzed as a categorical. Default is 0.}
  \item{one.plot}{For bar charts of multiple x-variables, indicates
       if a bar plot is produced for each x-variable, or all are combined
       into a single plot, such as for items that all share common responses
       such as survey data with a common Likert scale across variables.
       Default is if variables share a common response scale
       set to \code{TRUE}, otherwise \code{FALSE}.}\cr

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

  \item{horiz}{By default the value is \code{FALSE} so bars are vertical,
       unless \code{one.plot} is \code{TRUE}.}
  \item{beside}{For a two variable plot, set to \code{TRUE} for the levels of the
       first variable to be plotted as adjacent bars instead of stacked on each
       other.}
  \item{gap}{Gap between bars.  Provides the value of the \code{space} option from
       the standard R \code{\link{barplot}} function with a default of 0.2 unless two
       variables are plotted and beside=\code{TRUE}, in which case the default is
       c(.1,1).}
  \item{proportion}{Display proportions instead raw frequencies. For two-variable
        plots, display the column proportions, that is, a proportional stacked
        bar graph.}
  \item{scale.y}{If specified, a vector of three values that define the
        numerical values of the y-axis, the numerical axis, within the bounds of
        plot region: starting, ending and number of intervals.}\cr

  \item{fill}{Fill color of the bars, override the default with a vector
        of colors: names, rgb, hex or hcl and many other possibilities from
        palettes such as viridis.
        Can generate these colors with pre-defined qualitative, sequential and
        divergent palettes generated by lessR \code{\link{getColors}},
        such as \code{"blues"},  or
        set to the name of \code{y} to map the values of bar fill, specified
        as \code{(count)} if tabulated from the data.
        See the details and examples sections. Default is
        \code{bar.color.discrete} from the lessR \code{\link{style}} function 
        except sequential ordered factors.}
  \item{color}{Border color of the bars, can be a vector
        to customize the color for each bar. Default is
        \code{bar.color.discrete} from the lessR \code{\link{style}} function.}
  \item{trans}{Transparency factor of the area of each slice from 0, no
        transparency to 1, full transparency. Default is
        \code{trans.bar.fill} from the lessR \code{\link{style}} function.}
  \item{fill.split}{The value of the numeric variable \code{y} for which
        bars that correspond to values of \code{y} <= \code{fill.split} are
        displayed in the first \code{fill} color and other values displayed
        in the second \code{fill} color, with the first two values of
        the qualitative hues by default. }\cr

  \item{legend.title}{Title of the legend, which is usually set by default except
        when raw counts are entered as a matrix.  Then a title must be specified
        to generate a legend.}
  \item{legend.position}{When plotting two variables, location of the legend, with the
        default in the right margin. Additional options from standard R are
        "topleft", "top", "topright" and others as shown in the help for the
        \code{\link{legend}} function.}
  \item{legend.labels}{When plotting two variables, labels for the legend, which by
       default are the levels for the second or \code{by} variable.}
  \item{legend.horiz}{By default the legend is vertical, but can be changed to
       horizontal.}
  \item{legend.size}{Size of legend text.}\cr

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
  \item{sort}{Sort the categories by their frequency for one variable and by
        the column sums if a \code{by} variable. Not applicable to Trellis plots.
        By default \code{"0"} for no sort, or sort descending \code{"-"} or
        ascending \code{"+"}, unless \code{one.plot} is \code{TRUE}, then is
        set to \code{"+"}.}\cr

  \item{label.max}{To improve readability, the maximum size of the value
        labels before the labels are abbreviated for
        text output only. Not a literal maximum as preserving unique
        values may require a larger number of characters than specified.}
  \item{out.size}{To improve the readability of the frequency distribution
       of a single variable displayed at the console,
       the maximum number of characters on a
       line of output at the console for one variable before the frequency
       distribution is written vertically.}\cr

  \item{values}{If not \code{"off"}, adds the
        numerical results to the plot according to the default \code{"\%"}
        for tabulated counts and \code{"input"} for the input values for
        a \code{y}-variable explicitly provided, unless there are more than
        15 levels or \code{y} is present and non-integer in which case
        the default is \code{"off"}. For tabulated
        counts, \code{"prop"} is also available for proportions as well as
        \code{"input"}.}
  \item{values.color}{Color of the plotted text. Could be a vector to specify
        a unique color for each value. If fewer colors are specified than
        the number of categories, the colors are recycled.}
  \item{values.size}{Character expansion factor, the size, of the plotted text,
        for which the default value is 0.95, or 0.9 of value if \code{beside}
        is \code{TRUE} and \code{values.position} is \code{"in"} because bars are
        narrower.}
  \item{values.digits}{Number of decimal digits for which to display the values.
        Default is 0, round to the nearest integer for \code{"\%"},
        2 for \code{"prop"}, and if \code{"input"} and \code{y} is entered
        directly, display the literal value unless > 9999, in which case
        set to 0.}
  \item{values.position}{Position of the plotted text. Default is inside the bar,
        or, if \code{"out"}, as part of the label for each value
        outside of the bar.}
  \item{values.cut}{Threshold for displaying the value. If \code{values.position}
       equals \code{"out"}, then default is 0.028 unless there is a
       \code{by} variable or multiple x-variables on the same plot, then
       default is 0.040}\cr

  \item{xlab}{Label for \code{x}-axis. If \code{xlab} is not specified, then
       the label becomes
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
  \item{sub}{Sub-title of graph, below \code{xlab}.}
  \item{lab.adj}{Two-element vector -- x-axis label, y-axis label -- adjusts
       the position of the axis labels in approximate inches. + values move
       the labels away from plot edge. Not applicable to Trellis graphics.}
  \item{margin.adj}{Four-element vector -- top, right, bottom and left --
       adjusts the margins of the plotted figure in approximate inches.
       + values move the corresponding margin away from plot edge. 
       Not applicable to Trellis graphics.}
  \item{addtop}{In the same scale as the corresponding axis, puts
       more space between the bars and the top of the plot area, usually to
       accommodate the legend when plotting two variables or a display
       of the values on top of the bars.}\cr

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
  \item{pdf}{If \code{TRUE}, indicate to direct pdf graphics
       for each specified variable to a pdf file named BarChart_name.pdf
       where \code{name} is the variable name.}

  \item{\dots}{Other parameter values for graphics as defined
      by \code{\link{barplot}}, \code{\link{legend}}, and \code{\link{par}} including
      \code{xlim} and \code{ylim} for setting the range of the x and y-axes\cr
      \code{cex.main} for the size of the title\cr
      \code{col.main} for the color of the title\cr
      \code{"dotted"}, \code{"dotdash"}\cr
      \code{sub} and \code{col.sub} for a subtitle and its color\cr
      \code{las=3} to reorient vertical axis labels\cr
      \code{space} for one variable only}
}


\details{
OVERVIEW\cr
Plot a bar chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the bars, background and grid lines, all of which can be customized. The basic computations of the chart are provided with the standard R functions \code{\link{barplot}},  \code{\link{chisq.test}} and, for two variables, \code{\link{legend}}. Horizontal bar charts, specified by \code{horiz=TRUE}, list the value labels horizontally and automatically extend the left margin to accommodate both the value labels and the variable label.


DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or a variable in a data frame. The default input data frame is \code{d}.  Specify a different data frame name with the \code{data} option.  Regardless of its name, the variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{d$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function.

If the name of vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed. If two variables are specified, both variables should be in the data frame, or one of the variables is in the data frame and the other in the global environment.

To obtain a bar chart of each numerical variable in the \code{d} data frame, invoke \code{BarChart()}.  Or, for a data frame with a different name, insert the name between the parentheses. To analyze a subset of the variables in a data frame, specify the variable list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

The \code{rows} parameter subsets rows (cases) of the input data frame according to a logical expression. Use the standard R operators for logical statements as described in \code{\link{Logic}} such as \code{&} for and, \code{|} for or and \code{!} for not, and use the standard R relational operators as described in \code{\link{Comparison}} such as \code{==} for logical equality \code{!=} for not equals, and \code{>} for greater than. See the Examples.

The form of the entered data, the first variable x and optionally a second variable, y, is flexible.  The data may be entered as factors, numeric values, characters, or a matrix. The data may be entered and the resulting frequencies computed, or the frequencies can be entered directly.  The most natural type of data to enter, when entering the variables, is to enter factors.


COLORS\cr
For a one variable plot, set the default color of the bars by the current color theme according to \code{bar.fill.discrete} argument of the function \code{\link{style}}, which includes the default color theme \code{"hues"} that defines a qualitative HCL color scale, or set the bar color with the \code{fill} parameter, which references a specified vector of color specifications, such as generated by the lessR \code{\link{getColors}} function.

Set \code{fill} to a single color or a color palette, of which there are many possibilities. Define a qualitative color palette with \code{"hues"} that provides HCL colors of the same chroma (saturation) and luminance (brightness). Also available are the pre-specified R color palettes \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Pre-defined sequential and divergent color ranges are available as implicit calls to \code{\link{getColors}}. The full list of pre-defined color ranges (defined in 30 degree increments around the HCL color wheel): \code{"reds"}, \code{"rusts"}, \code{"browns"},
\code{"olives"}, \code{"greens"}, \code{"emeralds"}, \code{"turquoises"}, \code{"aquas"}, \code{"blues"}, \code{"purples"},\code{"violets"}, \code{"magentas"}, and \code{"grays"}.

Define the default qualitative color scale with a \code{fill} set to \code{"hues"}. Define a \emph{sequential color scale} with single value of \code{fill} for a pre-defined palette such as \code{"blues"}. Define a \emph{divergent color scale} with value of \code{fill} that consists of a vector of two such pre-defined ranges, such as \code{c("purples", "rusts")}. Divergent color palettes are applicable in particular for plotting multiple bar charts on the same plot such as for a set of Likert response items, all on a common response scale. Or, \emph{manually specify colors}. For example, for a two-level \code{by} variable, could set \code{fill} to \code{c("coral3","seagreen3")}, where the specified colors are \emph{not} pre-defined color ranges.

For the pre-defined color scales can obtain more control over the obtained color palettes with an explicit call to \code{\link{getColors}} for the argument to \code{fill}. Here the value of chroma (\code{c}) and luminance (\code{l}) can be explicitly manipulated in conjunction with the specification of a pre-defined color range. Or, create a custom color range for any value of hue (\code{h}). See  \code{\link{getColors}} for more information.

The values of another variable can be mapped into the fill color of the bars. To do so, set \code{fill} to the value of the variable, which would usually be the name of the \code{y} variable if explicitly given. Or, if \code{y} is tabulated, refer to the variable name as \code{(count)}. The larger the count for a level of \code{x}, the darker the bar. 


LEGEND\cr
When two variables are plotted, a legend is produced, with values for each level of the second or \code{by} variable.  By default, the location is placed in the right margin of the plot. This position can be changed with the \code{legend.position} option, which, in addition to the \code{lessR} option of \code{right.margin}, accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend.

If the default right margin is retained, variable labels are also accommodated for the legend title.  To conserve horizontal space, the variable label is listed in multiple lines if needed. The legend title is constructed by forming lines of maximum length of 12 characters, with multiple words per line if possible. Any single word in the label of more than 12 characters is abbreviated to 12 characters with the R \code{\link{abbreviate}} function. Also, any value labels are abbreviated to a maximum of 6 characters.

If the legend is not in the right margin, sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  This option only applies for the default vertical bars. Also, the legend is displayed vertically by default, but can be changed to horizontal with the \code{legend.horiz} option.

LONG CATEGORY NAMES\cr
For many plots, the names of the categories tend to be long. To adjust the plot for these long names, they can be rotated using the \code{rotate.x} and \code{rotate.y} parameters, in conjunction with \code{offset}, from the \code{\link{style}} function. The \code{offset} parameter moves the category name out from the axis to compensate for the rotation. The changes from \code{\link{style}} persist until further changes. To reset to the default after obtaining an analysis, use \code{style()}.

Also, the following codes are used to adjust line spacing:\cr
1. Any space in a category name is converted to a new line.\cr
2. If the space should not be converted to a new line, the replace with an underscore, which will display as a space and no line break.\cr

For the text output at the console, can specify the maximum number of characters in a label with \code{labels.max}. Longer value names are abbreviated to the specified length. This facilitates reading cross-tab tables. Also, a provided table pairs the abbreviated names with the actual names. For one variable frequency distributions, \code{out.size} provides the maximum number of characters for the text output before the horizontal display of the frequency distribution is shifted to a vertical presentation.

MULTIPLE BAR CHARTS ON THE SAME PANEL (PLOT)\cr
For multiple x-variables, set the parameter \code{one.plot} to \code{TRUE} to specify that each bar chart should be produced on the same panel as all other bars. This is most meaningful when all items have the same set of responses, such as a common Likert scale found in survey data. By default the one panel plot is produced when a common response scale is detected.

The algorithm to detect if the response scale is common first identifies the first variable with the largest set of responses, then checks the responses of all other variables. If all responses to all other variables are contained within the set of responses to the reference variable, then the response scales are the same. This means that on a Likert scale, for example, some items may not contain all possible responses, such as no one selects Strongly Disagree for an item. However, for the response scales to be deemed the same, at least one item (variable) must contain all possible responses.

Regardless, the \code{one.plot} parameter can be set to either \code{TRUE} or \code{FALSE} regardless of the commonality of responses. Setting this parameter explicitly saves some CPU time as the algorithm to evaluate the communality of responses need not be activated.


ENTER NUMERIC VARIABLE DIRECTLY\cr
Instead of calculating the counts from the data, the values of any numerical variable, including
the counts, can be entered directly as the \code{y}-variable, in addition to the categorical \code{x}-variable, and perhaps a categorical \code{by}-variable. See the examples below, under "bar chart directly from data".

Or, include the already tabulated counts as the data which is read into R, either as a matrix or a data frame.

STATISTICS\cr
In addition to the bar chart, descriptive and optional inferential statistics are also presented.  First, the frequency table for one variable or the joint frequency table for two variables is displayed. Second, the corresponding Cramer's V and chi-square test are also displayed by default.

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is listed as the label for the horizontal axis unless xlab is specified in the function call. If there are two variables to plot, the title of the resulting plot is based on the two variable labels, unless a specific title is listed with the \code{main} option. The variable label is also listed in the text output, next to the variable name. If the analysis is for two variables, then labels for both variables are included.

PDF OUTPUT\cr
To obtain pdf output, set the \code{pdf} option to \code{TRUE}, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{d}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > BarChart(cut(rnorm(50), breaks=seq(-5,5)))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- cut(rnorm(50), breaks=seq(-5,5))   # create vector Y in user workspace
    > BarChart(Y)     # directly reference Y}
}

\value{
The output can optionally be saved into an \code{R} object, otherwise it only appears in the console (unless \code{quiet} is set to \code{TRUE}). Two different types of components are provided: the pieces of readable output, and a variety of statistics. The readable output are character strings such as tables amenable for display. The statistics are numerical values amenable for further analysis. The motivation of these types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object and a \code{$}, can be inserted into the R~Markdown document (see \code{examples}), interspersed with explanation and interpretation. 


Tabulated numerical variable y\cr
------------------------------\cr
READABLE OUTPUT\cr
\code{out_title}: Title\cr
\code{out_lbl}:  Variable label\cr
\code{out_counts}: Two-way frequency distribution\cr
\code{out_chi}: Chi-square test\cr
\emph{One variable}: \code{out_miss}: Number of missing values\cr
\emph{Two variables}: \code{out_prop}: Cell proportions\cr
\emph{Two variables}: \code{out_row}: Cell proportions within each row\cr
\emph{Two variables}: \code{out_col}: Cell proportions within each col\cr

STATISTICS\cr
\code{n.dim}: Number of dimensions, 1 or 2\cr
\code{p_value}: p-value for null of equal proportions or independence\cr
\code{freq.df}: Data frame of the frequency distribution \cr
\emph{One variable}: \code{freq}: Frequency distribution\cr
\emph{One variable}: \code{prop}: Frequency distribution of proportions\cr 
\emph{One variable}: \code{n.miss}: Number of missing values\cr 


Numerical variable y read from data\cr
-----------------------------------\cr
\code{out_y}: Values of y\cr
\code{n.dim}: Number of dimensions, 1 or 2\cr
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
d <- rd("Employee", in.lessR=TRUE)

# --------------------------------------------------------
# bar chart from tabulating the data for a single variable
# --------------------------------------------------------

# for each level of Dept, display the frequencies
BarChart(Dept)
# short name
# bc(Dept)

# save the values output by BarChart into the myOutput list
myOutput <- BarChart(Dept)
# display the saved output
myOutput

# just males with salaries larger than 75,000 USD
BarChart(Dept, rows=(Gender=="M" & Salary > 85000))

# rotate and offset the axis labels, sort categories by frequencies
BarChart(Dept, rotate.x=45, offset=1, sort="-")

# set bars to a single color of blue with some transparency
BarChart(Dept, fill="blue", trans=0.3)
# progressive (sequential) color scale of blues
BarChart(Dept, fill="blues")

# change the theme just for this analysis, as opposed to style()
BarChart(Dept, theme="darkgreen")

# set bar color to hcl custom hues with chroma and luminance
#   at the values provided by the default hcl colors from
#   the getColors function, which defaults to h=240 and h=60
#   for the first two colors on the qualitative scale
bc(Gender, fill=c(hcl(h=180,c=100,l=55), hcl(h=0,c=100,l=55)))

# or set to unique colors via color names
BarChart(Gender, fill=c("palegreen3","tan"))

# darken the colors with an explicit call to getColors,
#   do a lower value of luminance, set to l=25
BarChart(Dept, fill=getColors(l=25), trans=0.4)

# column proportions instead of frequencies
BarChart(Gender, proportion=TRUE)

# map value of tabulated count to bar fill<D-d>
BarChart(Dept, fill=(count))

# data with many values of categorical variable Make and large labels
myd <- Read("Cars93", in.lessR=TRUE)
# perpendicular labels
bc(Make, rotate.x=90, data=myd)
# manage size of horizontal value labels
bc(Make, horiz=TRUE, label.max=4, data=myd)

# wes anderson move theme palette
# see getColors help file to see the full list of themes
BarChart(Dept, fill="GrandBudapest1", values="off")

# display bars for values of count <= 10 in a different color
#  than values above
BarChart(Dept, sort="+", fill.split=10)


# ----------------------------------------------------
# bar chart from tabulating the data for two variables
# ----------------------------------------------------

# at each level of Dept, show the frequencies of the Gender levels
BarChart(Dept, by=Gender)

# at each level of Dept, show the row proportions of the Gender levels
#   i.e., proportional stacked bar graph
BarChart(Dept, by=Gender, proportion=TRUE)

# at each level of Gender, show the frequencies of the Dept levels
# do not display percentages directly on the bars
BarChart(Gender, by=JobSat, fill="reds", values="off")

# specify two fill colors for Gender
BarChart(Dept, by=Gender, fill=getColors(c("deepskyblue", "black")))

# specify an ordered customized blue palette of colors for Dept
# colors can be named or customized with rgb function
# here "azure" is a single color instead of a range (ends in s)
BarChart(Gender, by=Dept,
         fill=getColors("azure", end.clr=rgb(100,110,200,max=255)))

# display bars beside each other instead of stacked, Female and Male
# the levels of Dept are included within each respective bar
# plot horizontally, display the value for each bar at the
#   top of each bar
BarChart(Gender, by=Dept, beside=TRUE, horiz=TRUE, values.position="out")

# horizontal bar chart of two variables, put legend on the top
BarChart(Gender, by=Dept, horiz=TRUE, legend.position="top")

# for more info on base R graphic options, enter:  help(par)
# for lessR options, enter:  style(show=TRUE)
# here fill is set in the style function instead of BarChart
#   along with the others
style(fill=c("coral3","seagreen3"), lab.color="wheat4", lab.cex=1.2,
      panel.fill="wheat1", main.color="wheat4")
BarChart(Dept, by=Gender,
         legend.position="topleft", legend.labels=c("Girls", "Boys"),
         xlab="Dept Level", main="Gender for Different Dept Levels",
         value.labels=c("None", "Some", "Much", "Ouch!"))
style()


# -----------------------------------------------------------------
# multiple bar charts tabulated from data across multiple variables
# -----------------------------------------------------------------

# bar charts for all non-numeric variables in the data frame called d
#   and all numeric variables with a small number of values, < n.cat
# BarChart(one.plot=FALSE)

d <- rd("Mach4", in.lessR=TRUE, quiet=TRUE)

# all on the same plot, bar charts for 20 6-pt Likert scale items
# default scale is divergent from "browns" to "blues"
BarChart(m01:m20, horiz=TRUE, values="off", sort="+")


\donttest{

# custom scale with explicit call to getColors, HCL chroma at 50
clrs <- getColors("greens", "purples", n=6, c=50)
BarChart(m01:m20, horiz=TRUE, values="off", sort="+", fill=clrs)

# custom divergent scale with pre-defined color palettes
#  with implicit call to getColors
BarChart(m01:m20, horiz=TRUE, values="off", fill=c("aquas", "rusts"))


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
d <- read.csv(text="
Dept, Salary
ACCT,51792.78
ADMN,71277.12
FINC,59010.68
MKTG,60257.13
SALE,68830.06", header=TRUE)
BarChart(Dept, Salary)

# specify two variables for a two variable bar chart
# also specify a y-variable to provide the counts directly
d <- read.csv(text="
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

d <- rd("Employee", in.lessR=TRUE)

# Place a message in the center of the plot
# \n indicates a new line
BarChart(Dept, add="Employees by\nDepartment", x1=3, y1=10)

# Use style to change some parameter values
style(add.trans=.8, add.fill="gold", add.color="gold4", add.lwd=0.5)
# Add a rectangle around the message centered at <3,10>
BarChart(Dept, add=c("rect", "Employees by\nDepartment"),
                     x1=c(2,3), y1=c(11, 10), x2=4, y2=9)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }




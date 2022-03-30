\name{Histogram}
\alias{Histogram}
\alias{hs}

\title{Histogram}

\description{
Abbreviation: \code{hs}

From the standard R function \code{\link{hist}}, plots a frequency histogram with default colors, including background color and grid lines plus an option for a relative frequency and/or cumulative histogram, as well as summary statistics and a table that provides the bins, midpoints, counts, proportions, cumulative counts and cumulative proportions. Bins can be selected several different ways besides the default, including specifying just the bin width and/or the bin start. Also provides improved error diagnostics and feedback for the user on how to correct the problem when the bins do not contain all of the specified data.

If a set of multiple variables is provided, including an entire data frame, then each numeric variable in that set of variables is analyzed, with the option to write the resulting histograms to separate pdf files. The related \code{\link{CountAll}} function does the same for all variables in the set of variables, histograms for continuous variables and bar charts for categorical variables. Specifying a \code{by1} or \code{by2} variable implements Trellis graphics.

When output is assigned into an object, such as \code{h} in \code{h <- hs(Y)}, can assess the pieces of output for later analysis. A primary such analysis is \code{knitr} for dynamic report generation from a generated R markdown file according to the \code{Rmd} option in which interpretative R output is embedded in documents. See \code{value} below.
}

\usage{
Histogram(x=NULL, data=d, rows=NULL,
          stat_x=c("count", "proportion"),
          n_cat=getOption("n_cat"), Rmd=NULL,

    by1=NULL, by2=NULL,
    n_row=NULL, n_col=NULL, aspect="fill",

    bin_start=NULL, bin_width=NULL, bin_end=NULL, breaks="Sturges",

    theme=getOption("theme"),
    fill=getOption("bar_fill_cont"),
    color=getOption("bar_color_cont"),
    trans=getOption("trans_bar_fill"),

    values=FALSE,
    reg="snow2", cumulate=c("off", "on", "both"),

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab_adj=c(0,0), margin_adj=c(0,0,0,0),

    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),
    scale_x=NULL, scale_y=NULL,

    density=FALSE, dn.hist=TRUE,
    bw=NULL, type=c("general", "normal", "both"),
    color_gen="gray20", color_nrm="gray20",
    fill_hist=getOption("se_fill"), fill_nrm=NULL, fill_gen=NULL,
    x.pt=NULL, y_axis=FALSE,
    rug=FALSE, color_rug="black", size_rug=0.5,

    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    eval_df=NULL, digits_d=NULL, quiet=getOption("quiet"), do_plot=TRUE,
    width=6, height=6, pdf_file=NULL, 
    fun_call=NULL, \ldots)

hs(\ldots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single numerical variable, 
        either within a data frame or as a vector in the users workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all numerical variables in the specified data
        frame, \code{d} by default.}
  \item{data}{Optional data frame that contains the variable(s) of interest,
        default is \code{d}.}
  \item{rows}{A logical expression that specifies a subset of rows of
       the data frame to analyze.}
  \item{stat_x}{Bin and transform values of variable \code{x} into
       \code{"counts"} by default or \code{"proportion"} if specified,
       that is, frequencies or relative frequencies.}
  \item{n_cat}{For the analysis of multiple variables, such as a data frame,
        specifies the largest number of unique values of variable of a numeric
        data type
        for which the variable will be analyzed as a categorical. Default is 0.}
  \item{Rmd}{File name for the file of R markdown to be written, if specified.
        The file type is .Rmd, which automatically opens in RStudio, but it is
        a simple
        text file that can be edited with any text editor, including RStudio.}\cr

  \item{by1}{A categorical variable called a conditioning variable that
        activates \bold{Trellis graphics}, from the lattice package, to provide
        a separate scatterplot (panel) of numeric primary variables \code{x}
        and \code{y} for each level of the variable.}
  \item{by2}{A second conditioning variable to generate Trellis
        plots jointly conditioned on both the \code{by1} and \code{by2} variables,
        with \code{by2} as the row variable, which yields a scatterplot (panel)
        for each cross-classification of the levels of numeric \code{x} and
        \code{y} variables.}
  \item{n_row}{Optional specification for the number of rows in the layout
        of a multi-panel display with Trellis graphics. Need not specify
        \code{ncols}.}
  \item{n_col}{Optional specification for the number of columns in the
        layout a multi-panel display with
        Trellis graphics. Need not specify \code{n_row} If set to 1, then
        the strip that labels each group is moved to the left of each plot
        instead of the top.}
  \item{aspect}{Lattice parameter for the aspect ratio of the panels,
        defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to "bank" to 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr

  \item{bin_start}{Optional specified starting value of the bins.}
  \item{bin_width}{Optional specified bin width, which can be specified with or
        without a \code{bin_start} value.}
  \item{bin_end}{Optional specified value that is within the last bin, so the
        actual endpoint of the last bin may be larger than the specified value.}
  \item{breaks}{The method for calculating the bins, or an explicit specification
       of the bins, such as with the standard R \code{\link{seq}} function or
       other options provided by the \code{\link{hist}} function that include
       the default \code{"Sturges"} plus \code{"Scott"} and \code{"FD"}. Not
       applicable and so not allowed if \code{density} is \code{TRUE}.}\cr

  \item{theme}{Color theme for this analysis. Make persistent across analyses
        with \code{\link{style}}.}
  \item{fill}{Fill color of the bars. 
        Can explicitly choose \code{"grays"}
        or \code{"hcl"} colors, or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}.
        Default is
        \code{bar_color} from the lessR \code{\link{style}} function.}
  \item{color}{Border color of the bars, can be a vector
        to customize the color for each bar. Default is
        \code{bar_color} from the lessR \code{\link{style}} function.}
  \item{trans}{Transparency factor of the area of each slice.
        Default is
        \code{trans_bar_fill} from the lessR \code{\link{style}} function.}\cr

  \item{values}{Replaces standard R \code{labels} options, which has multiple
       definitions in R. Specifies to display the count of each bin.}
  \item{reg}{The color of the superimposed, regular histogram when
        \code{cumulate="both"}.}
  \item{cumulate}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of
        \code{"both"} superimposes the regular histogram.}\cr

  \item{xlab}{Label for x-axis_ Defaults to variable name unless variable labels
        are present, the defaults to also include the corresponding variable
        label. Can style with the lessR \code{\link{style}} function}
  \item{ylab}{Label for y-axis_ Defaults to Frequency or Proportion.
       Can style with the lessR \code{\link{style}} function.}
  \item{main}{Label for the title of the graph.
       Can set size with \code{main_cex} and color with \code{main_color} from the
       lessR \code{\link{style}} function.}
  \item{sub}{Sub-title of graph, below xlab. Not yet implemented.}
  \item{lab_adj}{Two-element vector -- x-axis label, y-axis label -- adjusts
       the position of the axis labels in approximate inches. + values move
       the labels away from plot edge. Not applicable to Trellis graphics.}
  \item{margin_adj}{Four-element vector -- top, right, bottom and left --
       adjusts the margins of the plotted figure in approximate inches.
       + values move the corresponding margin away from plot edge. 
       Not applicable to Trellis graphics.}\cr

  \item{rotate_x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.
        Can set persistently with the lessR \code{\link{style}} function.}
  \item{rotate_y}{Degrees that the \code{y}-axis values are rotated.
        Can set persistently with the lessR \code{\link{style}} function.}
  \item{offset}{The amount of spacing between the axis values and the axis_ Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.
        Can set persistently with the lessR \code{\link{style}} function.}
  \item{scale_x}{If specified, a vector of three values that define the
        numerical values of the x-axis: starting, ending and
        number of intervals, within the bounds of plot region.}
  \item{scale_y}{Applies to the y-axis_ See \code{scale_x}.}\cr

  \item{density}{If \code{TRUE}, plot the smoothed \bold{kernel density}
        estimate.}
  \item{dn.hist}{When \code{density} is \code{TRUE}, plot a histogram
       behind the density curve.}
  \item{bw}{Bandwidth of \bold{kernel density} estimation. Initial value
       is "nrd0", but unless
       specified, then may be iterated upward to create a smoother curve.}
  \item{type}{Type of density curve plotted.  By default, the general
       density is plotted, though can request the normal density and
       both densities.}
  \item{color_gen}{Color of the general density curve.}
  \item{color_nrm}{Color of the normal curve.}
  \item{fill_hist}{Fill color for the histogram behind density curve,
       defaults to a light gray.} 
  \item{fill_nrm}{Fill color for the estimated normal curve, with a partially 
       transparent blue as the default, and transparent for the gray theme.}
  \item{fill_gen}{Fill color for the estimated general density curve, with a
        partially transparent light red as the default, and a light transparent
        gray for the gray theme.}
  \item{x.pt}{Value of the point on the x-axis for which to draw a unit interval
        around illustrating the corresponding area under the general density curve.
        Only applies when requesting \code{type=general}.}
  \item{y_axis}{Specifies if the y-axis, the density axis, should be included.}
  \item{rug}{If \code{TRUE}, add a rug plot, a direct display of density in the
        form of a narrow band beneath the density curve.}
  \item{color_rug}{Color of the rug ticks.}
  \item{size_rug}{Line width of the rug ticks.}\cr


  \item{add}{\bold{Draw one or more objects}, text or a geometric figures,
       on the plot.
       Possible values are any text to be written, the first argument, which is
       \code{"text"}, or, to indicate a figure,
       \code{"rect"} (rectangle), \code{"line"}, \code{"arrow"},
       \code{"v.line"} (vertical line), and \code{"h.line"} (horizontal line).
       The value \code{"means"} is short-hand for vertical and horizontal lines
       at the respective means. Does not apply to Trellis graphics.
       Customize with parameters such as \code{add_fill} and \code{add_color}
       from the \code{\link{style}} function.}
  \item{x1}{First x coordinate to be considered for each object.
           All coordinates vary from -1 to 1.}
  \item{y1}{First y coordinate to be considered for each object.}
  \item{x2}{Second x coordinate to be considered for each object.
        Only used for \code{"rect"}, \code{"line"} and \code{arrow}.}
  \item{y2}{Second y coordinate to be considered for each object.
        Only used for \code{"rect"}, \code{"line"} and \code{arrow}.}\cr

  \item{eval_df}{Determines if to check for existing data frame and
        specified variables. By default is \code{TRUE} 
        unless the \code{shiny} package is loaded then set to \code{FALSE} so
        that Shiny will run. Needs to be set to \code{FALSE} if using
        the pipe \code{\%\>\%} notation.}
  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
        with \code{\link{style}} function.}
  \item{do_plot}{If \code{TRUE}, the default, then generate the plot.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf_file}{Indicate to direct pdf graphics to the specified name of
        the pdf file.}
  \item{fun_call}{Function call. Used with \code{knitr} to pass the function call
        when obtained from the abbreviated function call \code{hs}.}\cr

  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{hist}} and \code{\link{par}} for general graphics,
      \code{xlim} and \code{ylim} for setting the range of the x and y-axes\cr
      \code{cex.main} for the size of the title\cr
      \code{col.main} for the color of the title\cr
      \code{cex} for the size of the axis value labels\cr
      \code{col.lab} for the color of the axis labels}
}


\details{
OVERVIEW\cr
Results are based on the standard R \code{\link{hist}} function to calculate and plot a histogram, or a multi-panel display of histograms with Trellis graphics, plus the additional provided color capabilities, a relative frequency histogram, summary statistics and outlier analysis. The \code{freq} option from the standard R \code{\link{hist}} function has no effect as it is always set to \code{FALSE} in each internal call to \code{\link{hist}}. To plot densities, use the \code{lessR} function \code{\link{Density}}.

VARIABLES and TRELLIS PLOTS\cr
At a minimum there is one primary variable, \code{x}, which results in a single histogram. Trellis graphics, from Deepayan Sarkar's \code{lattice} package, may be implemented in which multiple panels are displayed according to the levels of one or two categorical variables, called conditioning variables.  A variable specified with \code{by1} is a conditioning variable that results in a Trellis plot, the histogram of \code{x} produced at \emph{each} level of the \code{by1} variable. Inclusion of a second conditioning variable, \code{by2}, results in a separate histogram for \emph{each} combination of cross-classified values of both \code{by1} and \code{by2}. 

DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{d}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{d$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.

To obtain a histogram of each numerical variable in the \code{d} data frame, use \code{Histogram()}.  Or, for a data frame with a different name, insert the name between the parentheses. To analyze a subset of the variables in a data frame, specify the list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

The \code{rows} parameter subsets rows (cases) of the input data frame according to a logical expression. Use the standard R operators for logical statements as described in \code{\link{Logic}} such as \code{&} for and, \code{|} for or and \code{!} for not, and use the standard R relational operators as described in \code{\link{Comparison}} such as \code{==} for logical equality \code{!=} for not equals, and \code{>} for greater than. See the Examples.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{color_bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{style}}. The default color theme is \code{lightbronze}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{style}}, such as \code{"red"} and \code{"green"}. Use the option \code{style(sub_theme="black")} for a black background and partial transparency of plotted colors. 

For the color options, such as \code{fill}, the value of \code{"off"} is the same as \code{"transparent"}.

Set \code{fill} to a single color or a color range, of which there are many possibilities. For \code{"hues"} colors of the same chroma and luminance set \code{fill} to multiple colors all with the same saturation and brightness. Also available are the pre-specified R color schemes \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also provide pre-defined color ranges \code{"blues"}, \code{"reds"} and \code{"greens"}, or generate custom colors, such as from the lessR function \code{\link{getColors}}. 

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{d}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > Histogram(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > Histogram(Y)     # directly reference Y}

ERROR DETECTION\cr
A somewhat relatively common error by beginning users of the base R \code{\link{hist}} function may encounter is to manually specify a sequence of bins with the \code{seq} function that does not fully span the range of specified data values_ The result is a rather cryptic error message and program termination.  Here, \code{Histogram} detects this problem before attempting to generate the histogram with \code{\link{hist}}, and then informs the user of the problem with a more detailed and explanatory error message. Moreover, the entire range of bins need not be specified to customize the bins.  Instead, just a bin width need be specified, \code{bin_width}, and/or a value that begins the first bin, \code{bin_start}.  If a starting value is specified without a bin width, the default Sturges method provides the bin width.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf_file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.
}

\value{
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. Two different types of components are provided: the pieces of readable output, and a variety of statistics. The readable output are character strings such as tables amenable for display. The statistics are numerical values amenable for further analysis. The motivation of these types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object and a \code{$}, can be inserted into the R~Markdown document (see \code{examples}), interspersed with explanation and interpretation.

READABLE OUTPUT\cr
\code{out_suggest}: Suggestions for other similar analyses\cr
\code{out_summary}: Summary statistics\cr
\code{out_freq}: Frequency distribution\cr
\code{out_outliers}: Outlier analysis\cr

STATISTICS\cr
\code{bin_width}: Bin width\cr
\code{n_bins}: Number of bins\cr
\code{breaks}: Breaks of the bins\cr
\code{mids}: Bin midpoints\cr
\code{counts}: Bin counts\cr
\code{prop}: Bin proportion\cr 
\code{cumulate}: Bin cumulative counts\cr 
\code{cprop}: Bin cumulative proportion\cr
}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 5, NY: Routledge.

Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 4, NY: CRC Press.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871. 

Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R, Springer. http://lmdvr.r-forge.r-project.org/
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{getColors}}, \code{\link{hist}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{style}}.
}


\examples{
# get the data
d <- rd("Employee")


# make sure default style is active
style()


# --------------------
# different histograms
# --------------------

# histogram with all defaults
Histogram(Salary)
# short form
#hs(Salary)

# output saved for later analysis into object h
h <- hs(Salary)
# view full text output
h
# view just the outlier analysis
h$out_outliers
# list the names of all the components
names(h)

# histogram with no borders for the bars
Histogram(Salary, color="off")

# save the histogram to a pdf file
#Histogram(Salary, pdf=TRUE)

# just males employed more than 5 years
Histogram(Salary, rows=(Gender=="M" & Years > 5))

# histogram with red bars, black background, and black border
style(panel_fill="black", fill="red", panel_color="black")
Histogram(Salary)
# or use a lessR pre-defined sequential color palette
#   with some transparency
Histogram(Salary, fill="rusts", color="brown", trans=.1)

# histogram with purple color theme, translucent gold bars
style("purple", sub_theme="black")
Histogram(Salary)
# back to default color theme
style()

# histogram with specified bin width
# can also use bin_start
Histogram(Salary, bin_width=12000)

# histogram with rotated axis values, offset more from axis
# suppress text output
style(rotate_x=45, offset=1)
Histogram(Salary, quiet=TRUE)
style()

# histogram with specified bins and grid lines displayed over the histogram
Histogram(Salary, breaks=seq(0,150000,20000), xlab="My Variable")

# histogram with bins calculated with the Scott method and values displayed
Histogram(Salary, breaks="Scott", values=TRUE, quiet=TRUE)

# histogram with the number of suggested bins, with proportions
Histogram(Salary, breaks=15, stat_x="proportion")

# histogram with non-default values for x- and y-axes
d[2,4] <- 45000
Histogram(Salary, scale_x=c(30000,130000,5), scale_y=c(0,9.5,5))

# ----------------
# Trellis graphics
# ----------------
Histogram(Salary, by1=Dept)


# ---------------------
# cumulative histograms
# ---------------------

# cumulative histogram with superimposed regular histogram, all defaults
Histogram(Salary, cumulate="both")

# cumulative histogram plus regular histogram
Histogram(Salary, cumulate="both", reg="mistyrose")

# density
Histogram(Salary, density=TRUE)


# -------------------------------------------------
# histograms for data frames and multiple variables
# -------------------------------------------------

# create data frame, d, to mimic reading data with Read function
# d contains both numeric and non-numeric data
d <- data.frame(rnorm(50), rnorm(50), rnorm(50), rep(c("A","B"),25))
names(d) <- c("X","Y","Z","C")

# although data not attached, access the variable directly by its name
Histogram(X)

# histograms for all numeric variables in data frame called d
#  except for numeric variables with unique values < n_cat
# d is the default name, so does not need to be specified with data
Histogram()

# histogram with specified options, including red axis labels
style(fill="palegreen1", panel_fill="ivory", axis_color="red") 
Histogram(values=TRUE)
style()  # reset

# histograms for all specified numeric variables
# use the combine or c function to specify a list of variables
Histogram(c(X,Y))


# -----------
# annotations
# -----------

d <- rd("Employee")

# Place a message in the top-right of the graph
# Use \n to indicate a new line
hs(Salary, add="Salaries\nin our Company", x1=100000, y1=7)

# Use style to change some parameter values
style(add_trans=.8, add_fill="gold", add_color="gold4",
      add_lwd=0.5, add_cex=1.1)
# Add a rectangle around the message centered at <100000,7>
hs(Salary, add=c("rect", "Salaries\nin our Company"),
      x1=c(82000, 100000), y1=c(7.7, 7), x2=118000, y2=6.2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }



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
Histogram(x=NULL, data=mydata, n.cat=getOption("n.cat"), Rmd=NULL,

    by1=NULL, by2=NULL,
    n.row=NULL, n.col=NULL, aspect="fill",

    bin.start=NULL, bin.width=NULL, bin.end=NULL, breaks="Sturges",

    prop=FALSE, hist.counts=FALSE,
    reg="snow2", cumul=c("off", "on", "both"),

    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    rotate.x=getOption("rotate.x"),
    rotate.y=getOption("rotate.y"),
    offset=getOption("offset"),

    digits.d=NULL, quiet=getOption("quiet"), do.plot=TRUE,
    width=4.5, height=4.5, pdf=FALSE, 
    fun.call=NULL, \ldots)

hs(\ldots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single numerical variable, 
        either within a data frame or as a vector in the users workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all numerical variables in the specified data
        frame, \code{mydata} by default.}
  \item{data}{Optional data frame that contains the variable(s) of interest,
        default is \code{mydata}.}
  \item{n.cat}{For the analysis of multiple variables, such as a data frame,
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
  \item{n.row}{Optional specification for the number of rows in the layout
        of a multi-panel display with Trellis graphics. Need not specify
        \code{ncols}.}
  \item{n.col}{Optional specification for the number of columns in the
        layout a multi-panel display with
        Trellis graphics. Need not specify \code{n.row} If set to 1, then
        the strip that labels each group is moved to the left of each plot
        instead of the top.}
  \item{aspect}{Lattice parameter for the aspect ratio of the panels,
        defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to "bank" to 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr

  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or
        without a \code{bin.start} value.}
  \item{bin.end}{Optional specified value that is within the last bin, so the
        actual endpoint of the last bin may be larger than the specified value.}\cr
  \item{breaks}{The method for calculating the bins, or an explicit specification
       of the bins, such as with the standard R \code{\link{seq}} function or
       other options provided by the \code{\link{hist}} function.}\cr

  \item{prop}{Specify proportions or relative frequencies on the vertical axis.
       Default is \code{FALSE}.}
  \item{hist.counts}{Replaces standard R \code{labels} options, which has multiple
       definitions in R. Specifies to display the count of each bin.}
  \item{reg}{The color of the superimposed, regular histogram when
        \code{cumul="both"}.}
  \item{cumul}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of
        \code{"both"} superimposes the regular histogram.}\cr

  \item{xlab}{Label for x-axis. Defaults to variable name.}
  \item{ylab}{Label for y-axis. Defaults to Frequency or Proportion.}
  \item{main}{Title of graph.}
  \item{sub}{Sub-title of graph, below xlab.}\cr

  \item{rotate.x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate.y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}\cr

  \item{digits.d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
        with \code{\link{style}} function.}
  \item{do.plot}{If \code{TRUE}, the default, then generate the plot.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf}{If \code{TRUE}, graphics are to be redirected to a pdf file.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call
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
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{mydata}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.

To obtain a histogram of each numerical variable in the \code{mydata} data frame, use \code{Histogram()}.  Or, for a data frame with a different name, insert the name between the parentheses. To analyze a subset of the variables in a data frame, specify the list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{color.bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{style}}. The default color theme is \code{lightbronze}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{style}}, such as \code{"red"} and \code{"green"}. Use the option \code{style(sub.theme="black")} for a black background and partial transparency of plotted colors. 

For the color options, such as \code{fill}, the value of \code{"off"} is the same as \code{"transparent"}.

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > Histogram(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > Histogram(Y)     # directly reference Y}

ERROR DETECTION\cr
A somewhat relatively common error by beginning users of the base R \code{\link{hist}} function may encounter is to manually specify a sequence of bins with the \code{seq} function that does not fully span the range of specified data values. The result is a rather cryptic error message and program termination.  Here, \code{Histogram} detects this problem before attempting to generate the histogram with \code{\link{hist}}, and then informs the user of the problem with a more detailed and explanatory error message. Moreover, the entire range of bins need not be specified to customize the bins.  Instead, just a bin width need be specified, \code{bin.width}, and/or a value that begins the first bin, \code{bin.start}.  If a starting value is specified without a bin width, the default Sturges method provides the bin width.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.
}

\value{
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. Redesigned in \code{lessR} version 3.3 to provide two different types of components: the pieces of readable output, and a variety of statistics. The readable output are character strings such as tables amenable for reading. The statistics are numerical values amenable for further analysis. The motivation of these types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object and a \code{$}, can be inserted into the R~Markdown document (see \code{examples}).

READABLE OUTPUT\cr
code{out_ss}: Summary statistics\cr
code{out_freq}: Frequency distribution\cr
code{out_outliers}: Outlier analysis\cr
code{out_file}: Name and location of optional Rmd file\cr

STATISTICS\cr
code{bin_width}: Bin width\cr
code{n_bins}:  Number of bins\cr
code{breaks}: Breaks of the bins\cr
code{mids}: Bin midpoints\cr
code{counts}: Bin counts\cr
code{prop}: Bin proportion\cr 
code{counts_cumul}: Bin cumulative counts\cr 
code{prop_cumul}: Bin cumulative proportion\cr

Although not typically needed, if the output is assigned to an object named, for example, \code{h}, then the contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(h)}.
}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 5, NY: Routledge.

Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R, Springer. http://lmdvr.r-forge.r-project.org/
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{hist}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{style}}.
}


\examples{
# generate 50 random normal data values with three decimal digits
y <- round(rnorm(50),3)


# --------------------
# different histograms
# --------------------

# histogram with all defaults
Histogram(y)
# short form
hs(y)
# compare to standard R function hist
hist(y)

# output saved for later analysis into object h
h <- hs(y)
# view full text output
h
# view just the outlier analysis
h$out_outliers
# list the names of all the components
names(h)

# histogram with no borders for the bars, then restore default theme
style(bar.color="off")
Histogram(y)
style()

# save the histogram to a pdf file
Histogram(y, pdf=TRUE)

# histogram with red bars, black background, and black border
style(panel.fill="black", fill="red", panel.color="black")
Histogram(y)
# or set this color scheme for all subsequent analyses
#  here with the grid turned off as well
style(grid.color="off", panel.fill="black", bar.color="black")
Histogram(y)

# histogram with purple color theme, translucent gold bars
style("purple", sub.theme="black")
Histogram(y)
# back to default color theme
style()

# histogram with specified bin width
# can also use bin.start
Histogram(y, bin.width=.25)

# histogram with rotated axis values, offset more from axis
# suppress text output
Histogram(y, rotate.x=45, offset=1, quiet=TRUE)

# histogram with specified bins and grid lines displayed over the histogram
Histogram(y, breaks=seq(-5,5,.25), xlab="My Variable")

# histogram with bins calculated with the Scott method and values displayed
Histogram(y, breaks="Scott", hist.counts=TRUE, quiet=TRUE)

# histogram with the number of suggested bins, with proportions
Histogram(y, breaks=15, prop=TRUE)


# ----------------
# Trellis graphics
# ----------------
mydata <- rd("Employee", format="lessR", quiet=TRUE)
Histogram(Salary, by1=Dept)


# ---------------------
# cumulative histograms
# ---------------------

# cumulative histogram with superimposed regular histogram, all defaults
Histogram(y, cumul="both")

# cumulative histogram plus regular histogram
# present with proportions on vertical axis, override other defaults
Histogram(y, cumul="both", breaks=seq(-4,4,.25), prop=TRUE, 
  reg="mistyrose")


# -------------------------------------------------
# histograms for data frames and multiple variables
# -------------------------------------------------

# create data frame, mydata, to mimic reading data with Read function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(50), rnorm(50), rnorm(50), rep(c("A","B"),25))
names(mydata) <- c("X","Y","Z","C")

# although data not attached, access the variable directly by its name
Histogram(X)

# histograms for all numeric variables in data frame called mydata
#  except for numeric variables with unique values < n.cat
# mydata is the default name, so does not need to be specified with data
Histogram()

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
Histogram(breaks, data=warpbreaks)

# all histograms with specified options, including red axis labels
style(fill="palegreen1", panel.fill="ivory", axis.color="red") 
Histogram(hist.counts=TRUE)
style()  # reset

# histograms for all specified numeric variables
# use the combine or c function to specify a list of variables
Histogram(c(X,Y))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }



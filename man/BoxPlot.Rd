\name{BoxPlot}
\alias{BoxPlot}
\alias{bx}

\title{Boxplot}

\description{
Abbreviation: \code{bx}

Uses the standard R boxplot function, \code{\link{boxplot}} to display a boxplot in color. Also display the relevant statistics such as the hinges, median and IQR.

If the provided object to analyze is a set of multiple variables, including an entire data frame, then each non-numeric variable in the data frame is analyzed and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.

When output is assigned into an object, such as \code{b} in \code{b <- bx(Y)}, the pieces of output can be accessed for later analysis. A primary such analysis is \code{knitr} for dynamic report generation from an R Markdown file in which R output is embedded in documents, facilitated by the \code{Rmd} option. See \code{value} below.
}

\usage{
BoxPlot(x=NULL, data=mydata, n.cat=getOption("n.cat"),
        Rmd=NULL,

        fill=getOption("fill.bar"),
        stroke=getOption("stroke.bar"), 
        bg=getOption("bg"),
        grid=getOption("grid"),
        box=getOption("box"),

        cex.axis=0.75, axes="gray30",
        xlab=NULL, main=NULL, sub=NULL, digits.d=NULL,

        rotate.x=0, rotate.y=0, offset=0.5,

        horiz=TRUE, add.points=FALSE,

        quiet=getOption("quiet"),
        width=4.5, height=4.5, pdf=FALSE,
        do.plot=TRUE, fun.call=NULL, \ldots)

bx(\ldots)
}


\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single numerical variable, 
        either within a data frame or as a vector in the user's workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all numerical variables in the specified data
        frame, \code{mydata} by default.}
  \item{data}{Optional data frame that contains the variable(s) of interest,
        default is \code{mydata}.}
  \item{n.cat}{For the analysis of multiple variables, such as a data frame,
        specifies the largest number of unique values of variable of a numeric
        data type for which the variable will be analyzed as a categorical.}
  \item{Rmd}{File name for the file of R markdown instructions to be written,
        if specified.}\cr

  \item{fill}{Interior color of the box. Turn off with \code{fill="off"}.
       This and the following colors can also be changed globally, individually
       and as 
       a color theme, with the \code{lessR} \code{\link{global}} function.
       The \code{lessR} function \code{\link{showColors}} provides examples of all
       R named colors.}
  \item{stroke}{Color of any points that designate outliers. By default this
       is the same color as the box.}
  \item{bg}{Color of the plot background. Turn off with \code{bg="off"}.}
  \item{grid}{Color of the grid lines. Turn off with \code{grid="off"}.}
  \item{box}{Color of border around the plot background, the box, that encloses 
        the plot. Turn off with \code{box="off"}.}\cr

 \item{cex.axis}{Scale magnification factor, which by defaults displays the axis
       values to be smaller than the axis labels. Provides the functionality of, and
       can be replaced by, the standard R \code{cex.axis.}}
  \item{axes}{Color of the font used to label the axis values.}
  \item{xlab}{Label for the value axis, which defaults to the variable's name.}
  \item{main}{Title of graph.}
  \item{sub}{Sub-title of graph, below xlab.}
  \item{digits.d}{Number of decimal digits displayed in the listing of the summary
       statistics.}\cr
  \item{rotate.x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate.y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}\cr

  \item{horiz}{Orientation of the boxplot. Set \code{FALSE} for vertical.}
  \item{add.points}{If \code{TRUE}, then place a dot plot (i.e., stripchart) over the
       box plot.}\cr

  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{global}} function.}
  \item{width}{Width of the plot window in inches, defaults to 4.5.}
  \item{height}{Height of the plot window in inches, defaults to 4.5.}
  \item{pdf}{If \code{TRUE}, graphics to be redirected to a pdf file.}
  \item{do.plot}{If \code{TRUE}, the default, then generate the plot.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{bx}.}\cr

  \item{\dots}{Other parameter values for graphics as defined processed 
       by \code{\link{boxplot}} and \code{\link{bxp}} such as \code{whiskcol} for
       the whisker color, etc. and \code{\link{par}}, including \code{ylim} to set
       the limits of the value axis, \code{lwd} for the line width, \code{cex.lab}
       for the size of the label, \code{color.main} for the title, \code{color.lab},
       \code{sub}, \code{color.sub}, \code{srt} to rotate the axis value labels,
       and \code{color.ticks} to specify the color of the tick marks.}
}


\details{
OVERVIEW\cr
Unlike the standard R boxplot function, \code{\link{boxplot}}, the default here is for a horizontal boxplot.  Also, \code{BoxPlot} does not currently process in formula mode, so use the standard R \code{\link{boxplot}} function to process a formula in which a boxplot is displayed for a variable at each level of a second, usually categorical, variable.

Other graphic parameters are available to format the display, such as \code{main} for the title, and other parameters found in \code{\link{boxplot}} and \code{\link{par}}. To minimize white space around the boxplot, re-size the graphics window before or after creating the boxplot.


DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{mydata}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.

To obtain a box plot of each numerical variable in the \code{mydata} data frame, use \code{BoxPlot()}.  Or, for a data frame with a different name, insert the name between the parentheses.  To analyze a subset of the variables in a data frame, specify the list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{fill} for the color of the box. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{global}}. The default color theme is \code{dodgerblue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{global}}, such as \code{"red"} and \code{"green"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors.  

For the color options, such as \code{grid}, the value of \code{"off"} is the same as 
\code{"transparent"}.

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > BoxPlot(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > BoxPlot(Y)     # directly reference Y}

}

\value{
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. Redesigned in \code{lessR} version 3.3 to provide two different types of components: the pieces of readable output, and a variety of statistics. The readable output are character strings such as tables amenable for reading. The statistics are numerical values amenable for further analysis. The motivation of these types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object and a \code{$}, can be inserted into the \code{Rmd} document (see \code{examples}).

READABLE OUTPUT\cr
code{out_stats}: Summary statistics for a box plot\cr
code{out_outliers}: Outlier analysis\cr
code{out_file}: Name and location of optional R markdown file\cr

STATISTICS\cr
code{n}: Number of data values analyzed\cr
code{n.miss}:  Number of missing data values\cr
code{min}:  Minimum\cr
code{lower_whisker}: Lower whisker \cr
code{lower_hinge}:  Lower hinge\cr
code{median}: Median\cr
code{upper_hinge}: Upper hinge\cr
code{upper_whisker}: Upper whisker \cr
code{max}: Maximum \cr
code{IQR}: Inter-quartile range \cr

Although not typically needed, if the output is assigned to an object named, for example, \code{h}, then the contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(h)}.
}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 5, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{boxplot}}, \code{\link{bxp}}, \code{\link{par}}, \code{\link{global}}.
}

\examples{
# simulate data and get at least one outlier
y <- rnorm(100,50,10)
y[1] <- 90


# ------------------------------
# box plot for a single variable
# ------------------------------

# standard horizontal boxplot with all defaults
BoxPlot(y)

# short name
bx(y)

# save the box plot to a pdf file
BoxPlot(y, pdf=TRUE)

# vertical boxplot with plum color
BoxPlot(y, horiz=FALSE, fill="plum")

# box plot with outliers more strongly highlighted
BoxPlot(y, stroke="red", xlab="My Variable")


# ------------------------------------------------
# box plots for data frames and multiple variables
# ------------------------------------------------

# read internal lessR dataset
# mydata contains both numeric and non-numeric data
mydata <- rd("Employee", format="lessR", quiet=TRUE)

# box plot with superimposed dot plot (stripchart)
BoxPlot(Salary, add.points=TRUE)

# box plot with results saved to object b instead of displaying 
b <- BoxPlot(Salary)
# show the results
b
# show just the piece regarding the statistics
b$out_stats
# list the names of all the components
names(b)

# box plot with rotated axis values, offset more from axis
BoxPlot(Salary, rotate.x=45, offset=1)

# BoxPlot generates R markdown file to be "knit"
#  such as in RStudio
bx(Salary, Rmd="myout")

# box plots for all numeric variables in data frame called mydata
BoxPlot()

# box plots for all numeric variables in data frame called mydata
#  with specified options
BoxPlot(fill="palegreen1", stroke="plum")

# Use the subset function to specify a variable list
# box plots for all specified numeric variables
BoxPlot(c(Salary,Years))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ boxplot }





\name{BoxPlot}
\alias{BoxPlot}
\alias{bx}

\title{Boxplot}

\description{
Abbreviation: \code{bx}

Uses the standard R boxplot function, \code{\link{boxplot}} to display a boxplot in color. Also display the relevant statistics such as the hinges, median and IQR.

If the provided object to analyze is a set of multiple variables, including an entire data frame, then each non-numeric variable in the data frame is analyzed and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.
}

\usage{
BoxPlot(x=NULL, data=mydata, n.cat=getOption("n.cat"),

        col.fill=getOption("col.fill.bar"),
        col.stroke=getOption("col.stroke.bar"), 
        col.bg=getOption("col.bg"),
        col.grid=getOption("col.grid"),

        cex.axis=.85, col.axis="gray30",
        xlab=NULL, main=NULL, digits.d=NULL,

        horiz=TRUE, add.points=FALSE,

        quiet=getOption("quiet"),
        pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

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
        specifies the largest number of unique values of variable of a numeric data type
        for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{col.fill}{Color of the box.}
  \item{col.stroke}{Color of any points that designate outliers. By default this
       is the same color as the box.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis
       values to be smaller than the axis labels. Provides the functionality of, and
       can be replaced by, the standard R \code{cex.axis.}}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{xlab}{Label for the value axis, which defaults to the variable's name.}
  \item{main}{Title of graph.}
  \item{digits.d}{Number of decimal digits displayed in the listing of the summary
       statistics.}
  \item{horiz}{Orientation of the boxplot. Set \code{FALSE} for vertical.}
  \item{add.points}{If \code{TRUE}, then place a dot plot (i.e., stripchart) over the
       box plot.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{set}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected. If there is
       no filetype of \code{.pdf}, the filetype is added to the name.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for graphics as defined processed 
       by \code{\link{boxplot}} and \code{\link{bxp}} such as \code{whiskcol} for
       the whisker color, etc. and \code{\link{par}}, including \code{ylim} to set
       the limits of the value axis, \code{lwd} for the line width, \code{cex.lab}
       for the size of the label, \code{col.main} for the title, etc., and \code{col.ticks} to specify the color of the tick marks.}
}


\details{
OVERVIEW\cr
Unlike the standard R boxplot function, \code{\link{boxplot}}, the default here is for a horizontal boxplot.  Also, \code{BoxPlot} does not currently process in formula mode, so use the standard R \code{\link{boxplot}} function to process a formula in which a boxplot is displayed for a variable at each level of a second, usually categorical, variable.

Other graphic parameters are available to format the display, such as \code{main} for the title, and other parameters found in \code{\link{boxplot}} and \code{\link{par}}. To minimize white space around the boxplot, re-size the graphics window before or after creating the boxplot.


DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{mydata}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.

To obtain a box plot of each numerical variable in the \code{mydata} data frame, use \code{BoxPlot()}.  Or, for a data frame with a different name, insert the name between the parentheses.  To analyze a subset of the variables in a data frame, specify the list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{col.fill} for the color of the box. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{set}}. The default color theme is \code{dodgerblue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}, such as \code{"red"} and \code{"green"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors.  

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > BoxPlot(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > BoxPlot(Y)     # directly reference Y}

}

\value{
Based on the standard \code{R} function \code{\link{boxplot}}, invisibly returns a list with components described in \code{\link{boxplot}}.
}

\references{
Gerbing, D. W. (2013). R Data Analysis without Programming, Chapter 5, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{boxplot}}, \code{\link{bxp}}, \code{\link{par}}, \code{\link{set}}.
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
BoxPlot(y, pdf.file="MyBoxPlot.pdf")

# vertical boxplot with plum color
BoxPlot(y, horiz=FALSE, col.fill="plum")

# box plot with outliers more strongly highlighted
BoxPlot(y, col.stroke="red", xlab="My Variable")


# ------------------------------------------------
# box plots for data frames and multiple variables
# ------------------------------------------------

# create data frame, mydata, to mimic reading data with rad function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(100), rnorm(100), rnorm(100), rep(c("A","B"),50))
names(mydata) <- c("X","Y","Z","C")

# box plot for variable X from data frame, referred to directly
BoxPlot(X)

# box plot with superimposed dot plot (stripchart)
BoxPlot(X, add.points=TRUE)

# box plots for all numeric variables in data frame called mydata
BoxPlot()

# box plots for all numeric variables in data frame called mydata
#  with specified options
BoxPlot(col.fill="palegreen1", col.stroke="plum")

# Use the subset function to specify a variable list
# box plots for all specified numeric variables
BoxPlot(c(X,Y))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ boxplot }



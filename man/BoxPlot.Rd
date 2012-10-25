\name{BoxPlot}
\alias{BoxPlot}
\alias{bx}
\alias{bx.default}
\alias{bx.data.frame}

\title{Boxplot}

\description{
Abbreviation: \code{bx}

Uses the standard R boxplot function, \code{\link{boxplot}} to display a boxplot in color. Also display the relevant statistics such as the hinges, median and IQR.

If the provided object for which to calculate the box plot is a data frame, then a box plot is calculated for each numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{
BoxPlot(x=NULL, dframe=mydata, n.cat=getOption("n.cat"), text.out=TRUE, \ldots)

\method{bx}{data.frame}(x, n.cat, text.out, \ldots) 

\method{bx}{default}(x,
      col.fill=getOption("col.fill.pt"),
      col.stroke=getOption("col.stroke.pt"), 
      col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),

      cex.axis=.85, col.axis="gray30", col.ticks="gray30",
      xlab=NULL, main=NULL, digits.d=NULL, text.out=TRUE,

      horiz=TRUE, add.points=FALSE,
      pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

bx(\ldots)
}


\arguments{
  \item{x}{Variable for which to construct the histogram.  Can be a data frame.  If
       not specified with \code{dframe}, that is, no variable specified, then the
       data frame \code{mydata} is assumed.}
  \item{dframe}{Optional data frame that contains the variables of interest, default
       is \code{mydata}.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the
       largest number of unique values of variable of a numeric data type for 
       which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{col.fill}{Color of the box.}
  \item{col.stroke}{Color of any points that designate outliers. By default this is the 
       same color as the box.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis
       values to be smaller than the axis labels. Provides the functionality of, and
       can be replaced by, the standard R \code{cex.axis.}}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{col.ticks}{Color of the ticks used to label the axis values.}
  \item{xlab}{Label for the value axis, which defaults to the variable's name.}
  \item{main}{Title of graph.}
  \item{digits.d}{Number of decimal digits displayed in the listing of the summary
       statistics.}
  \item{text.out}{If \code{TRUE}, then display text output in console.}
  \item{horiz}{Orientation of the boxplot. Set \code{FALSE} for vertical.}
  \item{add.points}{If \code{TRUE}, then place a dot plot (i.e., stripchart) over the
       box plot.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for graphics as defined processed 
       by \code{\link{boxplot}} and \code{\link{par}}, including \code{ylim} to set
       the limits of the value axis, \code{lwd} for the line width, \code{cex.lab}
       for the size of the label, and \code{col.main} for the title, etc.}
}


\details{
OVERVIEW\cr
Unlike the standard R boxplot function, \code{\link{boxplot}}, the default here is for a horizontal boxplot.  Also, \code{BoxPlot} does not currently process in formula mode, so use the standard R \code{\link{boxplot}} function to process a formula in which a boxplot is displayed for a variable at each level of a second, usually categorical, variable.

Other graphic parameters are available to format the display, such as \code{main} for the title, and other parameters found in \code{\link{boxplot}} and \code{\link{par}}. To minimize white space around the boxplot, re-size the graphics window before or after creating the boxplot.
\code{\link{den}}.

DATA\cr
If the variable is in a data frame, the input data frame has the assumed name of \code{mydata}.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the \code{mydata$name} notation. 

To obtain a box plot of each numerical variable in the \code{mydata} data frame, use \code{BoxPlot()}.  Or, for a data frame with a different name, insert the name between the parentheses.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{col.bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}, such as \code{"red"} and \code{"green"}. Use the option \code{ghost=TRUE} for a black backgound, no gridlines and partial transaparency of plotted colors.  

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in a data frame called \code{mylabels}, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > BoxPlot(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > BoxPlot(Y)     # directly reference Y}

}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{boxplot}}, \code{\link{par}}, \code{\link{set}}.
}

\examples{
# simulate data and get at least one outlier
y <- rnorm(100,50,10)
y[1] <- 90


# -----------------------------
# boxplot for a single variable
# -----------------------------

# standard horizontal boxplot with all defaults
BoxPlot(y)

# short name
bx(y)

# save the box plot to a pdf file
BoxPlot(y, pdf.file="MyBoxPlot.pdf")

# vertical boxplot with plum color
BoxPlot(y, horiz=FALSE, col.fill="plum")

# boxplot with outliers more strongly highlighted
BoxPlot(y, col.stroke="red", xlab="My Variable")


# -----------------------------------------------
# boxplots for data frames and multiple variables
# -----------------------------------------------

# create data frame, mydata, to mimic reading data with rad function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(100), rnorm(100), rnorm(100), rep(c("A","B"),50))
names(mydata) <- c("X","Y","Z","C")

# boxplot for variable X from data frame, referred to directly
BoxPlot(X)

# boxplot with superimposed dot plot (stripchart)
BoxPlot(X, dotplot=TRUE)

# boxplots for all numeric variables in data frame called mydata
BoxPlot()

# boxplots for all numeric variables in data frame called mydata
#  with specified options
BoxPlot(col.fill="palegreen1", col.stroke="plum")

# Use the subset function to specify a variable list
mysub <- subset(mydata, select=c(X,Y))
BoxPlot(dframe=mysub)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ boxplot }



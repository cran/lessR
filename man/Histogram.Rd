\name{Histogram}
\alias{Histogram}
\alias{hst}
\alias{hst.default}
\alias{hst.data.frame}


\title{Histogram with Color}

\description{
Abbreviation: \code{hst}

Accessing the standard R function \code{\link{hist}}, plots a frequency histogram with default colors, including background color and grid lines plus an option for a relative frequency and/or cumulative histogram, as well as summary statistics and a table that provides the bins, midpoints, counts, proportions, cumulative counts and cumulative proportions. Bins can be selected several different ways besides the default, including specifying just the bin width. Also provides improved error diagnostics and feedback for the user on how to correct the problem when the bins do not contain all of the specified data.

If the provided object for which to calculate the histogram is a data frame, then a histogram is calculated for each numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{

Histogram(x=NULL, dframe=mydata, n.cat=getOption("n.cat"), text.out=TRUE, \ldots)

\method{hst}{data.frame}(x, n.cat, text.out, \ldots) 

\method{hst}{default}(x, col.bars=NULL, col.border=NULL, col.bg=NULL, 
         col.grid=NULL, col.reg="snow2", over.grid=FALSE,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         prop=FALSE, cumul=c("off", "on", "both"), 
         digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL, text.out=TRUE,
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) 

hst(\ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram.  Can be a data frame.  If not
      specified with \code{dframe}, that is, no variable specified, then the data frame
      \code{mydata} is assumed.}
  \item{dframe}{Optional data frame that contains the variable of interest, default
        is \code{mydata}.} \item{n.cat}{When analyzing all the variables in a data frame,
        specifies the largest number of unique values of variable of a numeric data type
        for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{col.bars}{Color of the bars.}
  \item{col.border}{Color of the border of the bars.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{col.reg}{The color of the superimposed, regular histogram when \code{cumul="both"}.}
  \item{over.grid}{If \code{TRUE}, plot the grid lines over the histogram.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis
        values to be smaller than the axis labels. Provides the functionality of,
        and can be replaced by, the standard R \code{cex.axis.}}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{col.ticks}{Color of the ticks used to label the axis values.}
  \item{colors}{Sets the color palette.}
  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other options 
       provided by the \code{\link{hist}} function.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{prop}{Specify proportions or relative frequencies on the vertical axis. Default
       is \code{FALSE}.}
  \item{cumul}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of \code{"both"} 
        superimposes the regular histogram.}
  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{xlab}{Label for x-axis. Defaults to variable name.}
  \item{ylab}{Label for y-axis. Defaults to Frequency or Proportion.}
  \item{main}{Title of graph.}
  \item{text.out}{If \code{TRUE}, then display text output in console.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{hist}} and \code{\link{plot}}, including \code{xlim}, \code{ylim},
      \code{lwd} and \code{cex.lab}, \code{col.main}, \code{density}, etc. Also
      includes \code{labels=TRUE} to display the frequency of the bin at the top
      of the corresponding bar.}
}


\details{
OVERVIEW\cr
Results are based on the standard R \code{\link{hist}} function for calculating and plotting a histogram, with the additional provided color capabilities and other options including a relative frequency histogram. 

However, a histogram with densities is not supported. The \code{freq} option from the standard R \code{\link{hist}} function has no effect as it is always set to \code{FALSE} in each internal call to \code{\link{hist}}.  To plot densities, which correspond to setting \code{freq} to \code{FALSE}, use the \code{lessR} function \code{\link{den}}.

DATA\cr
If the variable is in a data frame, the input data frame has the assumed name of \code{mydata}.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the \code{mydata$name} notation.

To obtain a histogram of each numerical variable in the \code{mydata} data frame, use \code{Histogram()}.  Or, for a data frame with a different name, insert the name between the parentheses.

COLOR THEME\cr
Individual colors in the plot can be manipulated with options such as \code{col.bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option. Or, the color theme can be changed for all subsequent graphical analysis with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in a data frame called \code{mylabels}, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > Histogram(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > Histogram(Y)     # directly reference Y}

ERROR DETECTION\cr
A somewhat relatively common error by beginning users of the base R \code{\link{hist}} function may encounter is to manually specify a sequence of bins with the \code{seq} function that does not fully span the range of specified data values. The result is a rather cryptic error message and program termination.  Here, \code{Histogram} detects this problem before attempting to generate the histogram with \code{\link{hist}}, and then informs the user of the problem with a more detailed and explanatory error message. Moreover, the entire range of bins need not be specified to customize the bins.  Instead, just a bin width need be specified, \code{bin.width}, and/or a value that begins the first bin, \code{bin.start}.  If a starting value is specified without a bin width, the default Sturges method provides the bin width.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{hist}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{set}}.
}


\examples{
# generate 100 random normal data values with three decimal digits
y <- round(rnorm(100),3)


# --------------------
# different histograms
# --------------------

# histogram with all defaults
Histogram(y)
# short form
hst(y)
# compare to standard R function hist
hist(y)
# save the histogram to a pdf file
Histogram(y, pdf.file="MyHistogram.pdf")

# histogram with only gray colors, similar to ggplot colors
Histogram(y, colors="gray")

# histogram with specified bin width
# can also use bin.start
Histogram(y, bin.width=.25)

# histogram with specified bins and grid lines displayed over the histogram
Histogram(y, breaks=seq(-5,5,.25), xlab="My Variable", over.grid=TRUE)

# histogram with bins calculated with the Scott method and values displayed
Histogram(y, breaks="Scott", labels=TRUE)

# histogram with the number of suggested bins, with proportions
Histogram(y, breaks=25, prop=TRUE)

# histogram with specified colors, overriding defaults
# col.bg and col.grid are defined in histogram
# all other parameters are defined in hist, par and plot functions
Histogram(y, col.bars="darkblue", col.border="lightsteelblue4", col.bg="ivory",
  col.grid="darkgray", density=25, angle=-45, cex.lab=.8, cex.axis=.8,
  col.lab="sienna3", main="My Title", col.main="gray40", xlim=c(-5,5), lwd=2,
  xlab="My Favorite Variable")


# ---------------------
# cumulative histograms
# ---------------------

# cumulative histogram with superimposed regular histogram, all defaults
Histogram(y, cumul="both")

# cumulative histogram plus regular histogram
# present with proportions on vertical axis, override other defaults
Histogram(y, cumul="both", breaks=seq(-4,4,.25), prop=TRUE, 
  col.reg="mistyrose")


# -------------------------------------------------
# histograms for data frames and multiple variables
# -------------------------------------------------

# create data frame, mydata, to mimic reading data with rad function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(100), rnorm(100), rnorm(100), rep(c("A","B"),50))
names(mydata) <- c("X","Y","Z","C")

# although data not attached, access the variable directly by its name
Histogram(X)

# histograms for all numeric variables in data frame called mydata
#  except for numeric variables with unique values < n.cat
# mydata is the default name, so does not need to be specified with dframe
Histogram()

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
Histogram(breaks, dframe=warpbreaks)
Histogram()

# histograms for all numeric variables in data frame called mydata
#  with specified options
Histogram(col.bars="palegreen1", col.bg="ivory", labels=TRUE)

# Use the subset function to specify a variable list
# histograms for all specified numeric variables
mysub <- subset(mydata, select=c(X,Y))
Histogram(dframe=mysub)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }


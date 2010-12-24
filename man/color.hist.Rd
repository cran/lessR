\name{color.hist}
\alias{color.hist}
\alias{color.hist.default}
\alias{color.hist.data.frame}

\title{Histogram with Color}

\description{
Using the standard R function \code{\link{hist}}, plots a frequency histogram with default colors, including background color and gridlines plus an option for a relative frequency and/or cumulative histogram, as well as summary statistics and a table that provides the bins, midpoints, counts, proportions, cumulative counts and cumulative proportions. Bins can be selected several different ways besides the default, including specifying just the bin width. Also provides improved error diagnostics and feedback for the user on how to correct the problem when the bins do not contain all of the specified data.

If the provided object for which to calculate the histogram is a data frame, then a histogram is calculated for each numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{

color.hist(x=NULL, \ldots)

\method{color.hist}{data.frame}(x, \ldots) 

\method{color.hist}{default}(x, col="lightsteelblue", border="black", col.bg="seashell",
         col.grid="grey90",  over.grid=FALSE,
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         show.values=FALSE, prop=FALSE, cumul=c("off", "on", "both"),
         col.reg="snow2", digits.d=5, xlab=NULL, main=NULL, \ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram.  Can be a data frame.  If not
      specified, then the data frame \code{mydata} is assumed.}
  \item{col}{Color of the histogram's bars.}
  \item{border}{Color of the border of the bars.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{over.grid}{If true, plot the grid lines over the histogram.}
  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other options provided by the \code{\link{hist}} function.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{show.values}{If true, display the frequency of the bin at the top of the
       corresponding bar.}
  \item{prop}{Specify proportions or relative frequencies on the vertical axis. Default
       is \code{FALSE}.}
  \item{cumul}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of \code{"both"} superimposes the regular histogram.}
  \item{col.reg}{The color of the superimposed, regular histogram when \code{cumul="both"}.}
  \item{xlab}{Label for x-axis. Defaults to variable name.}
  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{main}{Title of graph.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{hist}} and \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, \code{col.main}, \code{density}, etc.}
}


\details{
Results are based on the standard \code{\link{hist}} function for calculating and plotting a histogram, with the additional provided color capabilities and other options including a relative frequency histogram.  However, a histogram with densities is not supported.

A somewhat common error by beginning users of the base R \code{\link{hist}} function may encounter is to manually specify a sequence of bins with the \code{seq} function that does not fully span the range of specified data values. The result is a rather cryptic error message and program termination.  Here, \code{color.hist} detects this problem before attempting to generate the histogram with \code{\link{hist}}, and then informs the user of the problem with a more detailed and explanatory error message. Moreover, the entire range of bins need not be specified to customize the bins.  Instead, just a bin width need be specified, \code{bin.width}, and/or a value that begins the first bin, \code{bin.start}.  If a starting value is specified without a bin width, the default Sturges method provides the bin width.

The \code{freq} option from the traditional \code{\link{hist}} function has no effect as it is always off in each internal call to \code{\link{hist}}.  To plot densities, which correspond to setting  \code{freq} to \code{FALSE}, use the \code{\link{color.density}} function in this package.
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{hist}}, \code{\link{plot}}, \code{\link{par}}.
}


\examples{
# generate 100 random normal data values with three decimal digits
y <- round(rnorm(100),3)


# --------------------
# different histograms
# --------------------

# histogram with all defaults
color.hist(y)

# histogram with specified bin width
color.hist(y, bin.width=.25)

# histogram with specified bins and grid lines displayed over the histogram
color.hist(y, breaks=seq(-5,5,.25), xlab="My Variable", over.grid=TRUE)

# histogram with bins calculated with the Scott method and values displayed
color.hist(y, breaks="Scott", show.values=TRUE)

# histogram with the number of suggested bins, with proportions
color.hist(y, breaks=25, prop=TRUE)

# histogram with specified colors, overriding defaults
# col.bg and col.grid are defined in color.hist
# all other parameters are defined in hist, par and plot functions
color.hist(y, col="darkblue", border="lightsteelblue4", col.bg="ivory",
  col.grid="darkgray", density=25, angle=-45, cex.lab=.8, cex.axis=.8,
  col.lab="sienna3", main="My Title", col.main="gray40", xlim=c(-5,5), lwd=2,
  xlab="My Favorite Variable")


# ---------------------
# cumulative histograms
# ---------------------

# cumulative histogram with superimposed regular histogram, all defaults
color.hist(y, cumul="both")

# cumulative histogram plus regular histogram
# present with proportions on vertical axis, override other defaults
color.hist(y, cumul="both", breaks=seq(-4,4,.25), prop=TRUE, 
  col.reg="mistyrose")


# ---------------------------------
# histograms for multiple variables
# ---------------------------------

# read data into data frame called mydata
rad("http://web.pdx.edu/~gerbing/data/employees2.csv")

# histograms for all numeric variables in data frame called mydata
color.hist()

# histograms for all numeric variables in data frame called mydata
#  with specified options
color.hist(col="palegreen1", col.bg="ivory", show.values=TRUE)

# Use the subset function to specify a variable list
color.hist(subset(mydata, select=c(Age,HealthPlan)))

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }

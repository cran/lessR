\name{color.hist}
\alias{color.hist}

\title{Histogram with Color}

\description{
Using the standard R function \code{\link{hist}}, plots a frequency histogram with default colors, including background color and gridlines plus an option for a relative frequency and/or cumulative histogram, as well as summary statistics and a table that provides the bins, midpoints, counts, proportions, cumulative counts and cumulative proportions.  Also provides improved error diagnostics and feedback for the user on how to correct the problem when the bins do not contain all of the specified data.
}

\usage{
color.hist(x, col="lightsteelblue", border="black", col.bg="seashell",
         col.grid="grey90", breaks="Sturges", over.grid=FALSE,
         show.values=FALSE, prop=FALSE, cumul=c("off", "on", "both"), 
         col.reg="seashell2", digits=5, xlab=NULL, main=NULL, \ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram.}
  \item{col}{Color of the histogram's bars.}
  \item{border}{Color of the border of the bars.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other options provided by the \code{\link{hist}} function.}
  \item{over.grid}{If true, plot the grid lines over the histogram.}
  \item{show.values}{If true, display the frequency of the bin at the top of the
       corresponding bar.}
  \item{prop}{Specify proportions or relative frequencies on the vertical axis. Default
       is \code{FALSE}.}
  \item{cumul}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of \code{"both"} superimposes the regular histogram.}
  \item{col.reg}{The color of the superimposed, regular histogram when \code{cumul="both"}.}
  \item{xlab}{Label for x-axis.}
  \item{digits}{Number of significant digits for each of the displayed summary statistics.}
  \item{main}{Title of graph.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{hist}} and \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, \code{col.main}, \code{density}, etc.}
}


\details{
Results are based on the standard \code{\link{hist}} function for calculating and plotting a histogram, with the additional provided color capabilities and other options.  However, a histogram with densities is not supported.

An error by beginning users of the base R \code{\link{hist}} function may encounter is to manually specify a sequence of bins with the \code{seq} function that does not fully span the range of specified data values. The result is a rather cryptic error message and program termination.  Here, \code{color.hist} detects this problem before attempting to generate the histogram with \code{\link{hist}}, and then informs the user of the problem with a more detailed and explanatory error message. 

The \code{freq} option from the traditional \code{\link{hist}} function has no effect as it is always off in each internal call to \code{\link{hist}}.  To plot densities, which correspond to setting  \code{freq} to \code{FALSE}, use the \code{\link{color.density}} function in this package.
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{hist}}, \code{\link{plot}}, \code{\link{par}}.
}


\examples{
# generate 100 random normal data values
y <- rnorm(100)

# histogram with all defaults
color.hist(y)

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
  col.lab="sienna3", main="My Title", col.main="gray40", xlim=c(-5,5), lwd=2)

# cumulative histogram with superimposed regular histogram, all defaults
color.hist(y, cumul="both")

# cumulative histogram plus regular histogram
# present with proportions on vertical axis, override other defaults
color.hist(y, cumul="both", breaks=seq(-4,4,.25), prop=TRUE, 
  col.reg="mistyrose")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }

\name{hst}
\alias{hst}
\alias{hst.default}
\alias{hst.data.frame}
\alias{color.hist}

\title{Histogram with Color}

\description{
Accessing the standard R function \code{\link{hist}}, plots a frequency histogram with default colors, including background color and gridlines plus an option for a relative frequency and/or cumulative histogram, as well as summary statistics and a table that provides the bins, midpoints, counts, proportions, cumulative counts and cumulative proportions. Bins can be selected several different ways besides the default, including specifying just the bin width. Also provides improved error diagnostics and feedback for the user on how to correct the problem when the bins do not contain all of the specified data.

If the provided object for which to calculate the histogram is a data frame, then a histogram is calculated for each numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{

hst(x=NULL, dframe=mydata, ncut=4, \ldots)

\method{hst}{data.frame}(x, ncut, \ldots) 

\method{hst}{default}(x, col.bars="lightsteelblue", col.border="black",
         col.bg="ghostwhite", col.grid="grey90", over.grid=FALSE,
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         prop=FALSE, cumul=c("off", "on", "both"), mag.axis=.85,
         col.reg="snow2", digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL, \ldots)

color.hist(\ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram.  Can be a data frame.  If not
      specified with \code{dframe}, that is, no variable specified, then the data frame \code{mydata}
      is assumed.}
  \item{dframe}{Optional data frame that contains the variable of interest, default is \code{mydata}.}
  \item{ncut}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be anlayzed as a categorical. Set to 0 to turn off.}
  \item{col.bars}{Color of the histogram's bars.}
  \item{col.border}{Color of the border of the bars.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{over.grid}{If \code{TRUE}, plot the grid lines over the histogram.}
  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other options 
       provided by the \code{\link{hist}} function.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{prop}{Specify proportions or relative frequencies on the vertical axis. Default
       is \code{FALSE}.}
  \item{mag.axis}{Scale magnification factor, which by defaults displays the axis values to be smaller than
        the axis labels. Provides the functionality of, and can be replaced by, the standard R \code{cex.axis.}}
  \item{cumul}{Specify a cumulative histogram. The value of \code{"on"} displays the 
        cumulative histogram, with default of \code{"off"}. The value of \code{"both"} 
        superimposes the regular histogram.}
  \item{col.reg}{The color of the superimposed, regular histogram when \code{cumul="both"}.}
  \item{xlab}{Label for x-axis. Defaults to variable name.}
  \item{ylab}{Label for y-axis. Defaults to Frequency or Proportion.}
  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{main}{Title of graph.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{hist}} and \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} 
      and \code{cex.lab}, \code{col.main}, \code{density}, etc. Also includes \code{labels=TRUE} to display the frequency of the bin at the top of the
       corresponding bar}
}


\details{
Results are based on the standard \code{\link{hist}} function for calculating and plotting a histogram, with the additional provided color capabilities and other options including a relative frequency histogram.  However, a histogram with densities is not supported.

If the variable is in a data frame, the input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the mydata$name notation. If no variable is specified, then all numeric variables in the entire data frame are analyzed and the results written to a pdf file.

A somewhat common error by beginning users of the base R \code{\link{hist}} function may encounter is to manually specify a sequence of bins with the \code{seq} function that does not fully span the range of specified data values. The result is a rather cryptic error message and program termination.  Here, \code{hst} detects this problem before attempting to generate the histogram with \code{\link{hist}}, and then informs the user of the problem with a more detailed and explanatory error message. Moreover, the entire range of bins need not be specified to customize the bins.  Instead, just a bin width need be specified, \code{bin.width}, and/or a value that begins the first bin, \code{bin.start}.  If a starting value is specified without a bin width, the default Sturges method provides the bin width.

The \code{freq} option from the the standard R \code{\link{hist}} function has no effect as it is always set to \code{FALSE} in each internal call to \code{\link{hist}}.  To plot densities, which correspond to setting  \code{freq} to \code{FALSE}, use the \code{\link{dens}} function in this package.

A labels data frame named \code{mylabels}, obtained from the \code{\link{rad}} function, can list the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the corresponding variable label is listed as the title of the resulting plot, unless a specific label is listed with the \code{main} option. The varible label is also listed in the text output, next to the variable name. 
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

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
hst(y)
# compare to standard R function hist
hist(y)

# histogram with specified bin width
# can also use bin.start
hst(y, bin.width=.25)

# histogram with specified bins and grid lines displayed over the histogram
hst(y, breaks=seq(-5,5,.25), xlab="My Variable", over.grid=TRUE)

# histogram with bins calculated with the Scott method and values displayed
hst(y, breaks="Scott", labels=TRUE)

# histogram with the number of suggested bins, with proportions
hst(y, breaks=25, prop=TRUE)

# histogram with specified colors, overriding defaults
# col.bg and col.grid are defined in histogram
# all other parameters are defined in hist, par and plot functions
hst(y, col.bars="darkblue", col.border="lightsteelblue4", col.bg="ivory",
  col.grid="darkgray", density=25, angle=-45, cex.lab=.8, cex.axis=.8,
  col.lab="sienna3", main="My Title", col.main="gray40", xlim=c(-5,5), lwd=2,
  xlab="My Favorite Variable")


# ---------------------
# cumulative histograms
# ---------------------

# cumulative histogram with superimposed regular histogram, all defaults
hst(y, cumul="both")

# cumulative histogram plus regular histogram
# present with proportions on vertical axis, override other defaults
hst(y, cumul="both", breaks=seq(-4,4,.25), prop=TRUE, 
  col.reg="mistyrose")


# -------------------------------------------------
# histograms for data frames and multiple variables
# -------------------------------------------------

# create data frame, mydata, to mimic reading data with rad function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(100), rnorm(100), rnorm(100), rep(c("A","B"),50))
names(mydata) <- c("X","Y","Z","C")
rm(X); rm(Y); rm(Z); rm(C);

# although data not attached, access the variable directly by its name
hst(X)

# histograms for all numeric variables in data frame called mydata
#  except for numeric variables with unique values < ncut
# mydata is the default name, so does not need to be specified with dframe
hst()

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
hst(breaks, dframe=warpbreaks)
hst()

# histograms for all numeric variables in data frame called mydata
#  with specified options
hst(col.bars="palegreen1", col.bg="ivory", labels=TRUE)

# Use the subset function to specify a variable list
# histograms for all specified numeric variables
mysub <- subset(mydata, select=c(X,Y))
hst(dframe=mysub)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }

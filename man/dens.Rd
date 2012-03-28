\name{dens}
\alias{dens}
\alias{color.density}

\title{Data Based Density Curves with Color and Histogram}

\description{
Plots a normal density curve and/or a general density curve superimposed over a histogram, all estimated from the data. Also reports the Shapiro-Wilk normality test.
}

\usage{
dens(x, dframe=mydata, 
         bw="nrd0", type=c("both", "general", "normal"),
         col.bg="ghostwhite", col.grid="grey90", col.bars="grey86",
         col.nrm="black", col.gen="black",
         col.fill.nrm=rgb(80,150,200, alpha=70, maxColorValue=255), 
         col.fill.gen=rgb(250,210,230, alpha=70, maxColorValue=255),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, \ldots)

color.density(\ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram and density plot.}
  \item{dframe}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{bw}{Bandwidth of kernel estimation.}
  \item{type}{Type of density curve plotted.  By default, both the general
       density and the normal density are plotted.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{col.bars}{Default is to display the histogram in a light gray.  To suppress,
       the histogram, specify a color of \code{"transparent"}.}
  \item{col.nrm}{Color of the normal curve.}
  \item{col.gen}{Color of the general density curve.}
  \item{col.fill.nrm}{Fill color for the estimated normal curve, with a transparent blue as
       the default.}
  \item{col.fill.gen}{Fill color for the estimated general density curve, with a transparent 
       light red as the default.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{text.out}{If \code{TRUE}, then display text output in console.}
  \item{x.pt}{Value of the point on the x-axis for which to draw a unit interval
        around illustrating the corresponding area under the general density curve.  Only applies when requesting \code{type=general}.}
  \item{xlab}{Label for x-axis.}
  \item{main}{Title of graph.}
  \item{y.axis}{Specifies is the y-axis, the density axis, should be included.}
  \item{x.min}{Smallest value of the variable x plotted on the x-axis.}
  \item{x.max}{Largest value of the variable x plotted on the x-axis.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, \code{col.main}, \code{density}, and, for the general density calculations, can set bandwidth with the standard \code{bw}, etc.}
}


\details{
Results are based on the standard \code{\link{dnorm}} function and \code{\link{density}} R functions for estimating densities from data, as well as the \code{\link{hist}} function for calculating a histogram.  Colors are provided by default and can also be specified.

The input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name without having to invoke the mydata$name notation. Any missing data values are removed and the effecive sample size and number of missing values reported.

By default, the histogram is displayed in a light gray, as a background for the normal and/or general estimated density curves, though this color can be changed. Using the \code{alpha} option for the \code{\link{rgb}} function, the density curves are by default plotted with transparent colors to facilitate comparison to the background histogram. 

The default histogram is the same as the default provided by the \code{\link{hist}} function itself. The default can be modified with the \code{bin.start} and \code{bin.width} options.  Use the \code{\link{hst}} function in this package for more control over the parameters of the histogram.

The limits for the axes are automatically calculated so as to provide sufficient space for the  density curves and histogram, and should generally not require user intervention.  Also, the curves are centered over the plot window so that the resulting density curves are symmetric even if the underlying histogram is not.  The estimated normal curve is based on the corresponding sample mean and standard deviation.

If \code{x.pt} is specified, then \code{type} is set to general and \code{y.axis} set to \code{TRUE}.

A labels data frame named \code{mylabels}, obtained from the \code{\link{rad}} function, can list the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the corresponding variable label is listed as the title of the resulting plot, unless a specific label is listed with the \code{main} option. 
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{dnorm}}, \code{\link{density}}, \code{\link{hist}}, \code{\link{plot}}, \code{\link{rgb}}, \code{\link{shapiro.test}}.
}


\examples{
# generate 100 random normal data values
y <- rnorm(100)

# normal curve and general density curves superimposed over histogram
# all defaults
dens(y)

# suppress the histogram, leaving only the density curves
# specify x-axis label per the xlab option for the plot function
dens(y, col.bars="transparent", xlab="My Var")

# specify (non-transparent) colors for the curves,
# to make transparent, need alpha option for the rgb function
dens(y, col.nrm="darkgreen", col.gen="plum")

# display only the general estimated density
#  so do not display the estimated normal curve
# specify the bandwidth for the general density curve,
#  use the standard bw option for the density function
dens(y, type="general", bw=.6)

# display only the general estimated density and a corresponding
#  interval of unit width around x.pt
dens(y, type="general", x.pt=2)

# create data frame, mydata, to mimic reading data with rad function
# although data not attached, access the variable directly by its name
mydata <- data.frame(rnorm(100))
names(mydata) <- "X"
dens(X)

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
dens(breaks, dframe=warpbreaks)
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ density }
\keyword{ color }

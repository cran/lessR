\name{color.density}
\alias{color.density}

\title{Data Based Density Curves with Color and Histogram}

\description{
Plots a normal density curve and/or a general density curve superimposed over a histogram, all estimated from the data.
}

\usage{
color.density(x, col.nrm="darkblue", col.gen="blue", col.bg="ghostwhite",  
         col.grid="grey90", col.hist="grey86",
         col.fill.nrm=rgb(80,150,200, alpha=70, max=255), 
         col.fill.gen=rgb(250,210,230, alpha=70, max=255),
         bin.start=NULL, bin.width=NULL,
         type=c("both", "general", "normal"), x.pt=NULL,
         xlab=NULL, main=NULL, \ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram and density plots.}
  \item{col.nrm}{Color of the normal curve.}
  \item{col.gen}{Color of the general density curve.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{col.hist}{Default is to display the histogram in a light gray.  To suppress,
       the histogram, specify a color of \code{"transparent"}.}
  \item{col.fill.nrm}{Fill color for the estimated normal curve, with a transparent blue as
       the default.}
  \item{col.fill.gen}{Fill color for the estimated general density curve, with a transparent 
       light red as the default.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{type}{Type of density curve plotted.  By default, both the general
       density and the normal density are plotted.}
  \item{x.pt}{Value of the point on the x-axis for which to draw a unit interval
        around illustrating the corresponding area under the general density curve.  Only applies when requesting \code{type=general}.}
  \item{xlab}{Label for x-axis.}
  \item{main}{Title of graph.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, \code{col.main}, \code{density}, and, for the general density calculations, can set bandwidth with the standard \code{bw}, etc.}
}


\details{
Results are based on the standard \code{\link{dnorm}} function and \code{\link{density}} R functions for estimating densities from data, as well as the \code{\link{hist}} function for calculating a histogram.  Colors are provided by default and can also be specified.

By default, the histogram is displayed in a light gray, as a background for the normal and/or general estimated density curves, though this color can be changed. Using the \code{alpha} option for the \code{\link{rgb}} function, the density curves are by default plotted with transparent colors to facilitate comparison to the background histogram. 

The default histogram is the same as the default provided by the \code{\link{hist}} function itself. The default can be modified with the \code{bin.start} and \code{bin.width} options.  Use the \code{\link{color.hist}} function in this package for more control over the parameters of the histogram.

The limits for the axes are automatically calculated so as to provide sufficient space for the  density curves and histogram, and should generally not require user intervention.  Also, the curves are centered over the plot window so that the resulting density curves are symmetric even if the underlying histogram is not.  The estimated normal curve is based on the corresponding sample mean and standard deviation.
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{dnorm}}, \code{\link{density}}, \code{\link{hist}}, \code{\link{plot}}, \code{\link{rgb}}.
}


\examples{
# generate 100 random normal data values
y <- rnorm(100)

# normal curve and general density curves superimposed over histogram
# all defaults
color.density(y)

# suppress the histogram, leaving only the density curves
# specify x-axis label per the xlab option for the plot function
color.density(y, col.hist="transparent", xlab="My Var")

# specify (non-transparent) colors for the curves,
# to make transparent, need alpha option for the rgb function
color.density(y, col.fill.nrm="darkgreen", col.fill.gen="plum")

# display only the general estimated density
#  so do not display the estimated normal curve
# specify the bandwidth for the general density curve,
#  use the standard bw option for the density function
color.density(y, type="general", bw=.6)

# disply only the general estimated density and a corresponding
#  interval of unit width around x.pt
color.density(y, type="general", x.pt=2)
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ density }
\keyword{ color }

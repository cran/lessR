\name{Density}
\alias{Density}
\alias{den}

\title{Density Curves from Data plus Histogram}

\description{
Abbreviation: \code{den}

Plots a normal density curve and/or a general density curve superimposed over a histogram, all estimated from the data. Also reports the Shapiro-Wilk normality test.
}

\usage{
Density(x, dframe=mydata, 
         bw="nrd0", type=c("both", "general", "normal"),
         bin.start=NULL, bin.width=NULL, text.out=TRUE,

         col.bg=NULL, col.grid=NULL, col.bars=NULL,
         col.nrm="black", col.gen="black",
         col.fill.nrm=NULL, col.fill.gen=NULL,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 

         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         x.pt=NULL, xlab=NULL, main=NULL, y.axis=FALSE, 
         x.min=NULL, x.max=NULL, band=FALSE,
         pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

den(\ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram and density plot.}
  \item{dframe}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{bw}{Bandwidth of kernel estimation.}
  \item{type}{Type of density curve plotted.  By default, both the general
       density and the normal density are plotted.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{text.out}{If \code{TRUE}, then display text output in console.}
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
  \item{colors}{Sets the intensity of the color palette.}
  \item{cex.axis}{Scale magnification factor, which by default displays the axis values to be 
        smaller than the axis labels.}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{col.ticks}{Color of the ticks used to label the axis values.}
  \item{x.pt}{Value of the point on the x-axis for which to draw a unit interval
        around illustrating the corresponding area under the general density curve.  Only
        applies when requesting \code{type=general}.}
  \item{xlab}{Label for x-axis.}
  \item{main}{Title of graph.}
  \item{y.axis}{Specifies if the y-axis, the density axis, should be included.}
  \item{x.min}{Smallest value of the variable x plotted on the x-axis.}
  \item{x.max}{Largest value of the variable x plotted on the x-axis.}
  \item{band}{If \code{TRUE}, add a rug plot, a direct display of density in the form of a
        narrow band beneath the density curve}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, 
      \code{col.main}, \code{density}, and, for the general density calculations, can set 
      bandwidth with the standard \code{bw}, etc.}
}


\details{
OVERVIEW\cr
Results are based on the standard \code{\link{dnorm}} function and \code{\link{density}} R functions for estimating densities from data, as well as the \code{\link{hist}} function for calculating a histogram.  Colors are provided by default and can also be specified.

The default histogram can be modified with the \code{bin.start} and \code{bin.width} options.  Use the \code{\link{Histogram}} function in this package for more control over the parameters of the histogram.

The limits for the axes are automatically calculated so as to provide sufficient space for the  density curves and histogram, and should generally not require user intervention.  Also, the curves are centered over the plot window so that the resulting density curves are symmetric even if the underlying histogram is not.  The estimated normal curve is based on the corresponding sample mean and standard deviation.

If \code{x.pt} is specified, then \code{type} is set to general and \code{y.axis} set to \code{TRUE}.

DATA\cr
If the variable is in a data frame, the input data frame has the assumed name of \code{mydata}.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the \code{mydata$name} notation.

COLOR THEME\cr
Individual colors in the plot can be manipulated with options such as \code{col.bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option. Or, the color theme can be changed for all subsequent graphical analysis with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in a data frame called \code{mylabels}, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > Density(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > Density(Y)     # directly reference Y}
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
Density(y)

# short name
den(y)

# save the density plot to a pdf file
Density(y, pdf.file="MyDensityPlot.pdf")

# suppress the histogram, leaving only the density curves
# specify x-axis label per the xlab option for the plot function
Density(y, col.bars="transparent", xlab="My Var")

# specify (non-transparent) colors for the curves,
# to make transparent, need alpha option for the rgb function
Density(y, col.nrm="darkgreen", col.gen="plum")

# display only the general estimated density
#  so do not display the estimated normal curve
# specify the bandwidth for the general density curve,
#  use the standard bw option for the density function
Density(y, type="general", bw=.6)

# display only the general estimated density and a corresponding
#  interval of unit width around x.pt
Density(y, type="general", x.pt=2)

# create data frame, mydata, to mimic reading data with rad function
# although data not attached, access the variable directly by its name
mydata <- data.frame(rnorm(100))
names(mydata) <- "X"
Density(X)

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
Density(breaks, dframe=warpbreaks)
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ density }
\keyword{ color }



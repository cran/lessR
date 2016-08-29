\name{Density}
\alias{Density}
\alias{dn}

\title{Density Curves from Data plus Histogram}

\description{
Abbreviation: \code{dn}

Plots a normal density curve and/or a general density curve superimposed over a histogram, all estimated from the data. Also reports the Shapiro-Wilk normality test and summary statistics.

If the provided object to analyze is a set of multiple variables, including an entire data frame, then each non-numeric variable in the data frame is analyzed and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.

When output is assigned into an object, such as \code{d} in \code{d <- dn(Y)}, the pieces of output can be accessed for later analysis. A primary such analysis is \code{knitr} for dynamic report generation from an R markdown document in which R output is embedded in documents, facilitated by the \code{Rmd} option. See \code{value} below.
}

\usage{
Density(x, data=mydata, n.cat=getOption("n.cat"), 

       bw="nrd0", type=c("both", "general", "normal"),
       histogram=TRUE, bin.start=NULL, bin.width=NULL,

       Rmd=NULL, digits.d=NULL,

       color.fill=getOption("color.fill.pt"),
       color.bg=getOption("color.bg"),
       color.grid=getOption("color.grid"),
       color.box=getOption("color.box"),

       color.nrm="black", color.gen="black",
       color.fill.nrm=NULL, color.fill.gen=NULL,

       cex.axis=0.75, color.axis="gray30",

       rotate.values=0, offset=0.5,

       x.pt=NULL, xlab=NULL, main=NULL, sub=NULL, y.axis=FALSE, 
       x.min=NULL, x.max=NULL, band=FALSE, 

       quiet=getOption("quiet"),
       pdf.file=NULL, pdf.width=5, pdf.height=5,
       fun.call=NULL, \ldots)

dn(\ldots)
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
        for which the variable will be analyzed as categorical. Default is 0.}
  \item{bw}{Bandwidth of kernel estimation.}
  \item{type}{Type of density curve plotted.  By default, both the general
       density and the normal density are plotted.}
  \item{histogram}{If \code{TRUE} overlay the density plot over a histogram.}
  \item{bin.start}{Optional specified starting value of the bins.}
  \item{bin.width}{Optional specified bin width, which can be specified with or without 
        a \code{bin.start} value.}
  \item{Rmd}{File name for the file of R markdown to be written, if specified.
        The file type is .Rmd, which automatically opens in RStudio, but it is a simple
        text file that can be edited with any text editor, including RStudio.}
  \item{digits.d}{Number of significant digits for each of the displayed summary statistics.}
  \item{color.fill}{Default (for default color theme of \code{"dodgerblue"}) is to
       display the histogram in a light gray.  To suppress, the histogram,
       specify a color of \code{"transparent"}.}
  \item{color.bg}{Color of the plot background.}
  \item{color.grid}{Color of the grid lines.}
  \item{color.box}{Color of border around the plot background, the box, that encloses 
        the plot, with a default of \code{"black"}.}
  \item{color.nrm}{Color of the normal curve.}
  \item{color.gen}{Color of the general density curve.}
  \item{color.fill.nrm}{Fill color for the estimated normal curve, with a partially 
       transparent blue as the default, and transparent for the gray theme.}
  \item{color.fill.gen}{Fill color for the estimated general density curve, with a partially
       transparent light red as the default, and a light transparent gray for the gray theme.}
  \item{cex.axis}{Scale magnification factor, which by default displays the axis values to be 
        smaller than the axis labels.}
  \item{color.axis}{Color of the font used to label the axis values.}
  \item{rotate.values}{Degrees that the axis values are rotated, usually to accommodate
        longer values, typically used in conjunction with \code{offset}.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}
  \item{x.pt}{Value of the point on the x-axis for which to draw a unit interval
        around illustrating the corresponding area under the general density curve.  Only
        applies when requesting \code{type=general}.}
  \item{xlab}{Label for x-axis.}
  \item{main}{Title of graph.}
  \item{sub}{Sub-title of graph, below xlab.}
  \item{y.axis}{Specifies if the y-axis, the density axis, should be included.}
  \item{x.min}{Smallest value of the variable x plotted on the x-axis.}
  \item{x.max}{Largest value of the variable x plotted on the x-axis.}
  \item{band}{If \code{TRUE}, add a rug plot, a direct display of density in the form of a
        narrow band beneath the density curve}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
        with \code{\link{theme}} function.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected. If there is
       no filetype of \code{.pdf}, the filetype is added to the name.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{dn}.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, 
      \code{color.main}, \code{color.lab}, \code{sub},
      \code{color.sub}, and \code{color.ticks} to specify the color of
      the ticks used to label the axis values, \code{density}, \code{srt} to
      rotate the axis value labels and, for the general
      density calculations, can set bandwidth with the standard \code{bw}.}
}


\details{
OVERVIEW\cr
Results are based on the standard \code{\link{dnorm}} function and \code{\link{density}} R functions for estimating densities from data, as well as the \code{\link{hist}} function for calculating a histogram.  Colors are provided by default and can also be specified.

The default histogram can be modified with the \code{bin.start} and \code{bin.width} options.  Use the \code{\link{Histogram}} function in this package for more control over the parameters of the histogram.

The limits for the axes are automatically calculated so as to provide sufficient space for the  density curves and histogram, and should generally not require user intervention.  Also, the curves are centered over the plot window so that the resulting density curves are symmetric even if the underlying histogram is not.  The estimated normal curve is based on the corresponding sample mean and standard deviation.

If \code{x.pt} is specified, then \code{type} is set to general and \code{y.axis} set to \code{TRUE}.

DATA\cr
The data may either be a vector from the global environment, the user's workspace, as illustrated in the examples below, or one or more variable's in a data frame, or a complete data frame. The default input data frame is \code{mydata}. Can specify the source data frame name with the \code{data} option.  If multiple variables are specified, only the numerical variables in the list of variables are analyzed. The variables in the data frame are referenced directly by their names, that is, no need to invoke the standard \code{R} mechanisms of the \code{mydata$name} notation, the \code{\link{with}} function or the  \code{\link{attach}} function. If the name of the vector in the global environment and of a variable in the input data frame are the same, the vector is analyzed.

COLOR THEME\cr
Individual colors in the plot can be manipulated with options such as \code{color.bars} for the color of the histogram bars. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{theme}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{theme}}, such as \code{"red"} and \code{"green"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors. 

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{pdf.width} and \code{pdf.height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name (or list of variable names). This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > Density(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > Density(Y)     # directly reference Y}
}

\value{
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. Redesigned in \code{lessR} version 3.3 to provide two different types of components: the pieces of readable output, and a variety of statistics. The readable output are character strings such as tables amenable for reading. The statistics are numerical values amenable for further analysis. The motivation of these types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object and a \code{$}, can be inserted into the R~Markdown document (see \code{examples}).

READABLE OUTPUT\cr
code{out_title}: Title of output\cr
code{out_stats}: Statistics\cr
code{out_file}: Name and location of optional R markdown file\cr

STATISTICS\cr
code{bw}: Bandwidth parameter\cr
code{n}: Number of data values analyzed\cr
code{n.miss}: Number of missing data values\cr
code{W}: W statistic for Shapiro-Wilk normality test\cr
code{pvalue}: p-value for W statistic\cr

Although not typically needed, if the output is assigned to an object named, for example, \code{h}, then the contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(h)}.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{dnorm}}, \code{\link{density}}, \code{\link{hist}}, \code{\link{plot}}, \code{\link{rgb}}, \code{\link{shapiro.test}}.
}


\examples{

# create data frame, mydata, to mimic reading data with Read function
# mydata contains both numeric and non-numeric data
mydata <- data.frame(rnorm(50), rnorm(50), rnorm(50), rep(c("A","B"),25))
names(mydata) <- c("X","Y","Z","C")

# normal curve and general density curves superimposed over histogram
# all defaults
Density(Y)

# short name
dn(Y)

# save the density plot to a pdf file
Density(Y, pdf.file="MyDensityPlot.pdf")

# suppress the histogram, leaving only the density curves
# specify x-axis label per the xlab option for the plot function
Density(Y, color.fill="transparent", xlab="My Var")

# specify (non-transparent) colors for the curves,
# to make transparent, need alpha option for the rgb function
Density(Y, color.nrm="darkgreen", color.gen="plum")

# display only the general estimated density
#  so do not display the estimated normal curve
# specify the bandwidth for the general density curve,
#  use the standard bw option for the density function
Density(Y, type="general", bw=.6)

# display only the general estimated density and a corresponding
#  interval of unit width around x.pt
Density(Y, type="general", x.pt=2)

# generate R markdown file to be "knit" such as in RStudio
dn(Y, Rmd="myout")

# variable of interest is in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
Density(breaks, data=warpbreaks)

# densities for all numeric variables in a data frame
Density()

# densities for all specified numeric variables in a list of variables
# e.g., use the combine or c function to specify a list of variables
Density(c(X,Y))
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ density }
\keyword{ color }



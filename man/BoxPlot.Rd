\name{BoxPlot}
\alias{BoxPlot}
\alias{bx}
\alias{bx.default}
\alias{bx.data.frame}
\alias{color.boxplot}

\title{Boxplot}

\description{
Abbreviation: \code{bx}

Uses the standard R boxplot function, \code{\link{boxplot}} to display a boxplot in color. Also display the relevant statistics such as the hinges, median and IQR.

If the provided object for which to calculate the box plot is a data frame, then a box plot is calculated for each numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{
BoxPlot(x=NULL, dframe=mydata, \ldots)

\method{bx}{data.frame}(x, \ldots) 

\method{bx}{default}(x, col.box=NULL, col.pts=NULL, 
        col.bg=NULL, col.grid=NULL,
        colors=c("blue", "gray", "rose", "green", "gold", "red"), 
        cex.axis=.85, col.axis="gray30", col.ticks="gray30",
        horiz=TRUE, dotplot=FALSE,
        xlab=NULL, main=NULL, digits.d=NULL, \ldots)

bx(\ldots)

color.boxplot(\ldots)
}


\arguments{
  \item{x}{Variable for which to construct the histogram.  Can be a data frame.  If not
      specified with \code{dframe}, that is, no variable specified, then the data frame \code{mydata}
      is assumed.}
  \item{dframe}{Optional data frame that contains the variables of interest, default is \code{mydata}.}
  \item{col.box}{Color of the box.}
  \item{col.pts}{Color of any points that designate outliers. By default this is the same
        color as the box.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{colors}{Sets the color palette.}
  \item{cex.axis}{Scale magnification factor, which by defaults displays the axis values to be smaller than
        the axis labels. Provides the functionality of, and can be replaced by, the standard R \code{cex.axis.}}
  \item{col.axis}{Color of the font used to label the axis values.}
  \item{col.ticks}{Color of the ticks used to label the axis values.}
  \item{horiz}{Orientation of the boxplot. Set \code{FALSE} for vertical.}
  \item{dotplot}{If \code{TRUE}, then place a dot plot (i.e., stripchart) over the box plot.}
  \item{xlab}{Label for the value axis, which defaults to the variable's name.}
  \item{main}{Title of graph.}
  \item{digits.d}{Number of decimal digits displayed in the listing of the summary statistics.}
  \item{\dots}{Other parameter values for graphics as defined processed 
       by \code{\link{boxplot}} and \code{\link{par}}, including \code{ylim} to set the limits of the 
       value axis, \code{lwd} for the line width, \code{cex.lab} for the size of the label, and 
       \code{col.main} for the title, etc.}
}


\details{
Unlike the standard R boxplot function, \code{\link{boxplot}}, the default here is for a horizontal boxplot.  Also, \code{BoxPlot} does not currently process in formula mode, so use the standard R \code{\link{boxplot}} function to process a formula in which a boxplot is displayed for a variable at each level of a second, usually categorical, variable.

To obtain a box plot of each numerical variable in the \code{mydata} data frame, use \code{BoxPlot()}.  Or, for a data frame with a different name, insert the name between the parentheses.

If the variable is in a data frame, the input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the mydata$name notation. If no variable is specified, then all numeric variables in the entire data frame are analyzed and the results written to a pdf file.

Other graphic parameters are available to format the display, such as \code{main} for the title, and other parameters found in \code{\link{boxplot}} and \code{\link{par}}.

A labels data frame named \code{mylabels}, obtained from the \code{lessR} \code{\link{Read}} function, can list the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the corresponding variable label is listed as the title of the resulting plot, unless a specific label is listed with the \code{main} option. The variable label is also listed in the text output, next to the variable name. 

To minimize white space around the boxplot, re-size the graphics window before or after creating the boxplot.

The default background color of \code{col.bg=ghostwhite} provides a very mild cool tone with a slight emphasis on blue. The entire color theme can be specified at the system level with the \code{lessR} function \code{\link{set}} using the \code{colors} option.  Or, use the same option for \code{BoxPlot} to set the color theme just for one box plot. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in the \code{\link{help}} function for \code{\link{set}}. 
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

# vertical boxplot with plum color
BoxPlot(y, horiz=FALSE, col.box="plum")

# boxplot with outliers more strongly highlighted
BoxPlot(y, col.pts="red", xlab="My Variable")


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
BoxPlot(col.box="palegreen1", col.pts="plum")

# Use the subset function to specify a variable list
mysub <- subset(mydata, select=c(X,Y))
BoxPlot(dframe=mysub)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ boxplot }



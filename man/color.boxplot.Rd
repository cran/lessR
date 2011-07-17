\name{color.boxplot}
\alias{color.boxplot}
\alias{color.boxplot.default}
\alias{color.boxplot.data.frame}

\title{Color Boxplot}

\description{
Uses the standard R boxplot function, \code{\link{boxplot}} to display a boxplot in color. Also display the relevant statistics such as the hinges, median and IQR.

If the provided object for which to calculate the box plot is a data frame, then a box plot is calculated for each numeric variable in the data frame and the results written to a pdf file in the current working directory.  The name of this file and its path are specified in the output.
}

\usage{
color.boxplot(x=NULL, \ldots)

\method{color.boxplot}{default}(x, col.box="bisque2", col.point=NULL, 
         horizontal=TRUE, xlab=NULL, digits.d=10, \ldots)

\method{color.boxplot}{data.frame}(x, \ldots) 
}


\arguments{
  \item{x}{Variable for which to construct the boxplot.}
  \item{col.box}{Color of the box.}
  \item{col.point}{Color of any points that designate outliers. By default this is the same
        color as the box.}
  \item{horizontal}{Orientation of the boxplot. Set \code{FALSE} for vertical.}
  \item{xlab}{Label for the value axis, which defaults to the variable's name.}
  \item{digits.d}{Number of decimal digits displayed in the listing of the summary statistics.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{boxplot}} and \code{\link{par}}, including \code{ylim} to set the limits of the value axis, \code{lwd} for the line width, \code{cex.lab} for the size of the label, and \code{col.main} for the title, etc.}
}


\details{
Unlike the standard R boxplot function, \code{\link{boxplot}}, the default here is for a horizontal boxplot.  Also, \code{color.boxplot} does not currently process in formula mode, so use the standard \code{\link{boxplot}} function to process a formula in which a boxplot is displayed for a variable at each level of a second, usually categorical, variable.

Other graphic parameters are available to format the display, such as \code{main} for the title, and other parameters found in \code{\link{boxplot}} and \code{\link{par}}.

To minimize white space around the boxplot, may wish to re-size the graphics window before or after creating the boxplot.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{boxplot}}, \code{\link{par}}.
}

\examples{
# simulate data and get at least one outlier
y <- rnorm(100,50,10)
y[1] <- 90


# -----------------------------
# boxplot for a single variable
# -----------------------------

# standard horizontal boxplot with all defaults
color.boxplot(y)

# vertical boxplot with plum color
color.boxplot(y, horizontal=FALSE, col.box="plum")

# boxplot with outliers more strongly highlighted
color.boxplot(y, col.point="red", xlab="My Variable")


# ----------------------------------
# boxplots across multiple variables
# ----------------------------------

# read data into data frame called mydata
#rad("http://web.pdx.edu/~gerbing/data/employees2.csv")

# boxplots for all numeric variables in data frame called mydata
#color.boxplot()

# boxplots for all numeric variables in data frame called mydata
#  with specified options
#color.boxplot(col.box="palegreen1", col.point="plum", show.values=TRUE)

# Use the subset function to specify a variable list
#color.boxplot(subset(mydata, select=c(Age,HealthPlan)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ boxplot }

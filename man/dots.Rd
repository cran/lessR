\name{dots}
\alias{dots}

\title{Dot or Strip Chart}

\description{
Plots a dot chart, also called a strip chart, and identifies outliers.
}

\usage{
dots(x, dframe=mydata, 
         col.bg="ghostwhite", col.grid="grey85", 
         col.pts="midnightblue", pt.reg=21, pt.out=19, 
         col.out30="firebrick2", col.out15="firebrick4",
         xlab=NULL, main=NULL, text.out=TRUE, new=TRUE, \ldots)
}

\arguments{
  \item{x}{Variable for which to construct the histogram and density plots.}
  \item{dframe}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{col.pts}{Color of regular (non-outlier) points.}
  \item{pt.reg}{Type of regular (non-outlier) point.  See help for \code{\link{points}}
        for more information. Default is 21, a circle with no fill.}
  \item{pt.out}{Type of point for outliers. Default is 19, a filled circle.}
  \item{col.out30}{Color of severe outliers.}
  \item{col.out15}{Color of potential outliers.}
  \item{xlab}{Label for x-axis.}
  \item{main}{Title of graph.}
  \item{text.out}{If \code{TRUE}, then display text output in console.}
  \item{new}{If \code{TRUE}, then add the dots to an existing graph.}
  \item{\dots}{Other parameters, such as from  \code{\link{stripchart}} and \code{\link{par}}.}
}


\details{
Results are based on the standard \code{\link{stripchart}} function. Colors are provided by default and can also be specified.

The input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name without having to invoke the mydata$name notation. Any missing data values are removed and the effecive sample size and number of missing values reported.

A labels data frame named \code{mylabels}, obtained from the \code{\link{rad}} function, can list the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the corresponding variable label is listed as the title of the resulting plot, unless a specific label is listed with the \code{main} option. 
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{stripchart}}. 
}


\examples{
# generate 100 random normal data values
y <- rnorm(100)

# default dots
dots(y)

# dots with diamonds as points
dots(y, pt.reg=23)

# dots with custom colors for outliers
dots(y, pt.reg=23, col.out15="blue", col.out30="red")


# dots with built-in data set
dots(breaks, dframe=warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dots }
\keyword{ strip chart }

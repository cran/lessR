\name{print.out_all}
\alias{print.out_all}

\title{Display All Text Output from a Saved List Object}

\description{
Displays all the results saved as an \code{R} list into an object from a \code{lessR} analysis.  An example of a saved object is \code{r} in \code{r <- reg(Y ~ X)}. The results are displayed at the console or integrated into a \code{knitr} analysis, for example from \code{RStudio}. This function is usually implicitly accessed by the user simply by entering the name of the saved object at the console or in a \code{knitr} file.
}

\usage{
\method{print}{out_all}(x, ...)
}


\arguments{
  \item{x}{The list of components to display.}
  \item{\dots}{Other parameter values_}
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Regression}}
}

\examples{
# read internal data set
d <- rd("Employee", in_lessR=TRUE, quiet=TRUE)
# do the summary statistics
s <- ss_brief(Salary)
# display all the output, print function is implicit
s
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print.out_all}

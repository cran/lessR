\name{print.out_all}
\alias{print.out_all}

\title{Disply All Text Output from a Saved List Object}

\description{
Lists the entire saved results of an analysis from a \code{lessR} function into an object, such as for later display at the console or to be integrated into a \code{knitr} analysis, for example from \code{RStudio}. This function is usually implicitly accessed by the user simply by entering the name of the saved object at the console or in a \code{knitr} file. An example of a saved object is \code{r} in \code{r <- reg(Y ~ X)}.

Now just applies to the \code{lessR} \code{\link{Regression}} function.
}

\usage{
\method{print}{out_all}(x, ...)
}


\arguments{
  \item{x}{The output to display, a character vector or a list of character vectors.}
  \item{\dots}{Other parameter values.}
}

\references{
Gerbing, D. W. (2013). R Data Analysis without Programming, Chapters 9 and 10, NY: Rutledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Regression}}
}

\examples{
# read internal data set
mydata <- rd("Employee", format="lessR", quiet=TRUE)
# do the summary statistics
s <- ss.brief(Salary)
# print all the output, print function is implicit
s
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print.out_all }



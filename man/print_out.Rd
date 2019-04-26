\name{print.out}
\alias{print.out}

\title{Display a Portion of Output from a Saved List Object}

\description{
Displays the portions of saved results of an analysis from a \code{lessR} function into an object, such as for later display at the console or to be integrated into a \code{Rmd} analysis, for example from \code{RStudio}. This function is usually implicitly accessed by the user simply by entering the name of an output piece into the console or in a \code{Rmd} file, such as, such as \code{r$out_coefs} that results from \code{r} in \code{r <- reg(Y ~ X)}.

Now just applies to the \code{lessR} \code{\link{Regression}} function.
}

\usage{
\method{print}{out}(x, ...)
}


\arguments{
  \item{x}{The piece of output to display, a character vector or a list of character vectors.}
  \item{\dots}{Other parameter values_}
}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapters 9 and 10, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Regression}}
}

\examples{
# read internal data set
d <- rd("Employee", in_lessR=TRUE, quiet=TRUE)
# do the summary statistics
s <- ss.brief(Salary)
# print the piece of output, print function is implicit
s$outliers
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print.out }





\name{print.out_piece}
\alias{print.out_piece}

\title{Display a Piece of Output from a Saved List Object}

\description{
Lists the pieces of saved results of an analysis from a \code{lessR} function into an object, such as for later display at the console or to be integrated into a \code{Rmd} analysis, for example from \code{RStudio}. This function is usually implicitly accessed by the user simply by entering the name of an output piece into the console or in a \code{Rmd} file, such as, such as \code{r$out_coefs} that results from \code{r} in \code{r <- reg(Y ~ X)}.

Now just applies to the \code{lessR} \code{\link{Regression}} function.
}

\usage{
\method{print}{out_piece}(x, ...)
}


\arguments{
  \item{x}{The piece of output to display, a character vector or a list of character vectors.}
  \item{\dots}{Other parameter values.}
}

\references{
Gerbing, D. W. (2013). R Data Analysis withprint.out_piece Programming, Chapters 9 and 10, NY: Rprint.out_pieceledge.
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
# print the piece of output, print function is implicit
s$outliers
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print.out_piece }



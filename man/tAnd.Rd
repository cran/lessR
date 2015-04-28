\name{tAnd}
\alias{tAnd}

\title{Text Processing: Insert and}

\description{Inserts the word and into a vector of words, each a separate character string. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
tAnd(x)
}


\arguments{
  \item{x}{The set of character strings for which to insert and.}
}


\details{
Input is a vector of character strings, output is a single character string with and inserted if needed.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
tAnd(c("sky", "land", "mountains"))
}


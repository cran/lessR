\name{xAnd}
\alias{xAnd}

\title{Text Processing: Insert and Into a List}

\description{Inserts the word \code{and} into a vector of words, each a separate character string. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
xAnd(x)
}


\arguments{
  \item{x}{The set of character strings for which to insert \code{and}.}
}


\details{
Input is a vector of character strings, output is a single character string with \code{and} inserted if needed.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
xAnd(c("sky", "land", "mountains"))
}


\name{xNum}
\alias{xNum}

\title{Text Processing: Convert a Number to a Word}

\description{Converts a number to a word. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
  xNum(x) 
}


\arguments{
  \item{x}{The integer to convert.}
}


\details{
Input is an integer, or coerced to integer after rounding. For integers from 0 to 12, output is the single English word. For values larger than 12, or negative, the integer is just converted to character format.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
xNum(5)
}


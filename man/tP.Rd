\name{tP}
\alias{tP}

\title{Print Formatted Numbers}

\description{Prints numbers nicely formatted. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
tP(var, d=NULL)
}


\arguments{
  \item{var}{The variable.}
  \item{d}{The digits.}
}


\details{
Input is numeric, output is formatted text.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
tP(12345678.9, d=2)
}

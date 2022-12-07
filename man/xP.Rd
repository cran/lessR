\name{xP}
\alias{xP}

\title{Text Processing: Print Formatted Numbers}

\description{Prints numbers nicely formatted, with optional units. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
xP(x, d_d=NULL, unit=NULL, semi=FALSE)
}


\arguments{
  \item{x}{The variable.}
  \item{d_d}{The digits.}
  \item{unit}{Unit of measurement for the variable.}
  \item{semi}{Add a semicolon before the unit to add some horizontal spacing
        in math mode.}
}


\details{
Input is numeric, output is formatted text. A special unit is \code{"$"}, which is added to the front of the number instead of as a trailing descriptor.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
xP(12345678.9, d_d=2, unit="$")

xP(12345678.9, d_d=2, unit="lbs")
}

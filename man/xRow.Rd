\name{xRow}
\alias{xRow}

\title{Text Processing: Add the Word Row to Case Labels that Could be Numeric}

\description{For a vector of row names, if the names can be represented as integers the word Row is added to the beginning of each name in the vector. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
xRow(x)
}

\arguments{
  \item{x}{Vector with names for each value.}
}

\details{
Input is a vector of values, output is vector of associated row labels, perhaps with the added word Row.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
# The word Row gets added
v <- c(2, 4, 6)
names(v) <- c("1", "2", "3")
xRow(v)

# The word Row does not get added
v <- c(2, 4, 6)
names(v) <- c("Bill", "Tulane", "Hanna")
xRow(v)
}


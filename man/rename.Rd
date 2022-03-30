\name{rename}
\alias{rename}

\title{Rename a Variable in a Data Frame}

\description{
\code{rename} renames the specified variable in a data frame.
}

\usage{
rename(data, from, to)
}

\arguments{
  \item{data}{Data frame that contains the variable to rename}
  \item{from}{Variable to be renamed.}
  \item{to}{New variable name.}
}

\details{
A simple wrapper for R code:\cr

    names(data)[which(names(data) == from)] <- to

Exists to save the extra complexity and typing when renaming a variable in a data frame.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{recode}}.
}

\examples{
d <- Read("Mach4", quiet=TRUE)

names(d)
d <- rename(d, m03, third)
names(d)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ long-form }

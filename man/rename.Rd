\name{rename}
\alias{rename}

\title{Rename One or More Variables in a Data Frame}

\description{
\code{rename} renames a single variable or a vector of variables in a data frame.
}

\usage{
rename(data, from, to)
}

\arguments{
  \item{data}{Data frame that contains the relevant variables.}
  \item{from}{One or more variables to rename.}
  \item{to}{Corresponding list of new variable names.}
}

\details{
Assign the result to the data frame of interest, which can be the same data frame that contains the variables to rename.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{recode}}.
}

\examples{

d <- Read("Mach4", quiet=TRUE)
names(d)

# single name change
d <- rename(d, m03, third)
names(d)

# vector of name changes
d <- rename(d, c(m01, m19), c(first, nineteen))
names(d)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ long-form }

\name{see}
\alias{see}

\title{View the Upper and Left Corners of a Data Frame}

\description{
Useful for large data frame. View the top-left corner of the specified data frame and the bottom-right corner of the data frame.
}

\usage{
see(data, n_row=min(nrow(data), 5), n_col=min(ncol(data), 8))
}

\arguments{
  \item{data}{Name of the data frame to view.}
  \item{n_row}{Number of rows to view.}
  \item{n_col}{Number of columns to view.}
}

\details{
For the specified number of rows and columns, just view the subset of the data frame in terms of the top-left and the bottom-right.
}

\value{
The subset data frame.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Extract}}.
}

\examples{
d <- Read("Employee", quiet=TRUE)

# view the default top-left and bottom-right four rows and eight columns
see(d)

# view the top-left two rows and bottom-right four columns
see(d, n_row=2, n_col=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{see}


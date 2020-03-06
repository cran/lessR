\name{ir}
\alias{ir}

\title{General Variable List for Selecting Rows with base R Extract}

\description{

In the Extract function, do not append data frame name and \$ to variable names in \code{\link{Extract}} for expressions to subset a data frame by rows.

}

\usage{
ir(x)

}

\arguments{
  \item{x}{Logical expression to subset rows.}
}

\details{
Eliminates the need to prepend the data frame name and a \$ to each variable name in the specified logical expression to select rows. 
}

\value{
The row names of the rows of data that satisfy the specified logical conditions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{ic}}, \code{\link{Extract}}.
}

\examples{
d <- Read("Employee", quiet=TRUE)

# no data frame name attached to variable names
#   as variables assumed in the data frame
d[ir(Gender=="M" & Post>90), ]

# with ic for the columns
d[ir(Gender=="M" & Post>90), ic(Years:Salary, Post)]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ir}



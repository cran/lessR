\name{ic}
\alias{ic}

\title{General Variable List for Selecting Columns with base R Extract}

\description{

Replace \code{\link{c}} with \code{ic} in \code{\link{Extract}} for expressions to subset a data frame by columns, either by including or excluding the specified variables. Without \code{ic}, variable names must be quoted and no variable ranges are permissible.

}

\usage{
ic(x, \ldots)

}

\arguments{
  \item{x}{First expression of variables.}
  \item{\dots}{Additional expressions of variables.}
}

\details{
\code{ic} replaces \code{\link{c}} when using \code{\link{Extract}} to specify one or more variables in a subset. Variable names do not need to be quoted. General expressions of lists of variables can be specified, including cell ranges.

If only a single column is retained, need to add \code{drop=FALSE} to the Extract subsetting function call.

If the specified variables are to be deleted, include a "-" anywhere in the list of variables, with a comma after if listed first and a comma before if listed anywhere else.
}

\value{
The indices of the columns to be retained in the subset data frame.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{ir}}, \code{\link{Extract}}.
}

\examples{
d <- Read("Employee", quiet=TRUE)

# include first three rows and only the specified variables
d[1:3, ic(Years:Salary, Post)]

#  include first three rows and delete the specified variables
d[1:3, ic(Years:Salary, Post, "-")]

# with ir for the rows
d[ir(Gender=="M" & Post>90), ic(Years:Salary, Post)]

# because of the default for the base R Extract function,
# if only one variable retained,
# then add drop=FALSE to retain a data frame
d[1:3, ic(Salary), drop=FALSE]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ic}



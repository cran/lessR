\name{.}
\alias{.}

\title{Function . for Selecting Rows/Columns with base R Extract}

\description{

Using the base R \code{\link{Extract}} function, with the unobtrusive function name, \code{.}, express a subsetting operation as\cr
d[.(rows), .(cols)]\cr
for a less annoying experience. With \code{.} to express a logical criterion to select rows, do not append the data frame name and \code{$} to variable names in  expressions as otherwise required by  \code{\link{Extract}}. Can also do a random selection of rows. For columns, no need to quote variable names, can include variable ranges defined by a colon, :, and add \code{-} to exclude designated columns. Also does not list rows missing data when not requested as does \code{\link{Extract}}.
}

\usage{
.(x, \ldots)

}

\arguments{
  \item{x}{Logical expression to subset rows or columns.}
  \item{\dots}{Allows multiple expressions when selecting columns.}`
}

\details{
Eliminates the need to prepend the data frame name and a \$ to each variable name in the specified logical expression to select rows. For columns, no quoting variables, allow variable ranges. 

Can create a character string called \code{rows} that expresses the logic of row selection. Can create a character string called \code{cols} that expresses the logic of column (variable) selection. To negate the \code{rows} expression, \code{.(!rows)}. Use \code{-.(cols)} to exclude designated variables. 

Select a random selection of rows with the containing function \code{random(n)}, where \code{n} is the specified number of random rows to select from the full data frame.
}

\value{
The row or columns names of the rows of data or columns of data that satisfy the specified logical conditions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Extract}} \code{\link{subset}}.
}

\examples{
# see vignette

d <- Read("Employee", quiet=TRUE)

# no data frame name attached to variable names
#   as variables assumed in the data frame
d[.(Gender=="M" & Post>90), ]

# include first three rows and only the specified variables
# variable range permitted
d[1:3, .(Years:Salary, Post)]

#  include first three rows and delete the specified variables
d[1:3, -.(Years:Salary, Post)]

# select rows and columns
d[.(Gender=="M" & Post>90), .(Years:Salary, Post)]

# because of the default for the base R Extract function [ ],
# if only one variable retained,
# then add drop=FALSE to retain the result as a data frame
d[1:3, .(Salary), drop=FALSE]

# define character string arguments
cols <- "Gender:Salary, Post"
rows <- "Gender=='M' & Post>93"
d[.(rows), .(cols)]
# negate
d[.(!rows), -.(cols)]

# random selection of 4 rows, retain all variables
d[.(random(4)), ]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{.}



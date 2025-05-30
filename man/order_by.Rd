\name{order_by}
\alias{order_by}

\title{order_by the Rows of a Data Frame}

\description{
Sorts the values of a data frame according to the values of one or more variables contained in the data frame, or the row names.  Variable types include numeric and factor variables.  Factors are sorted by the ordering of their values, which, by default is alphabetical. Sorting by row names is also possible.
}

\usage{
order_by(data=d, by, direction=NULL, quiet=getOption("quiet"), \dots)
}

\arguments{
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{d} by default.}
  \item{by}{One or more variables to be sorted, or just the character string 
       \code{row.names} or \code{random}.}
  \item{direction}{Default is ascending for all variables listed in \code{by}.
       Or, specify a list of \code{"+"} for ascending and \code{"-"} for
       descending, one for each variable to be sorted.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{\dots}{Other parameter values.}
}

\details{
\code{order_by} sorts the rows of a data frame and lists the first five rows of the sorted data frame. Specify the values upon which to base the sort with the required \code{by} parameter. If not all sorted variables are sorted in ascending order, then also specify a sequence of \code{"+"} for ascending and \code{"-"} for descending, respectively, one for each variable to be sorted. If \code{row.names} or \code{random} is specified, then no other variables can be specified.

A list of consecutive variables can be specified using the colon notation, such as Years:Salary  To specify a list of multiple variables, or \code{"+"} and \code{"-"} signs, or sets of variables, separate each set of variables or each sign by a comma, then invoke the R combine or \code{\link{c}} function. For example, if three variables are to be sorted, the first two ascending and the last descending, then specify, \code{direction=c("+","+","-")}.

\code{order_by} is based on the standard R function \code{\link{order}}, though  the \code{order_by} function allows for the sorting of factors, whereas \code{\link{order}} does not.
}

\value{
The sorted data frame is returned, usually assigned the name of \code{d} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}
\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{order}}.
}

\examples{
# construct data frame
d <- read.table(text="Severity Description
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# sort the data frame called d according to Severity
#   in ascending order
d <- order_by(d, Severity)

# sort Description in descending order, sort Severity within
#  each level of Description in ascending order
d <- order_by(d, c(Description, Severity), direction=c("-", "+"))  

# sort by row names in ascending order
d <- order_by(d, row.names)

# randomly re-shuffle the rows of data
d <- order_by(d, random)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sort }

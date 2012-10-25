\name{Sort}
\alias{Sort}
\alias{srt}

\title{Sort the Rows of a Data Frame}

\description{
Abbreviation: \code{srt}

Sorts the values of a data frame according to the values of one or more variables contained in the data frame, or the row names.  Variable types include numeric and factor variables.  Factors are sorted by the ordering of their values, which, by default is alphabetical. Sorting by row names is also possible.
}

\usage{
Sort(by, direction=NULL, brief=FALSE, keep=TRUE, dframe=mydata, ...)

srt(\ldots)
}

\arguments{
  \item{by}{One or more variables to be sorted, or just the character string 
       \code{row.names} or \code{random}.}
  \item{direction}{Default is ascending for all variables listed in \code{by}.
       Or, specify a list of \code{"+"} for ascending and \code{"-"} for
       descending, one for each variable to be sorted.}
  \item{brief}{If \code{TRUE}, then no text output is provided.}
  \item{keep}{If \code{TRUE}, the default, then the data frame with the transformed
       variable(s) replaces the input data frame.}
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{\dots}{Parameter values.}
}

\details{
\code{Sort} sorts the rows of a data frame, by default \code{mydata}, and lists the first five rows of the sorted data frame. Specify the values upon which to base the sort with the required \code{by} parameter. If not all variables to be sorted are not to be sorted in ascending order, then also specify a sequence of \code{"+"} for ascending and \code{"-"} for descending, respectively, one for each variable to be sorted. If \code{row.names} or \code{random} is specified, then no other variables can be specified.

Given the focus on a single data frame within the \code{lessR} system, the input data frame has a default value of the standard \code{mydata}, and by default writes the revised data frame over the input data frame, without the need for an assignment statement. Set \code{keep=FALSE} to not have the output data frame overwrite the input data frame. To save the sorted data in this situation, explicitly assign the result of the \code{Sort}, the sorted data frame, to the specified data frame, as shown in the examples.

A list of consecutive variables can be specified using the colon notation, such as Years:Salary  To specify a list of multiple variables, or \code{"+"} and \code{"-"} signs, or sets of variables, separate each set of variables or each sign by a comma, then invoke the R combine or \code{\link{c}} function. For example, if three variables are to be sorted, the first two ascending and the last descending, then specify, \code{direction=c("+","+","-")}.

\code{Sort} is based on the standard R function \code{\link{order}}, though  the \code{Sort} function allows for the sorting of factors, whereas \code{\link{order}} does not.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{order}}.
}

\examples{
# construct data frame
mydata <- read.table(text="Severity Description
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# sort the data frame called mydata according to Severity
#   in ascending order
Sort(Severity)

# abbreviated form, replace original with sorted
srt(Severity)

# sort Description in descending order, sort Severity within
#  each level of Description in ascending order
Sort(c(Description, Severity), direction=c("-", "+"))  

# data in a different data frame than mydata
data(dataEmployee)
Sort(Gender, dframe=dataEmployee)

# sort by row names in ascending order
Read(lessR.data="Employee")
Sort(row.names)

# randomly re-shuffle the rows of data
Read(lessR.data="Employee")
Sort(random)

# save sorted data in data frame mysort
Read(lessR.data="Employee")
mysort <- Sort(row.names, keep=FALSE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sort }

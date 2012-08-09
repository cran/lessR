\name{Sort}
\alias{Sort}
\alias{srt}

\title{Sort the Rows of a Data Frame}

\description{
Abbreviation: \code{srt}

Sorts the values of a data frame according to the values of one or more variables contained in the data frame, or the row names.  Variables types include numeric and factor variables.  Factors are sorted by the ordering of their values, which, by default is alphabetical.
}

\usage{
Sort(by, direction=NULL, dframe=mydata, ...)

srt(\ldots)
}

\arguments{
  \item{by}{One or more variables to be sorted, or the character string 
       \code{"row.names"}.}
  \item{direction}{Default is ascending for all variables listed in \code{by}.
       Or, specifiy a list of \code{"+"} for ascending and \code{"-"} for
       descending, one for each variable to be sorted.}
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{\dots}{Parameter values.}
}

\details{
Specify the values to be recoded with the required \code{by} parameter. If not all variables are to be sorted within ascending order, then also specify a sequence of
\code{"+"} for ascending and \code{"-"} for descending, one for each variable to be sorted.

A set of variables can be listed using the colon notation, such as Years:Salary  To specify a list of multiple variables or \code{"+"} and \code{"-"} signs, or sets of variables, separate each set of variables or each sign by a comma, then invoke the R combine or \code{\link{c}} function. For example, if three variables are to be sorted, the first two ascending and the last descending then specfiy, \code{direction=c("+","+","-")}.

\code{Sort} is based on the standard R function \code{\link{order}}, though this function allows for the sorting of factors, whereas \code{\link{order}} does not.
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

# recode Severity into a new variable called SevereNew
Sort(Severity)

# abbreviated form,  replace original with recoded
# another option, the sequence function, to generate list of values
srt(Severity)

# sort Description in descending order, sort Severity within
#  each level of Description in ascending order
# Description is a factor, explicitly set the order of the labels
mydata <- transform(mydata, 
  Description=factor(Description, labels=c("Mild", "Moderate", "Severe"))
)
Sort(c(Description, Severity), direction=c("-", "+"))  

# data in a different data frame than mydata
data(datEmployee)
Sort(Gender, dframe=datEmployee)

# sort by row names in ascending order
data(datEmployee)
row.names(datEmployee) <- datEmployee$Name
datEmployee <- datEmployee[, -1]
Sort("row.names", dframe=datEmployee)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ recode }







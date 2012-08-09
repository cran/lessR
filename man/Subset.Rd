\name{Subset}
\alias{Subset}
\alias{subs}

\title{Subset the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{subs}

A copy of the standard R \code{\link{subset}} function except that the modified data frame is by default written to the input data frame, which is then saved automatically.  The intent is to provide a function that is easier to use and suffices when the focus is on a single data frame. Also, output is provided that provides feedback and guidance regarding the specified subset operations.
}

\usage{
Subset(rows, columns, brief=FALSE,
       save.dframe=TRUE, dframe=mydata, \ldots)

subs(\ldots)
}

\arguments{
  \item{rows}{Specify the rows, i.e., observations, to be included or deleted.}
  \item{columns}{Specify the columns, i.e., variables, to be included or
        deleted.}
  \item{brief}{If \code{TRUE}, then no text output is provided.}
  \item{save.dframe}{If \code{TRUE}, then the output data frame replaces the 
       input data frame.}
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{\dots}{The list of variables, each of the form, \code{variable = equation}. Each \code{variable} can be the name of an existing variable in the data frame or a newly created variable.}
}

\details{
The standard \code{\link{subset}} function creates a subset based on one or more variables in the input data frame, which must be specified, and this is saved in the output data frame specified with an assignment statement.  Given the focus on a single data frame within the \code{lessR} system, the input data frame has a default value of the standard \code{mydata}, and by default writes the revised data frame over the input data frame, without the need for an assignment statement. However, the behavior of the standard  \code{\link{subset}} function can be mimicked by setting \code{save.dframerame=FALSE}.

Also guidance and feedback regarding the subsets are provided by default. The first six lines of the input data frame are listed before the subset operation, followed by the first six lines of the output data frame.

To indicate retaining an observation, specify at least one variable name and the value of the variable for which to retain the corresponding observations, using two equal signs to indicate the logical equality.  If no rows are specified, all rows are retained.

To indicate retaining a variable, specify at least one variable name. To specify multiple variables, separate adjacent variables by a comma, and enclose the list within the standard R combine function, \code{\link{c}}.  A single variable may be replaced by a range of consecutive variables indicated by a colon, which separates the first and last variables of the range.  To delete a variable or variables, put a minus sign, -, in front of the \code{c}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{subset}}, \code{\link{factor}}.
}

\examples{
# construct data frame
mydata <- read.table(text="Severity Description
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# only include those with Moderate
Subset(rows=Description=="Moderate")

# only include those with Moderate
# use abbreviation and do not need the rows= for the first argument
subs(Description=="Moderate")

# only retain females and Years and Salary as variables in datEmployee
data(datEmployee)
Subset(rows=Gender=="F", columns=c(Years, Salary), dframe=datEmployee)

# delete Years and Salary from datEmployee
data(datEmployee)
Subset(columns=-c(Years, Salary), dframe=datEmployee)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ subset }









\name{Subset}
\alias{Subset}
\alias{subs}
\alias{locate}

\title{Subset the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{subs}, \code{locate}

Based directly on the standard R \code{\link{subset}} function except that the modified data frame is by default written to the input data frame, which is then saved automatically.  The intent is to provide a function that is easier to use and suffices when the focus is on a single data frame. Also, output is provided that provides feedback and guidance regarding the specified subset operations, an option is provided that locates rows of data without creating a new data frame, and rows of data may be randomly extracted with a hold out validation sample created.
}

\usage{
Subset(rows, columns, brief=FALSE, keep=TRUE,
       dframe=mydata, validate=NULL, \ldots)

subs(\ldots)

locate(\ldots, keep=FALSE)
}

\arguments{
  \item{rows}{Specify the rows, i.e., observations, to be included or deleted,
       such as with a logical expression. If an integer or proportion, specifies
       number of rows to data to randomly extract.}
  \item{columns}{Specify the columns, i.e., variables, to be included or
        deleted.}
  \item{brief}{If \code{TRUE}, then no text output is provided.}
  \item{keep}{If \code{TRUE}, the default, then the output data frame replaces the 
       input data frame. If \code{FALSE}, then just locate the specified data, perhaps
       assigning the result to a new data frame.}
  \item{dframe}{The name of the data frame from which to create the subset, which
       is \code{mydata} by default.}
  \item{validate}{Create a hold out sample for validation if rows is a proportion
       or an integer to indicate random extraction of rows of data. Default is
       \code{TRUE} if \code{rows} is a logical condition, and \code{TRUE} if
       numeric.}
  \item{\dots}{The list of variables, each of the form, \code{variable = equation}.
       Each \code{variable} can be the name of an existing variable in the data frame
       or a newly created variable.}
}

\details{
\code{Subset} creates a subset based on one or more rows of data and one or more variables in the input data frame, and lists the first five rows of the revised data frame. Given the focus on a single data frame within the \code{lessR} system, the input data frame has a default value of the standard \code{mydata}, and by default writes the revised data frame over the input data frame, without the need for an assignment statement. 

The argument \code{rows} can be a logical expression based on values of the variables, or it can be an integer or proportion to indicate random extraction of rows. An integer specifies the number of rows to retain, and a proportion specifies the corresponding proportion, which is then rounded to an integer. If the default  \code{validate=TRUE} is retained, then a hold out data frame is also created.

In contrast, the standard R \code{\link{subset}} function, which has no default input data frame, requires an assignment statement to a data frame to save the subset.  However, the behavior of the standard  \code{\link{subset}} function can be mimicked by setting \code{keep=FALSE}, in which case an assignment statement would be used to specify the output data frame if the output was to be saved. This is equivalent to using the abbreviation \code{locate}.

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

# only include those with a value of Moderate for Description
Subset(rows=Description=="Moderate")

# only include those with a value of Moderate for Description
# use abbreviation and do not need the rows= for the first argument
subs(Description=="Moderate")

# locate, that is, display, the second row of data
# note that mydata must be explicitly specified
locate(row.names(mydata)==2)

# only retain females and Years and Salary as variables in datEmployee
data(dataEmployee)
Subset(rows=Gender=="F", columns=c(Years, Salary), dframe=dataEmployee)

# delete Years and Salary from datEmployee
Read(lessR.data="Employee")
Subset(columns=-c(Years, Salary))

# locate only women with more than 10 years employment
# save in a new data frame, women
Read(lessR.data="Employee")
women <- locate(Gender=="F" & Years>10)

# locate all rows for females, display at console and save into mynewdata
Read(lessR.data="Employee")
mynewdata <- locate(Gender=="F")

# locate row by its row.name, here the employee's name
Read(lessR.data="Employee")
locate(row.names(mydata)=="Fulton, Scott")

# randomly extract 60\% of the data and create a hold-out sample
Read(lessR.data="Employee")
Subset(.6)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ subset }



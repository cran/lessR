\name{pivot}
\alias{pivot}

\title{Create a Pivot Table}

\description{

Summarize a numerical variable (\code{value}) over one or more categorical variables (\code{for_eah}), with the sample size for each cell and the possibility to subset rows. Based on the base~R function \code{\link{aggregate}}.

}

\usage{
pivot(data, compute, value, by, filter=NULL, table_2d=FALSE)
}

\arguments{
  \item{data}{Data frame that contains the variables.}
  \item{compute}{The statistic to aggregate over the combinations of the
        values of the categorical variables.}
  \item{value}{Numerical response variable that is aggregated, i.e.,
       summarized across the groups.}
  \item{by}{Categorical variables that define the groups (cells).}
  \item{filter}{A logical expression that specifies a subset of rows of
       the data frame to analyze.}
  \item{table_2d}{If two \code{by} variables, and \code{TRUE}, then display
       results as a two-dimensional table instead of a linear list.}
}

\details{
\code{pivot} uses base R \code{\link{aggregate}} to generate a pivot table (Excel terminology). Express the categorical variables over which to pivot as a vector with the \code{\link{c}} function.

\code{pivot} provides two additional features than \code{\link{aggregate}} provides. First is the inclusion of the sample size for each cell. Second is that the data parameter is listed first in the parameter list, which facilitates the use of the pipe operator from the \code{magrittr} package. Also, there is a different interface as the \code{by} variables are specified as a vector.

Presently, only a single variable for the aggregation, the \code{value}. Variable ranges in the specification of \code{by} as not needed in general. Only a small number of grouping variables generally define the cells for the aggregation.

The following table lists available single summary statistics. The list is not necessarily exhaustive.

\tabular{ll}{
Statistic \tab Meaning\cr
----------- \tab --------------------------------\cr
\code{sum} \tab sum\cr
\code{mean} \tab arithmetic mean\cr
\code{median} \tab median\cr
\code{min} \tab minimum\cr
\code{max} \tab maximum\cr
\code{sd} \tab standard deviation\cr
\code{var} \tab variance\cr
\code{IQR} \tab inter-quartile range\cr
\code{mad} \tab mean absolute deviation\cr
----------- \tab --------------------------------\cr
}

Some statistical functions are available that return multiple values.

\tabular{ll}{
Statistic \tab Meaning\cr
----------- \tab --------------------------------\cr
\code{range} \tab minimum, maximum\cr
\code{quantile} \tab range + quartiles\cr
\code{summary} \tab quantile + mean\cr
----------- \tab --------------------------------\cr
}

These three functions output their values as an R matrix, which then replaces the values of the variable that is aggregated in the resulting output data frame.

}

\value{
Returns a data frame of the aggregated values, unless for two \code{by} variables and \code{table_2d} is \code{TRUE}, when a table is returned.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{aggregate}}.
}

\examples{
d <- Read("Employee", quiet=TRUE)

# calculate the mean salary for each combination of Dept and Gender
# parameter values by position
pivot(d, mean, Salary, c(Dept, Gender))

# parameter values by name
pivot(data=d, compute=mean, value=Salary, by=c(Dept, Gender))

# filter the rows, men only
pivot(d, mean, Salary, c(Dept, Gender), filter=(Gender=="M"))

# output as a table
pivot(d, mean, Salary, c(Dept, Gender), table_2d=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{pivot}



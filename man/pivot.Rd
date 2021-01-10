\name{pivot}
\alias{pivot}

\title{Create a Pivot (Summary) Table}

\description{

Summarize, that is, aggregate, a numerical variable (\code{value}) over one or more categorical variables (\code{by}). Display the sample size for each cell and provide the possibility to subset rows. Based on the base R function \code{\link{aggregate}}.

}

\usage{
pivot(data, compute, value, rows_by, cols_by=NULL,
         na_value=FALSE, na_by=FALSE, na_remove=TRUE,
         factors=FALSE, digits_d=NULL)
}

\arguments{
  \item{data}{Data frame that contains the variables.}
  \item{compute}{The statistic, defined as a function, to aggregate
       over the combinations of the values of the categorical variables.}
  \item{value}{One or more numerical response variables that are aggregated,
       i.e., summarized across the groups.}
  \item{rows_by}{Categorical variables that define the groups (cells) listed
       in the rows of the output long-form data frame, available to input
       into other data analysis routines. Not used if function to compute
       is \code{tabulate}.}
  \item{cols_by}{Up to two categorical variables that define the groups
       displayed as columns in a two dimensional table.} 
  \item{na_value}{If \code{TRUE}, then show cells with the aggregated variable
       that are missing.}
  \item{na_by}{If \code{TRUE}, then show levels of the grouping variables
       that are missing.}
  \item{na_remove}{Sets base R parameter \code{na.rm}, which, if \code{TRUE},
       removes missing values from the \code{value} variables. Otherwise,
       the aggregation for a cell with missing data returns \code{NA}.}
  \item{factors}{For \code{rows_by} variables of type \code{Date}, 
        \code{character}, and \code{integer}, the variable type is retained
        in the summary table. If \code{TRUE}, then the \code{rows_by} variables
        become factors.}
  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}
}

\details{
\code{pivot} uses base R \code{\link{aggregate}} to generate a pivot table (Excel terminology). Express the categorical variables over which to pivot as a vector with the \code{\link{c}} function.

\code{pivot} provides two additional features than \code{\link{aggregate}} provides. First is a complete missing data analysis. If there is no missing data for the numerical variables that are aggregated, then the cell sizes are included with the aggregated data. If there is such missing data, then the amount of available data is displayed for all values to be aggregated for each cell.

The second is that the data parameter is listed first in the parameter list, which facilitates the use of the pipe operator from the \code{magrittr} package. Also, there is a different interface as the \code{by} variables are specified as a vector.

Variable ranges in the specification of \code{by} are not needed in general. Only a small number of grouping variables generally define the cells for the aggregation.

The following table lists available single summary statistics. The list is not necessarily exhaustive.

The unique function is \code{tabulate}, which does not refer to the base R function of that name, but an indicator to just provide cell counts, no other statistics, generalizing the \code{pivot} function to cover more cases. Accordingly, the \code{value} and \code{by} parameters are redundant, so the \code{by} parameter is not needed.

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
\cr
\code{tabulate} \tab count of each cell only\cr
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
library(knitr)  # for kable() called from pivot()
d <- Read("Employee", quiet=TRUE)

# parameter values by name
pivot(data=d, compute=mean, value=Salary, rows_by=c(Dept, Gender))

# calculate mean of Years and Salary for each combination of Dept and Gender
# parameter values by position
pivot(d, mean, c(Years, Salary), c(Dept, Gender))

# output as a 2-d table
pivot(d, mean, Salary, Dept, Gender)

# tabulate only
pivot(d, tabulate, c(Dept, Gender))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{pivot}



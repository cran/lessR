\name{pivot}
\alias{pivot}

\title{Create a Pivot (Summary) Table}

\description{

Compute one or more designated descriptive statistics (\code{compute} over one or more numerical variables (\code{variable}) either for all the data or aggregated over one or more categorical variables (\code{by}). Because the output is a two-dimensional table, select any two of the three possibilities: Multiple compute functions for the descriptive statistics, multiple continuous variables over which to compute, and multiple categorical variables by which to define groups for aggregation. Displays the sample size for each group. Uses the base R function \code{\link{aggregate}} for which to perform the aggregation.

}

\usage{
pivot(data, compute, variable, by=NULL, by_cols=NULL, rows=NULL,
         na_by_show=TRUE, na_remove=TRUE, sort=NULL,
         out_names=NULL, factors=FALSE, q_num=4,
         table_prop=c("none", "all", "row", "col"),
         show_n=TRUE, digits_d=3, quiet=getOption("quiet"))
}

\arguments{
  \item{data}{Data frame that contains the variables.}
  \item{compute}{One or more statistics, defined as one or more functions,
       to aggregate over the combinations of the values of the categorical
       variables.}
  \item{variable}{One or more numeric response variables for which to
       \code{compute} the specified statistics, perhaps aggregated,
       i.e., summarized across the groups.}
  \item{by}{Categorical variables that define the groups (cells) listed
       in the rows of the output long-form data frame, available to input
       into other data analysis routines. Not used if function to compute
       is \code{tabulate}. Ignore to compute over the variables for all the
       data, e.g., the grand mean.}
  \item{by_cols}{Up to two categorical variables that define the groups
       displayed as columns in a two dimensional table.} 
  \item{rows}{Subset, i.e., filter, rows of the input data frame for analysis.}\cr

  \item{na_by_show}{If \code{TRUE}, show missing levels of the grouping
       variables with \code{n} set to 0.}
  \item{na_remove}{Sets base R parameter \code{na.rm}, which, if \code{TRUE},
       removes missing values from the \code{value} variables and then reports
       how many values were missing. Otherwise, the aggregation for a cell
       with any missing data returns \code{NA}.}
  \item{sort}{Set to \code{"+"} for an ascending sort or \code{"-"} for
       a descending sort according to the last variable in the output data
       frame.}
  \item{out_names}{Custom names for the aggregated variables. If more than one,
        list in the same order as specified in \code{variable}.}
  \item{factors}{For \code{by} variables of type \code{Date}, 
        \code{character}, and \code{integer}, the variable type is retained
        in the summary table. If \code{TRUE}, then the \code{by} variables
        become factors.}
  \item{q_num}{For the computation of quantiles, number of intervals. Default
        value of 4 provides quartiles.}
  \item{table_prop}{Applies to a created \code{table} for the value of
        \code{compute}. Default value of \code{"none"} leaves frequencies.
        Value of \code{"all"} converts to cell proportions based on the
        grand total. Values of \code{"row"} and \code{"col"} provide
        proportions based on row and column sums.}
  \item{show_n}{By default, show the sample size and number missing for each
        computed summary statistic. If \code{FALSE}, delete all variables
        from the output data frame that begin with \code{n_} or \code{na_}.}\cr

  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
}

\details{
\code{pivot} uses base R \code{\link{aggregate}} to generate a pivot table (Excel terminology). Express multiple categorical variables over which to pivot as a vector with the \code{\link{c}} function.

\code{pivot} provides two additional features than \code{\link{aggregate}} provides. First is a complete missing data analysis. If there is no missing data for the numerical variables that are aggregated, then the cell sizes are included with the aggregated data. If there is such missing data, then the amount of available data is displayed for all values to be aggregated for each cell.

The second is that the data parameter is listed first in the parameter list, which facilitates the use of the pipe operator from the \code{magrittr} package. Also, there is a different interface as the \code{by} variables are specified as a vector.

Variable ranges in the specification of \code{by} are not needed in general. Only a small number of grouping variables generally define the cells for the aggregation.

The following table lists available single summary statistics. The list is not necessarily exhaustive.

The unique function is \code{tabulate}, which does not refer to the base R function of that name, but an indicator to just provide cell counts, no other statistics, generalizing the \code{pivot} function to cover more cases. Accordingly, the \code{vaiable} and \code{by} parameters are redundant, so the \code{by} parameter is not needed.

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
\code{skew} \tab skew\cr
\code{kurtosis} \tab kurtosis\cr
\code{IQR} \tab inter-quartile range\cr
\code{mad} \tab mean absolute deviation\cr
\cr
\code{tabulate} \tab count of each cell only\cr
----------- \tab --------------------------------\cr
}

The functions \code{skew()} and \code{kurtosis()} are provided by this package as they have no counterparts in base R. All other functions are from base R.

The \code{quantile} and \code{table} statistical function returns multiple values.

\tabular{ll}{
Statistic \tab Meaning\cr
----------- \tab --------------------------------\cr
\code{quantile} \tab min, quartiles, max\cr
\code{table}    \tab frequencies or proportions\cr
----------- \tab --------------------------------\cr
}
The \code{table} computation applies to an aggregated variable that consists of discrete categories, such as the numbers 1 through 5 for responses to a 5-pt Likert scale. The result is a table of frequencies or proportions, referred to for two or more variables as either a contingency table, a cross-tabulation table, or a joint frequency distribution. Only the \code{table} computation applies to non-numeric as well as numeric variables, though only meaningful if the aggregated variable consists of a relatively small set of discrete values.

The default quantiles for \code{quantile} are quartiles. Specify a custom number of quantiles with the \code{q_num} parameter, which has the default value of 4 for quartiles.


}

\value{
Returns a data frame of the aggregated values, unless for two \code{by} variables and \code{table_2d} is \code{TRUE}, when a table is returned. 

The count of the number of elements in each group is provided as the variable \code{n}. If a combination of \code{by} variable levels that defines a group is empty, the \code{n} is set to 0 with the values of the \code{variable} set to \code{NA}.

The number of missing elements of the \code{value} variable is provided as the variable \code{miss}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{aggregate}}.
}

\examples{
library(knitr)  # for kable() called from pivot()
d <- Read("Employee", quiet=TRUE)

# parameter values by name
pivot(data=d, compute=mean, variable=Salary, by=c(Dept, Gender))

# visualize the aggregation
a <- pivot(d, mean, Salary, c(Dept, Gender))
BarChart(Dept, Salary_mn, by=Gender, data=a)

# calculate mean of Years and Salary for each combination of Dept and Gender
# parameter values by position
pivot(d, mean, c(Years, Salary), c(Dept, Gender))

# output as a 2-d table
pivot(d, mean, Salary, Dept, Gender)

# tabulate group counts only
pivot(d, tabulate, c(Dept, Gender))

# multiple functions for which to aggregate
pivot(d, c(mean,sd,median,IQR), Years, c(Gender,Dept), digits_d=2)

# A variety of statistics computed for several variables over the
#  entire data set without aggregation
pivot(d, c(mean,sd,skew,kurtosis), c(Years,Salary,Pre,Post), digits_d=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{pivot}



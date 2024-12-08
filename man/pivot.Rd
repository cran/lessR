\name{pivot}
\alias{pivot}

\title{Create a Pivot (Summary) Table}

\description{
Compute one or more designated descriptive statistics (\code{compute} over one or more numerical variables (\code{variable}) either for all the data or aggregated over one or more categorical variables (\code{by}). Because the output is a two-dimensional table, select any two of the three possibilities: Multiple compute functions for the descriptive statistics, multiple continuous variables over which to compute, and multiple categorical variables by which to define groups for aggregation. Displays the sample size for each group. Uses the base R function \code{\link{aggregate}} for which to perform the aggregation.
}

\usage{
pivot(data, compute, variable, by=NULL, by_cols=NULL, filter=NULL,
         show_n=TRUE, na_by_show=TRUE, na_remove=TRUE, na_group_show=TRUE,
         out_names=NULL, sort=NULL, sort_var=NULL,  
         table_prop=c("none", "all", "row", "col"), table_long=FALSE,
         factors=TRUE, q_num=4, digits_d=NULL, quiet=getOption("quiet"))
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
       into other data analysis routines. Ignore to compute over the variables
       for all the data, e.g., the grand mean.}
  \item{by_cols}{Up to two categorical variables that define the groups
       displayed as columns in a two dimensional table.} 
  \item{filter}{Subset, i.e., filter, rows of the input data frame for
        analysis.}\cr

  \item{show_n}{By default, display the sample size and number missing for each
        computed summary statistic. If \code{FALSE}, delete all variables
        from the output data frame that end with \code{n_} or \code{na_}.}
  \item{na_by_show}{If \code{TRUE},
       the default, if all values of `variable` are missing for a group
       so that the entire level of the `by` variables is missing, show those
       missing cells with a reported value of computed variable \code{n} as 0.
        Otherwise delete the row from the output.}
  \item{na_remove}{Sets base R parameter \code{na.rm}. If \code{TRUE},
       the default, removes missing values from the \code{variable}(s),
       then reports how many values were missing. Otherwise, the 
       aggregation statistic for a cell with any missing data returns \code{NA}.}
  \item{na_group_show}{If \code{TRUE}, the default, display \code{<NA>} for missing
       data of a grouping variable as a level for that variable. Otherwise, do
       not treat a missing value of a group as a level for which to aggregate.}
  \item{out_names}{Custom names for the aggregated variables. If more than one,
        list in the same order as specified in \code{variable}. Does not apply
        to the \code{table} option where the column names are the levels of
        the \code{by} variable(s).}\cr

  \item{sort}{Set to \code{"+"} for an ascending sort or \code{"-"} for
       a descending sort according to the last variable in the output data
       frame.}
  \item{sort_var}{Either the name of the variable in the output data frame
       to sort, or its column number. Default is the last column.}\cr

  \item{table_prop}{Applies to a created \code{table} for the value of
        \code{compute}. Default value of \code{"none"} leaves frequencies.
        Value of \code{"all"} converts to cell proportions based on the
        grand total. Values of \code{"row"} and \code{"col"} provide
        proportions based on row and column sums.}
  \item{table_long}{Applies to the value of \code{compute} of \code{table}.
        If set to \code{TRUE}, then the cross-tabs table is output in
        long form, one count per row.}\cr

  \item{factors}{For \code{by} variables of type
        \code{character} and \code{integer}, converted to factors
        in the summary table by default, except for \code{Date}
        variables that always retain their type. If \code{FALSE}, then
        the \code{by} variables retain their original character or 
        integer type.}
  \item{q_num}{For the computation of quantiles, number of intervals. Default
        value of 4 provides quartiles.}
  \item{digits_d}{Number of significant digits for each displayed
       summary statistic. Trailing zeros are deleted, so, for example, integers
       display as integers. If not specified, defaults to 3
       unless there are more than 3 decimal digits and only a single
       digit to the left of the decimal point. Then enough digits are
       displayed to capture some non-zero decimal digits to avoid rounding
       to 0.000. To see all digits without trailing decimal 0's,
       set at a large number such as 20.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
}

\details{
\code{pivot} uses base R \code{\link{aggregate}} to generate a pivot table (Excel terminology). Express multiple categorical variables over which to pivot as a vector with the \code{\link{c}} function.

\code{pivot} provides two additional features than \code{\link{aggregate}} provides. First is a complete missing data analysis. If there is no missing data for the numerical variables that are aggregated, then the cell sizes are included with the aggregated data. If there is such missing data, then the amount of available data is displayed for all values to be aggregated for each cell.

The second is that the data parameter is listed first in the parameter list, which facilitates the use of the pipe operator from the \code{magrittr} package. Also, there is a different interface as the \code{by} variables are specified as a vector.

Variable ranges in the specification of \code{by} are not needed in general. Only a small number of grouping variables generally define the cells for the aggregation.

The following table lists available single summary statistics. The list is not necessarily exhaustive as the references are to functions provided by base R, including any not listed below.

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
The \code{table} computation applies to an aggregated variable that consists of discrete categories, such as the numbers 1 through 5 for responses to a 5-pt Likert scale. The result is a table of frequencies or proportions, a contingency table, referred to for two or more variables as a cross-tabulation table or a joint frequency distribution. Other statistical functions can be simultaneously computed with \code{table}, though only meaningful if the aggregated variable consists of a relatively small set of discrete, numeric values. 

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

# parameter values named
pivot(data=d, compute=mean, variable=Salary, by=c(Dept, Gender))

# visualize the aggregation
# when reading a table of coordinates, a, BarChart cannot deal with
#   with missing data so do not show groups that are missing as
#   another level
a <- pivot(d, mean, Salary, c(Dept, Gender), na_group_show=FALSE)
BarChart(Dept, Salary_mean, by=Gender, data=a)

# calculate mean of Years and Salary for each combination of Dept and Gender
# parameter values by position
pivot(d, mean, c(Years, Salary), c(Dept, Gender))

# output as a 2-d cross-tabulation table
pivot(d, mean, Salary, Dept, Gender)

# cross-tabulation table
pivot(d, table, Dept, Gender)
# long form
pivot(d, table, Dept, Gender, table_long=TRUE)

# multiple functions for which to aggregate
pivot(d, c(mean,sd,median,IQR), Years, c(Gender,Dept), digits_d=2)

# A variety of statistics computed for several variables over the
#  entire data set without aggregation
pivot(d, c(mean,sd,skew,kurtosis), c(Years,Salary,Pre,Post), digits_d=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{pivot}



\name{describe}
\alias{describe}
\alias{describe.default}
\alias{describe.formula}
\alias{describe.data.frame}

\title{Summary Statistics with an Option for Each Level of Another Variable}

\description{
Descriptive or summary statistics for a numeric variable, or summary statistics for all variables in the data matrix, both numeric and factors.  For a single variable, there is also an option for summary statistics at each level of a second, usually categorical variable or factor, with a relatively few number of levels.  Includes the sample mean, standard deviation, minimum, median and maximum for the numeric summary, and the table of counts for each value of a factor.
}

\usage{
describe(x, \ldots)

\method{describe}{default}(x, digits=NULL, lbl=NULL, \dots)

\method{describe}{formula}(formula, data=mydata, \dots)

\method{describe}{data.frame}(x, \dots)
}

\arguments{
 \item{x}{Values of response variable for first group.}
  \item{formula}{A \code{\link{formula}} of the form Y ~ X, where Y is the 
        numeric response variable compared across the two groups, and X is a grouping variable (factor) with two levels that define the corresponding groups.}
  \item{data}{An optional matrix or data frame containing the variables in 
        the formula. By default the variables are taken from environment (formula).}
  \item{lbl}{A name to use to label the output of a variable in lieu of its name.}
  \item{digits}{Specifies the number of decimal digits to display in the output.}
  \item{\dots}{Further arguments to be passed to or from methods, which is the option
       \code{digits} which specifies the number of decimal digits to display in the output when calling with a formula.}
}

\details{
The formula version specifies a categorical variable or factor, with a relatively few number of values called levels. The formula method is invoked when the data include a variable, generically referred to as X, and a numerical response variable, generically referred to as Y. The formula is of the form Y ~ X, with the names Y and X replaced by the actual variable names specific to a particular analysis.  The formula method automatically retrieves the names of the variables and data values for display on the resulting output. Then the response variable is analyze at each level of the factor.  

The digits parameter specifies the number of decimal digits in the output.  It must follow the formula specification when used with the formula version. By default the number of decimal digits is one more than the largest number of decimal digits in the data for that variable.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{summary}},  \code{\link{formula}}.
}

\examples{
# ----------------------------------------------------------
# Data simulated, call describe with a formula
# ----------------------------------------------------------

# Create simulated data, no population mean difference
# X has two values only, Y is numeric
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)

# Analyze all the values of Y
# Specify 3 decimal digits for each statistic listed
describe(Y, digits=3)

# Analyze data with formula version
# Get the summary statistics for Y at each level of X
# Specify 3 decimal digits for each statistic listed
describe(Y ~ X, digits=3)

# Summary for all variables in the data table
# Specify with a data frame instead of a variable
mydata <- data.frame(X, Y)
attach(mydata)
describe(mydata, digits=3)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }

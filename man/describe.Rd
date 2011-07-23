\name{describe}
\alias{describe}
\alias{describe.numeric}
\alias{describe.factor}
\alias{describe.formula}
\alias{describe.data.frame}
\alias{describe.character}
\alias{describe.default}

\title{Summary Statistics with an Option for Each Level of Another Variable}

\description{
Descriptive or summary statistics for a numeric variable or a factor, one at a time or for all numeric and factor variables in the data matrix.  For a single variable, there is also an option for summary statistics at each level of a second, usually categorical variable or factor, with a relatively few number of levels.  Includes the sample mean, standard deviation, minimum, median and maximum for the numeric summary, and the table of counts for each value of a factor.  For numeric variables, also includes the number of non-missing and missing values.
}

\usage{
describe(x=NULL, \ldots)

\method{describe}{numeric}(x, digits.d=NULL, lbl=NULL, \dots)

\method{describe}{factor}(x, lbl=NULL, \dots)

\method{describe}{formula}(formula, data=mydata, \dots)

\method{describe}{data.frame}(x, \dots)

\method{describe}{character}(x, lbl=NULL, \dots)

\method{describe}{default}(x, \dots)
}

\arguments{
 \item{x}{Values of response variable for first group.  If ignored, then the data frame
          mydata becomes the default value.}
  \item{formula}{A \code{\link{formula}} of the form Y ~ X, where Y is the 
        numeric response variable compared across the two groups, and X is a grouping variable (factor) with two levels that define the corresponding groups.}
  \item{data}{An optional matrix or data frame containing the variables in 
        the formula. By default the variables are taken from environment (formula).}
  \item{lbl}{A name to use to label the output of a variable in lieu of its name.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{\dots}{Further arguments to be passed to or from methods, which is the option
       \code{digits} which specifies the number of decimal digits to display in the output when calling with a formula.}
}

\details{
The formula version specifies a categorical variable or factor, with a relatively few number of values called levels. The formula method is invoked with an expression of the form Y ~ X, with the names Y and X replaced by the actual variable names specific to a particular analysis, where Y is a numeric variable and X is a categorical variable with relatively few values or levels.  The formula method automatically retrieves the names of the variables and data values for display on the resulting output. Then the response variable is analyzed at each level of the factor.  

The \code{digits.d} parameter specifies the number of decimal digits in the output.  It must follow the formula specification when used with the formula version. By default the number of decimal digits displayed for the analysis of a variable is one more than the largest number of decimal digits in the data for that variable.

The function \code{\link{rad}} in this package reads the data from an external csv file into the data frame called mydata.  To describe all of the variables in this data frame, invoke describe(mydata), or just describe(), which then defaults to the former.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

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
Y <- round(rnorm(n=n, mean=50, sd=10),3)

# Analyze all the values of numerical Y and categorical X
describe(Y)
describe(X)

# Analyze data with formula version
# Get the summary statistics for Y at each level of X
# Specify 3 decimal digits for each statistic displayed
describe(Y ~ X, digits.d=2)

# Analyze a small example data set from the web
# Read data into mydata data frame with the rad function 
# Optionally display the data frame by listing its name
# Analyze all variables in the data table with describe()
#rad("http://web.pdx.edu/~gerbing/data/employees2.csv")
#mydata
#describe()

# Use the subset function to specify a variable list
#describe(subset(mydata, select=c(Age:Dept,HealthPlan)))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }

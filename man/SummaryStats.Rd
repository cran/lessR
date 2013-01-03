\name{SummaryStats}
\alias{SummaryStats}
\alias{ss}
\alias{ss.brief}

\title{Summary Statistics for One or Two Variables}

\description{
Abbreviation: \code{ss}, \code{ss.brief}

Descriptive or summary statistics for a numeric variable or a factor, one at a time or for all numeric and factor variables in the data frame.  For a single variable, there is also an option for summary statistics at each level of a second, usually categorical variable or factor, with a relatively few number of levels.  For a numeric variable, output includes the sample mean, standard deviation, skewness, kurtosis, minimum, 1st quartile, median, third quartile and maximum, as well as the number of non-missing and missing values. For a categorical variable, the output includes the table of counts for each value of a factor, the total sample size, and the corresponding proportions.  
}

\usage{
SummaryStats(x=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 
         digits.d=NULL, brief=FALSE, \ldots)

ss.brief(\dots, brief=TRUE)

ss(\dots)
}

\arguments{
 \item{x}{Values of response variable for first group.  If ignored, then the data frame
          mydata becomes the default value.}
  \item{data}{Optional data frame that contains the variable of interest, default is \code{mydata}.}
  \item{by}{Applies to an analysis of a numeric variable, which is then analyzed at each level of the \code{by} variable. The variable is coerced to a factor.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{brief}{If \code{TRUE}, then only sample size information, mean, standard deviation,
        minimum, median and maximum are reported for a numeric variable. For a categorical
        variable, only the table of frequencies and the chi-square test are reported.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
OVERVIEW\cr
The \code{by} option specifies a categorical variable or factor, with a relatively few number of values called levels. The variable of interest is analyzed at each level of the factor. 

The \code{digits.d} parameter specifies the number of decimal digits in the output.  It must follow the formula specification when used with the formula version. By default the number of decimal digits displayed for the analysis of a variable is one more than the largest number of decimal digits in the data for that variable.

Reported outliers are based on the boxplot criterion.  The determination of an outlier is based on the length of the box, which corresponds, but may not equal exactly, the interquartile range.  A value is reported as an outlier if it is more than 1.5 box lengths away from the box.

The \code{lessR} function \code{\link{Read}} reads the data from an external csv file into the data frame called \code{mydata}.  To describe all of the variables in a data frame, invoke \code{SummaryStats(mydata)}, or just \code{SummaryStats()}, which then defaults to the former.

In the analysis of a categorical variable, if there are more than 10 levels then an abbreviated analysis is performed, only reporting the values and the associated frequencies. If all the values are unique, then the user is prompted with a note that perhaps this is actually an ID field which should be specified using the \code{row.names} option when reading the data.


DATA\cr
If the variable is in a data frame, the input data frame has the assumed name of \code{mydata}.  If this data frame is named something different, then specify the name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the \code{mydata$name} notation.

To analyze each variable in the \code{mydata} data frame, use \code{SummaryStats()}.  Or, for a data frame with a different name, insert the name between the parentheses.

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, \code{mydata} by default, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{> SummaryStats(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > SummaryStats(Y)     # directly reference Y}


}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{summary}}, \code{\link{formula}}, \code{\link{boxplot}}.
}

\examples{

# -------------------------------------------
# one or two numeric or categorical variables
# -------------------------------------------

# create data frame, mydata, to mimic reading data with rad function
# mydata contains both numeric and non-numeric data
# X has two character values, Y is numeric
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- round(rnorm(n=n, mean=50, sd=10),3)
mydata <- data.frame(X,Y)
rm(X); rm(Y)

# Analyze the values of numerical Y
# Calculate n, mean, sd, skew, kurtosis, min, max, quartiles
SummaryStats(Y)
# short name
ss(Y)

# Analyze the values of categorical X
# Calculate frequencies and proportions, totals, chi-square
SummaryStats(X)

# Only a subset of available summary statistics
ss.brief(Y)
ss.brief(X)

# Get the summary statistics for Y at each level of X
# Specify 2 decimal digits for each statistic displayed
SummaryStats(Y, by=X, digits.d=2)


# -----------------
# entire data frame 
# -----------------

# Analyze all variables in data frame mydata at once
# Any variables with a numeric data type and 4 or less
#  unique values will be analyzed as a categorical variable
SummaryStats()

# Analyze all variables in data frame mydata at once
# Any variables with a numeric data type and 7 or less
#  unique values will be analyzed as a categorical variable
SummaryStats(n.cat=7)


# ----------------------------------------
# data frame different from default mydata
# ----------------------------------------

# variables in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
SummaryStats(breaks, by=wool, data=warpbreaks)

# Analyze all variables in data frame warpbreaks at once
SummaryStats(warpbreaks)


# ----------------------------
# can enter many types of data
# ----------------------------

# generate and enter integer data
X1 <- sample(1:4, size=100, replace=TRUE)
X2 <- sample(1:4, size=100, replace=TRUE)
SummaryStats(X1)
SummaryStats(X1,X2)

# generate and enter type double data
X1 <- sample(c(1,2,3,4), size=100, replace=TRUE)
X2 <- sample(c(1,2,3,4), size=100, replace=TRUE)
SummaryStats(X1)
SummaryStats(X1, by=X2)

# generate and enter character string data
# that is, without first converting to a factor
Travel <- sample(c("Bike", "Bus", "Car", "Motorcycle"), size=25, replace=TRUE)
SummaryStats(Travel)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }



\name{ss}
\alias{ss}
\alias{ss.numeric}
\alias{ss.factor}
\alias{ss.data.frame}
\alias{ss.character}
\alias{ss.default}

\title{Summary Statistics with an Option for Each Level of Another Variable}

\description{
Descriptive or summary statistics for a numeric variable or a factor, one at a time or for all numeric and factor variables in the data frame  For a single variable, there is also an option for summary statistics at each level of a second, usually categorical variable or factor, with a relatively few number of levels.  For a numeric variable, output includes the sample mean, standard deviation, skewness, kurtosis, minimum, 1st quartile, median, third quartile and maximum, as well as the number of non-missing and missing values. For a cateogorical variable, the output includes the table of counts for each value of a factor, the total sample size, and the corresponding proportions.  
}

\usage{
ss(x=NULL, by=NULL, dframe=mydata, ncut=4, \ldots)

\method{ss}{numeric}(x, by=NULL, dframe=mydata, 
                  digits.d=NULL, brief=TRUE, \dots)

\method{ss}{factor}(x, by=NULL, \dots)

\method{ss}{data.frame}(x, ncut, \dots)

\method{ss}{character}(x, by=NULL, \dots)

\method{ss}{default}(x, \dots)
}

\arguments{
 \item{x}{Values of response variable for first group.  If ignored, then the data frame
          mydata becomes the default value.}
  \item{dframe}{Optional data frame that contains the variable of interest, default is \code{mydata}.}
  \item{by}{Appllies to an analysis of a numeric variable, which is then analyzed at each level of the \code{by} variable. The variable is coerced to a factor.}
  \item{ncut}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be anlayzed as a categorical. Set to 0 to turn off.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{brief}{If \code{TRUE}, then only sample size information, mean, standard deviation,
        minimum, median and maximum are reported for a numeric variable. For a categorical
        variable, only the table of frequencies and the chi-square test are reported.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
If the variable of interest is in a data frame, the input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the mydata$name notation. If no variable is specified, then all numeric variables in the entire data frame are analyzed and the results written to a pdf file.

The \code{by} option specifies a categorical variable or factor, with a relatively few number of values called levels. The variable of interest is analyzed at each level of the factor. 

The \code{digits.d} parameter specifies the number of decimal digits in the output.  It must follow the formula specification when used with the formula version. By default the number of decimal digits displayed for the analysis of a variable is one more than the largest number of decimal digits in the data for that variable.

Reported outliers are based on the boxplot critierion.  The determination of an outlier is based on the length of the box, which corresponds, but may not equal exactly, the interquartile range.  A value is reported as an outlier if it is more than 1.5 box lengths away from the box.

The function \code{\link{rad}} in this package reads the data from an external csv file into the data frame called mydata.  To describe all of the variables in this data frame, invoke ss(mydata), or just ss(), which then defaults to the former.

In the analysis of a categorical variable, if there are more than 10 levels then an abbreviated analysis is performed, only reporting the values and the associated frequencies. If all the values are unique, then the user is prompted with a note that perhaps this is actually an ID field which should be specified using the \code{row.names} option when reading the data.

Although not a formal paramater of \code{ss}, use \code{brief=FALSE} to print the three tables of proportions, for overall and rows and columns, for contingency tables with two dimensions. For numeric variables, adds to the reported statistics the skew and kurtosis and also the first and third quartiles.

}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{summary}}, \code{\link{formula}}, \code{\link{boxplot}}.
}

\examples{

# -----------------------------------------------
# ss for data frames and multiple variables
# -----------------------------------------------

# create data frame, mydata, to mimic reading data with rad function
# mydata contains both numeric and non-numeric data
# X has two character values, Y is numeric
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- round(rnorm(n=n, mean=50, sd=10),3)
mydata <- data.frame(X,Y)
rm(X); rm(Y)

# Analyze all the values of numerical Y, then categorical X
ss(Y)
ss(X)

# Analyze all the values of Y and X with more output 
ss(Y, brief=FALSE)
ss(X, brief=FALSE)

# Get the summary statistics for Y at each level of X
# Specify 2 decimal digits for each statistic displayed
ss(Y, by=X, digits.d=2)

# Analyze all variables in data frame mydata at once
# Any variables with a numeric data type and 4 or less
#  unique values will be analyzed as a categorical variable
ss()

# Analyze all variables in data frame mydata at once
# Any variables with a numeric data type and 7 or less
#  unique values will be analyzed as a categorical variable
ss(ncut=7)

# variables in a data frame which is not the default mydata
# access the breaks variable in the R provided warpbreaks data set
# although data not attached, access the variable directly by its name
data(warpbreaks)
ss(breaks, by=wool, dframe=warpbreaks)

# Analyze all variables in data frame warpbreaks at once
ss(warpbreaks)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }

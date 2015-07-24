\name{SummaryStats}
\alias{SummaryStats}
\alias{ss}
\alias{ss.brief}

\title{Summary Statistics for One or Two Variables}

\description{
Abbreviation: \code{ss}, \code{ss.brief}

Descriptive or summary statistics for a numeric variable or a factor, one at a time or for all numeric and factor variables in the data frame.  For a single variable, there is also an option for summary statistics at each level of a second, usually categorical variable or factor, with a relatively few number of levels.  For a numeric variable, output includes the sample mean, standard deviation, skewness, kurtosis, minimum, 1st quartile, median, third quartile and maximum, as well as the number of non-missing and missing values. For a categorical variable, the output includes the table of counts for each value of a factor, the total sample size, and the corresponding proportions.  

If the provided object to analyze is a set of multiple variables, including an entire data frame, then each non-numeric variable in the data frame is analyzed and the results written to a pdf file in the current working directory. The name of each output pdf file that contains a bar chart and its path are specified in the output.

When output is assigned into an object, such as \code{s} in \code{s <- ss(Y)}, the pieces of output can be accessed for later analysis. A primary such analysis is \code{knitr} for dynamic report generation in which R output embedded in documents See \code{value} below.
}

\usage{
SummaryStats(x=NULL, by=NULL, data=mydata, n.cat=getOption("n.cat"), 
         digits.d=NULL, brief=getOption("brief"), \ldots)

ss.brief(\dots, brief=TRUE)

ss(\dots)
}

\arguments{
  \item{x}{Variable(s) to analyze.  Can be a single variable, either
        within a data frame or as a vector in the user's workspace,
        or multiple variables in a data frame such as designated with the
        \code{\link{c}} function, or an entire data frame. If not specified,
        then defaults to all variables in the specified data frame,
        \code{mydata} by default.}
  \item{by}{Applies to an analysis of a numeric variable, which is then analyzed at each level of the \code{by} variable. The variable is coerced to a factor.}
  \item{data}{Optional data frame that contains the variable of interest, default is \code{mydata}.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{brief}{If set to \code{TRUE}, reduced text output. Can change system default with \code{\link{set}} function.}

  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
OVERVIEW\cr
The \code{by} option specifies a categorical variable or factor, with a relatively few number of values called levels. The variable of interest is analyzed at each level of the factor. 

The \code{digits.d} parameter specifies the number of decimal digits in the output.  It must follow the formula specification when used with the formula version. By default the number of decimal digits displayed for the analysis of a variable is one more than the largest number of decimal digits in the data for that variable.

Reported outliers are based on the boxplot criterion.  The determination of an outlier is based on the length of the box, which corresponds, but may not equal exactly, the interquartile range.  A value is reported as an outlier if it is more than 1.5 box lengths away from the box.

Skewness is computed with the usual adjusted Fisher-Pearson standardized moment skewness coefficient, the version found in many commercial packages.

The \code{lessR} function \code{\link{Read}} reads the data from an external csv file into the data frame called \code{mydata}.  To describe all of the variables in a data frame, invoke \code{SummaryStats(mydata)}, or just \code{SummaryStats()}, which then defaults to the former.

In the analysis of a categorical variable, if there are more than 10 levels then an abbreviated analysis is performed, only reporting the values and the associated frequencies. If all the values are unique, then the user is prompted with a note that perhaps this is actually an ID field which should be specified using the \code{row.names} option when reading the data.


DATA\cr
If the variable is in a data frame, the input data frame has the assumed name of \code{mydata}.  If this data frame is named something different, then specify the name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name, that is, no need to invoke the \code{mydata$name} notation.

To analyze each variable in the \code{mydata} data frame, use \code{SummaryStats()}.  Or, for a data frame with a different name, insert the name between the parentheses. To analyze a subset of the variables in a data frame, specify the list with either a : or the \code{\link{c}} function, such as m01:m03 or c(m01,m02,m03).

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

ONLY VARIABLES ARE REFERENCED\cr
The referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{> SummaryStats(rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > Y <- rnorm(50)   # create vector Y in user workspace
    > SummaryStats(Y)     # directly reference Y}


}

\value{
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. Redesigned in \code{lessR} version 3.3 to provide two different types of components: the pieces of readable output in character format, and a variety of statistics. The readable output are character strings such as tables amenable for reading. The statistics are numerical values amenable for further analysis. A primary motivation of these two types of output is to facilitate \code{knitr} documents, as the name of each piece can be inserted into the \code{knitr} document.

If the analysis is of a single numeric variable, the full analysis returns the following statistics: \code{n}, \code{miss}, \code{mean}, \code{sd}, \code{skew}, \code{kurtosis}, \code{min}, \code{quartile1}, \code{median}, \code{quartile3}, \code{max}, \code{IQR}. The brief analysis returns the corresponding subset of the summary statistics. If a \code{by} variable is included in the analysis, then nothing is returned except the text output. The pieces of readable output are \code{out_stats} and \code{out_outliers}.

If the analysis is of a single categorical variable, a list is invisibly returned with two tables, the frequencies and the proportions, respectively named \code{freq} and \code{prop}. The pieces of readable output are \code{out_title} and \code{out_stats}.

If two categorical variables are analyzed, then for the full analysis four tables are returned as readable output, but no numerical statistics. The pieces are \code{out_title}, \code{out_freq}, \code{out_prop}, \code{out_colsum}, \code{out_rowsum}.

Although not typically needed, if the output is assigned to an object named, for example, \code{s}, as in \code{s <- ss(Y)}, then the contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(s)}.
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
n <- 15
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- round(rnorm(n=n, mean=50, sd=10),3)
mydata <- data.frame(X,Y)
rm(X); rm(Y)

# Analyze the values of numerical Y
# Calculate n, mean, sd, skew, kurtosis, min, max, quartiles
SummaryStats(Y)
# short name
ss(Y)
# output saved for later analysis
s <- ss(Y)
# view full text output
s
# view just the outlier analysis
s$out_outliers
# list the names of all the components
names(s)

# Analyze the values of categorical X
# Calculate frequencies and proportions, totals, chi-square
SummaryStats(X)

# Only a subset of available summary statistics
ss.brief(Y)
ss.brief(X)

# Reference the summary stats in the object: stats
stats <- ss(Y)
my.mean <- stats$mean

# Get the summary statistics for Y at each level of X
# Specify 2 decimal digits for each statistic displayed
SummaryStats(Y, by=X, digits.d=2)


# ----------
# data frame 
# ----------

# Analyze all variables in data frame mydata at once
# Any variables with a numeric data type and 4 or less
#  unique values will be analyzed as a categorical variable
SummaryStats()

# Analyze all variables in data frame mydata at once
# Any variables with a numeric data type and 7 or less
#  unique values will be analyzed as a categorical variable
SummaryStats(n.cat=7)

# analyze just a subset of a data frame
mydata <- Read("Employee", format="lessR", quiet=TRUE)
SummaryStats(c(Salary,Years))


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






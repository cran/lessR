\name{ttest}
\alias{ttest}
\alias{tt}
\alias{tt.brief}

\title{Generic Method for t-test and Standardized Mean Difference with Enhanced Graphics}

\description{
Abbreviation: \code{tt}, \code{tt.brief}

Provides enhanced output from the standard \code{\link{t.test}} function applied to the analysis of the mean of a single variable, or the independent groups analysis of the mean difference, from either data or summary statistics. Includes the analysis of a dependent-groups analysis from the data. The data can be in the form of a data frame or separate vectors of data, one for each group. This output includes the basic descriptive statistics, analysis of assumptions and the hypothesis test and confidence interval. For two groups the output also includes the analysis for both with and without the assumption of homogeneous variances, the pooled or within-group standard deviation, and the standardized mean difference or Cohen's \emph{d} and its confidence interval.

The output from data for two groups introduces the \acronym{ODDSMD} plot, which displays the Overlapping Density Distributions of the two groups as well as the means, mean difference and Standardized Mean Difference. The plot also includes the results of the descriptive and inferential analyses. For the dependent-groups analysis, a scatter plot of the two groups of data also is produced, which includes the diagonal line through the scatter plot that represents equality, and a line segment for each point in the scatter plot which is the vertical distance from the point to the diagonal line to display the amount of change.

Can also be called from the more general \code{\link{model}} function.
}

\usage{
ttest(x=NULL, y=NULL, data=mydata, paired=FALSE,

         n=NULL, m=NULL, s=NULL, mu=NULL, 
         n1=NULL, n2=NULL, m1=NULL, m2=NULL, s1=NULL, s2=NULL, 

         Ynm="Y", Xnm="X", X1nm="Group1", X2nm="Group2", xlab=NULL, 

         brief=getOption("brief"), digits.d=NULL, conf.level=0.95,
         alternative=c("two.sided", "less", "greater"),
         mmd=NULL, msmd=NULL, Edesired=NULL, 

         show.title=TRUE, bw1="bcv", bw2="bcv",

         graph=TRUE, line.chart=FALSE,
         width=5, height=5, pdf.file=NULL, ...)       

tt.brief(\ldots, brief=TRUE)

tt(\ldots)
}

\arguments{
  \item{x}{A \code{\link{formula}} of the form Y ~ X, where Y is the 
        numeric response variable compared across the two groups, and X is a
        grouping variable with two levels that define the corresponding groups,
        or, if the data are submitted in the form of two vectors, the responses
        for the first group.}
  \item{y}{If \code{x} is not a formula, the responses for the second group,
        otherwise \code{NULL}.}
  \item{n}{Sample size for one group.}
  \item{m}{Sample size for one group.}
  \item{s}{Sample size for one group.}
  \item{mu}{Hypothesized mean for one group. If not present, then confidence interval only.}
  \item{n1}{Sample size for first of two groups.}
  \item{n2}{Sample size for second of two groups.}
  \item{m1}{Sample mean for first of two groups.}
  \item{m2}{Sample mean for second of two groups.}
  \item{s1}{Sample standard deviation for first of two groups.}
  \item{s2}{Sample standard deviation for second of two groups.}
  \item{data}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{paired}{Set to \code{TRUE} for a dependent-samples t-test with two data vectors 
       or variables from a data frame, with the difference computed from subtracting
       the first vector from the second.}
  \item{Ynm}{Name of response variable.}
  \item{Xnm}{Name of predictor variable, the grouping variable or factor with exactly
        two levels.}
  \item{X1nm}{Value of grouping variable, the level that defines the first group.}
  \item{X2nm}{Value of grouping variable, the level that defines the second group.}
  \item{xlab}{x-axis label, defaults to variable name, or, if present, variable label.}
  \item{brief}{If set to \code{TRUE}, reduced text output. Can change system default with \code{\link{style}} function.}
  \item{digits.d}{Number of decimal places for which to display numeric values. Suggestion only.}
  \item{conf.level}{Confidence level of the interval, expressed as a proportion.}
  \item{alternative}{Default is \code{"two.sided"}. Other values are \code{"less"}
       and \code{"greater"}.}
  \item{mmd}{Minimum Mean Difference of practical importance, the difference of the
       response variable between two group means. The concept is optional, and only one of mmd
       and msmd is provided.}
  \item{msmd}{For the Standardized Mean Difference, Cohen's d, the Minimum value of
       practical importance. The concept is optional, and only one of mmd and msmd is provided.}
  \item{Edesired}{The desired margin of error for the needed sample size calculation for a 95\% confidence interval, based on Kupper and Hafner (1989).}
  \item{show.title}{Show the title on the graph of the density functions for two groups.}
  \item{bw1}{Bandwidth for the computation of the densities for the first group.}
  \item{bw2}{Bandwidth for the computation of the densities for the second group.}
  \item{graph}{If \code{TRUE}, then display the graph of the overlapping density distributions.}
  \item{line.chart}{Plot the run chart for the response variable for each group
        in the analysis.}
  \item{pdf.file}{Name of the pdf file to which the density graph is redirected. Also
       specifies to save the line charts with pre-assigned names if they are computed.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
OVERVIEW\cr
If \code{n} or \code{n1} are set to numeric values, then the analysis proceeds from the summary statistics, the sample size and mean and standard deviation of each group. Missing data are counted and then removed for further analysis of the non-missing data values.  Otherwise the analysis proceeds from data, which can be in a data frame, by default named \code{mydata}, with a grouping variable and response variable, or in two data vectors, one for each group.

Following the format and syntax of the standard \code{\link{t.test}} function, to specify the two-group test with a formula, \code{\link{formula}}, the data must include a variable that has exactly two values, a grouping variable or factor generically referred to as X, and a numerical response variable, generically referred to as Y. The formula is of the form Y ~ X, with the names Y and X replaced by the actual variable names specific to a particular analysis.  The formula method automatically retrieves the names of the variables and data values for display on the resulting output.

The values of the response variable Y can be organized into two vectors, the values of Y for each group in its corresponding vector. When submitting data in this form, the output is enhanced if the actual names of the variables referred to generically as X and Y, as well as the names of the levels of the factor X, are explicitly provided. 

For the output, when computed from the data the two groups are automatically arranged so that the group with the larger mean is listed as the first group.  The result is that the resulting mean difference, as well as the standardized mean difference, is always non-negative.

The inferential analysis in the full version provides both homogeneity of variance and the Welch test which does not assume homogeneity of variance. Only a two-sided test is provided.  The null hypothesis is a population mean difference of 0.

If computed from the data, the bandwidth parameter controls the smoothness of the estimated density curve.  To obtain a smoother curve, increase the bandwidth from the default value.

DATA\cr
If the input data frame is named something different than \code{mydata}, then specify the name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name without having to invoke the mydata$name notation. 

PRACTICAL IMPORTANCE\cr
The practical importance of the size of the mean difference is addressed when one of two parameter values are supplied, the minimum mean difference of practical importance, mmd, or the corresponding standardized version, msmd.  The  remaining value is calculated and both values are added to the graph and the console output.

DECIMAL DIGITS\cr
The number of decimal digits is determined by default from the largest number of decimal digits of the entered descriptive statistics.  The number of decimal digits is then set at that value, plus one more with a minimum of two decimal digits by default.  Or, override the default with the \code{digits.d} parameter.

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.
}

\value{
Returned value is NULL except for a two-group analysis from a formula.  Then the values for the response variable of the two groups are separated and returned invisibly as a list for further analysis as indicated in the examples below. The first group of data values is the group with the largest sample mean.
\item{value1}{Value of the grouping variable for the first group.}
\item{group1}{Data values for the first group.}
\item{value2}{Value of the grouping variable for the second group.}
\item{group2}{Data values for the second group.}
}


\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 8, NY: Routledge.

Kupper and Hafner (1989). The American Statistician, 43(2):101-105.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{t.test}}, \code{\link{density}}, \code{\link{plot.density}}, \code{\link{ttestPower}}, \code{\link{formula}}.
}

\examples{
# ----------------------------------------------------------
# tt for two groups, from a formula
# ----------------------------------------------------------

mydata <- Read("Employee", in.lessR=TRUE, quiet=TRUE)


# analyze data with formula version
# variable names and levels of X are automatically obtained from data
# although data frame not attached, reference variable names directly
ttest(Salary ~ Gender)

# short form
tt(Salary ~ Gender)

# brief version of results
tt.brief(Salary ~ Gender)

# return the vectors group1 and group2 into the object t.out
# separate the data values for the two groups and analyze separately
t.out <- ttest(Salary ~ Gender)
Histogram(group1, data=t.out)
Histogram(group2, data=t.out)

# compare to standard R function t.test
t.test(mydata$Salary ~ mydata$Gender, var.equal=TRUE)

# consider the practical importance of the difference
ttest(Salary ~ Gender, msmd=.5)

# obtain the line chart of the response variable for each group
ttest(Salary ~ Gender, line.chart=TRUE)

# variable of interest is in a data frame which is not the default mydata
# access the data frame in the lessR dat.twogroup data set
# although data not attached, access the variables directly by their name
data(dataLearn)
ttest(Score ~ StudyType, data=dataLearn)


# ----------------------------------------------------------
# tt for a single group, from data
# ----------------------------------------------------------

# confidence interval only, from data
ttest(Salary)

# confidence interval and hypothesis test, from data
ttest(Salary, mu=52000)


# -------------------------------------------------------
# tt for two groups from data stored in two vectors 
# -------------------------------------------------------

# create two separate vectors of response variable Y
# the vectors exist are not in a data frame
#   their lengths need not be equal
Y1 <- round(rnorm(n=10, mean=50, sd=10),2)
Y2 <- round(rnorm(n=10, mean=60, sd=10),2)

# analyze the two vectors directly
# usually explicitly specify variable names and levels of X
#   to enhance the readability of the output
ttest(Y1, Y2, Ynm="MyY", Xnm="MyX", X1nm="Group1", X2nm="Group2")

# dependent groups t-test from vectors in global environment
ttest(Y1, Y2, paired=TRUE)

# dependent groups t-test from variables in data frame mydata
mydata <- data.frame(Y1,Y2)
rm(Y1);  rm(Y2)
ttest(Y1, Y2, paired=TRUE)
# independent groups t-test from variables (vectors) in a data frame
ttest(Y1, Y2)


# -------------------------------------------------------
# tt from summary statistics
# -------------------------------------------------------

# one group: sample size, mean and sd
# optional variable name added
tt(n=34, m=8.92, s=1.67, Ynm="Time")

# confidence interval and hypothesis test, from descriptive stats
# get rid of the data frame, analysis should still proceed
rm(mydata)
tt.brief(n=34, m=8.92, s=1.67, mu=9, conf.level=0.90)

# two groups: sample size, mean and sd for each group
# specify the briefer form of the output
tt.brief(n1=19, m1=9.57, s1=1.45, n2=15, m2=8.09, s2=1.59)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ smd }



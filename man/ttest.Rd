\name{ttest}
\alias{ttest}
\alias{tt}
\alias{tt.brief}
\alias{smd.t.test}

\title{Generic Method for t-test and Standardized Mean Difference with Enhanced Graphics}

\description{
Abbreviation: \code{tt}, \code{tt.brief}

Provides enhanced output from the standard \code{\link{t.test}} function applied to the analysis of the mean of a single variable or the independent groups analysis of the mean difference, from either data or summary statistics. The data can be in the form of a data frame or two separate vectors of data, one for each group. This output includes the basic descriptive statistics, analysis of assumptions and the hypothesis test and confidence interval. For two groups the output also includes the pooled or within-group standard deviation, and the standardized mean difference or Cohen's \emph{d} and its confidence interval. The output also includes the inferential analysis based on the Welch test which does not assume equal variances. The output from data for two groups introduces the \acronym{ODDSMD} plot, which displays the Overlapping Density Distributions of the two groups as well as the means, mean difference and Standardized Mean Difference. The plot also includes the results of the descriptive and inferential analyses.

Can also be called from the more general \code{\link{model}} function.
}

\usage{
ttest(x=NULL, y=NULL, dframe=mydata,
         n = NULL, m = NULL, s = NULL, mu0 = NULL, 
         n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
         Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
         brief=FALSE, digits.d = NULL, 
         conf.level = 0.95, mmd = NULL, msmd = NULL, 
         bw1 = "nrd", bw2 = "nrd", \ldots)

tt.brief(\ldots, brief=TRUE)

tt(\ldots)

smd.t.test(\ldots)
}

\arguments{
  \item{x}{A \code{\link{formula}} of the form Y ~ X, where Y is the 
        numeric response variable compared across the two groups, and X is a]
        grouping variable (factor) with two levels that define the corresponding groups,
        or the values of response variable for first group.}
  \item{y}{If \code{x} is not a formula, values of response variable for second group,
        otherwise \code{NULL}.}
  \item{n}{Sample size for one group.}
  \item{m}{Sample size for one group.}
  \item{s}{Sample size for one group.}
  \item{mu0}{Hypothesized mean for one group. If not present, then confidence interval only.}
  \item{n1}{Sample size for first of two groups.}
  \item{n2}{Sample size for second of two groups.}
  \item{m1}{Sample mean for first of two groups.}
  \item{m2}{Sample mean for second of two groups.}
  \item{s1}{Sample standard deviation for first of two groups.}
  \item{s2}{Sample standard deviation for second of two groups.}
  \item{dframe}{Data frame that contains the variable of interest, default is \code{mydata}.}
  \item{Ynm}{Name of response variable.}
  \item{Xnm}{Name of predictor variable, the grouping variable or factor with exactly
        two levels.}
  \item{X1nm}{Value of grouping variable, the level that defines the first group.}
  \item{X2nm}{Value of grouping variable, the level that defines the second group.}
  \item{brief}{Extent of displayed results.}
  \item{digits.d}{Number of decimal places for which to display numeric values. Suggestion only.}
  \item{conf.level}{Confidence level of the interval, expressed as a proportion.}
  \item{mmd}{Minimum Mean Difference of practical importance, the difference of the
       response variable between two group means. The concept is optional, and only one of mmd
       and msmd is provided.}
  \item{msmd}{For the Standardized Mean Difference, Cohen's d, the Minimum value of
       practical importance. The concept is optional, and only one of mmd and msmd is provided.}
  \item{bw1}{Bandwidth for the computation of the densities for the first group.}
  \item{bw2}{Bandwidth for the computation of the densities for the second group.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
If \code{n} or \code{n1} are set to values, then the analysis proceeds from the summary statistics, the sample size and mean and standard deviation of each group. Missing data are counted and then removed for further analysis of the non-missing data values.  Otherwise the analysis proceeds from the data, which can be in a data frame with a grouping variable and response variable, or in two data vectors, one for each group.

Following the format and syntax of the standard \code{\link{t.test}} function, the methods for the generic function \code{tt} include formula and default.  The formula method is invoked when the data include a variable that has exactly two values, a grouping variable or factor generically referred to as X, and a numerical response variable, generically referred to as Y. The formula is of the form Y ~ X, with the names Y and X replaced by the actual variable names specific to a particular analysis.  The formula method automatically retrieves the names of the variables and data values for display on the resulting output.

The default method is invoked when the values of the response variable Y are organized into two vectors, the values of Y for each group in the corresponding vector. The vectors must be defined in the user workspace as they are generally of unequal length and so generally not conformable to a data frame. When submitting data in this form, the output is enhanced if the actual names of the variables referred to generically as X and Y, as well as the names of the levels of the factor X, are explicitly provided. 

The formula version assumes the data are in a data frame. The input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name without having to invoke the mydata$name notation.  The split version of the response variable, that is, its values organized into two vectors, one for each group, are saved in the global environment under the names \code{group1} and \code{group2} for further analysis if desired.

This version of \code{tt} provides the inferential analysis for both homogeneity of variance and the Welch test which does not assume homogeneity of variance. Only a two-sided test is provided.  The null hypothesis is a population mean difference of 0.

If computed from the data, the bandwidth parameter controls the smoothness of the estimated density curve.  To obtain a smoother curve, increase the bandwidth from the default value.

For the output, when computed from the data the two groups are automatically arranged so that the group with the larger mean is listed as the first group.  The result is that the resulting mean difference, as well as the standardized mean difference, is always non-negative.

The confidence interval of the standardized mean difference is computed by the \code{ci.smd} function, written by Ken Kelley, from the \code{MBESS} package.

The practical importance of the size of the mean difference is addressed when one of two parameter values are supplied, the minimum mean difference of practical importance, mmd, or the corresponding standardized version, msmd.  The  remaining value is calculated and both values are added to the graph and the console output.

After running \code{tt}, the following statistics are available for further analysis: sample sizes \code{n1} and \code{n2}, sample means \code{m1} and \code{m2}, sample standard deviations, \code{s1} and \code{s2}, plus the within-group or pooled standard deviation, \code{sw}. For example, if the t-test does not achieve significance, then perhaps a power curve is of interest, obtained with the \code{lessR} function \code{\link{ttp}}.  

The number of decimal digits is determined by default from the largest number of decimal digits of the entered descriptive statistics.  The number of decimal digits is then set at that value, plus one more with a minimum of two decimal digits by default.  Or, override the default with the \code{digits.d} parameter.

A labels data frame named \code{mylabels}, obtained from the \code{\link{Read}} function, can list the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the varible label for the response variable and the grouping variable is listed in the text output. 
}


\references{
Kelley, K., \code{smd} function from the \code{MBESS} package.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{t.test}}, \code{\link{density}}, \code{\link{plot.density}}, \code{\link{powercurve.t.test}}, \code{\link{formula}}.
}

\examples{
# ----------------------------------------------------------
# tt for two groups, from a formula
# ----------------------------------------------------------

# create simulated data, no population mean difference
# X has two values only, Y is numeric
# put into a data frame, required for formula version
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(X,Y)

# analyze data with formula version
# variable names and levels of X are automatically obtained from data
# although data frame not attached, reference variable names directly
ttest(Y ~ X)
# short form
tt(Y ~ X)
# brief version of results
tt.brief(Y ~ X)
# Compare to standard R function t.test
t.test(Y ~ X, var.equal=TRUE)

# consider the practical importance of the difference
ttest(Y ~ X, msmd=.5)

# variable of interest is in a data frame which is not the default mydata
# access the data frame in the lessR dat.twogroup data set
# although data not attached, access the variables directly by their name
data(dat.twogroup)
ttest(ShipTime ~ Supplier, dframe=dat.twogroup)


# -------------------------------------------------------
# tt for two groups from data stored in two vectors 
# -------------------------------------------------------

# create two separate vectors of response variable Y
# the vectors exist are not in a data frame
#   their lengths need not be equal
n <- 10
Y1 <- rnorm(n=n/2, mean=50, sd=10)
Y2 <- rnorm(n=n/2, mean=60, sd=10)

# analyze the two vectors directly
# usually explicitly specify variable names and levels of X
#   to enhance the readability of the output
ttest(Y1, Y2, Ynm="MyY", Xnm="MyX", X1nm="Group1", X2nm="Group2")


# ----------------------------------------------------------
# tt for a single group, from data
# ----------------------------------------------------------

# confidence interval only, from data
ttest(Y)

# confidence interval and hypothesis test, from data
ttest(Y, mu0=52)


# -------------------------------------------------------
# tt from summary statistics
# -------------------------------------------------------

# one group: sample size, mean and sd
# optional variable name added
tt(n=34, m=8.92, s=1.67, Ynm="Time")

# confidence interval and hypothesis test, from descriptive stats
tt(n=34, m=8.92, s=1.67, mu0=9)

# two groups: sample size, mean and sd for each group
# specify the briefer form of the output
tt.brief(n1=19, m1=9.57, s1=1.45, n2=15, m2=8.09, s2=1.59)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ smd }



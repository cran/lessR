\name{ttest}
\alias{ttest}
\alias{ttest.brief}
\alias{ttest.default}
\alias{ttest.formula}

\title{Generic Method for t-test and Standardized Mean Difference with Enhanced Graphics}

\description{
Provides enhanced output from the standard \code{\link{t.test}} function applied to the independent groups analysis of the mean difference. This output includes the basic descriptive statistics, the pooled or within-group standard deviation, and the standardized mean difference or Cohen's \emph{d} and its confidence interval, as well as an evaluation of assumptions. The output introduces the \acronym{ODDSMD} plot, which displays the Overlapping Density Distributions of the two groups as well as the means, mean difference and Standardized Mean Difference. The plot also includes the results of the descriptive and inferential analyses.
}

\usage{
ttest(x, y=NULL, \ldots)

\method{ttest}{brief}(x, y=NULL, dframe=mydata, brief=TRUE, \ldots)

\method{ttest}{default}(x, y, 
         Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
         brief=FALSE, digits.d = 2, 
         conf.level = 0.95, mmd = NULL, msmd = NULL, 
         bw1 = "nrd", bw2 = "nrd", \dots)

\method{ttest}{formula}(my.formula, y=NULL, dframe=mydata, \dots)
}

\arguments{
  \item{x}{A formula, or the values of response variable for first group.}
  \item{y}{If \code{x} is not a formula, values of response variable for second group, otherwise \code{NULL}.}
  \item{my.formula}{A \code{\link{formula}} of the form Y ~ X, where Y is the 
        numeric response variable compared across the two groups, and X is a grouping variable (factor) with two levels that define the corresponding groups.}
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
       response variable between two group means. The concept is optional, and only one of mmd and msmd is provided.}
  \item{msmd}{For the Standardized Mean Difference, Cohen's d, the Minimum value of
       practical importance. The concept is optional, and only one of mmd and msmd is provided.}
  \item{bw1}{Bandwidth for the computation of the densities for the first group.}
  \item{bw2}{Bandwidth for the computation of the densities for the second group.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
Following the format and syntax of the standard \code{\link{t.test}} function, the two methods for the generic function \code{ttest} are formula and default.  The formula method is invoked when the data include a variable that has exactly two values, a grouping variable or factor generically referred to as X, and a numerical response variable, generically referred to as Y. The formula is of the form Y ~ X, with the names Y and X replaced by the actual variable names specific to a particular analysis.  The formula method automatically retrieves the names of the variables and data values for display on the resulting output.

The default method is invoked when the values of the response variable Y are organized into two vectors, the values of Y for each group in the corresponding vector.  When submitting data in this form, the output is enhanced if the actual names of the variables referred to generically as X and Y, as well as the names of the levels of the factor X, are explicitly provided. 

The formula version assumes the data are in a data frame. The input data frame has the assumed name of mydata.  If this data frame is named something different, then specify the name with the \code{dframe} option.  Regardless of its name, the data frame need not be attached to reference the variable directly by its name without having to invoke the mydata$name notation.

This version of \code{ttest} assumes homogeneity of variance in the computation of the standard error of the mean difference. Also, only a two-sided test is provided.  The null hypothesis is a population mean difference of 0.

The bandwidth parameter controls the smoothness of the estimated density curve.  To obtain a smoother curve, increase the bandwidth from the default value.

For the output, the two groups are automatically arranged so that the group with the larger mean is listed as the first group.  The result is that the resulting mean difference, as well as the standardized mean difference, is always non-negative.

The confidence interval of the standardized mean difference is computed by the \code{ci.smd} function, written by Ken Kelley, from the \code{MBESS} package.

The practical importance of the size of the mean difference is addressed when one of two parameter values are supplied, the minimum mean difference of practical importance, mmd, or the corresponding standardized version, msmd.  The  remaining value is calculated and both values are added to the graph and the console output.

After running \code{ttest}, the following statistics are available for further analysis: sample sizes \code{n1} and \code{n2}, sample means \code{m1} and \code{m2}, sample standard deviations, \code{s1} and \code{s2}, plus the within-group or pooled standard deviation, \code{sw}. For example, if the t-test does not achieve significance, then perhaps a power curve is of interest.  Then \code{n1}, \code{n2} and \code{sw} can be used to construct a power curve without re-entering their values,  \cr
\code{\link{powercurve.t.test}()}.  

A labels data frame named \code{mylabels}, obtained from the \code{\link{rad}} function, can list the label for some or all of the variables in the data frame that contains the data for the analysis.  If this labels data frame exists, then the varible label for the response variable and the grouping variable is listed in the text output. 
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
# call ttest with a formula
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
# although data frame is not attached, can reference the variable
#   names directly
ttest(Y ~ X)
# brief version of results
ttest.brief(Y ~ X)
# Compare to standard R function t.test
t.test(Y ~ X, var.equal=TRUE)

# consider the practical importance of the difference
ttest(Y ~ X, msmd=.5)

# variable of interest is in a data frame which is not the default mydata
# access the data frame in the R provided warpbreaks data set
# although data not attached, access the variables directly by their name
data(warpbreaks)
ttest(breaks ~ wool, dframe=warpbreaks)


# -------------------------------------------------------
# data stored in two vectors, call ttest accordingly
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
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ smd }

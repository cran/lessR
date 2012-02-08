\name{stats.t.test}
\alias{stats.t.test}

\title{Generic Method for One- or Two-Sample t-test from Summary Statistics}

\description{
Performs the two-tailed t-test for either one or two samples (groups) of the mean or mean difference, respectively, from sample statistics.  For two groups, performs the independent-groups t-test assuming equal group population variances and a null hypothesis of zero. The comparison of means is accomplished from the six basic summary statistics, the sample size, sample mean and sample standard deviation for each group.  Displays the basic t-value and p-value and also many of the intermediate computations, such as, for two groups, the within-group or pooled standard deviation. Also displays the standardized mean difference for the two-sample t-test.
}

\usage{
stats.t.test(n = NULL, m = NULL, s = NULL, mu0 = NULL, 
         n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, s2 = NULL, 
         Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
         conf.level = 0.95, digits.d = NULL, \dots)
}

\arguments{
  \item{n}{Sample size for one group.}
  \item{m}{Sample size for one group.}
  \item{s}{Sample size for one group.}
  \item{mu0}{Hypothesized mean for one group.}
  \item{n1}{Sample size for first of two groups.}
  \item{n2}{Sample size for second of two groups.}
  \item{m1}{Sample mean for first of two groups.}
  \item{m2}{Sample mean for second of two groups.}
  \item{s1}{Sample standard deviation for first of two groups.}
  \item{s2}{Sample standard deviation for second of two groups.}
  \item{Ynm}{Name of response variable.}
  \item{Xnm}{Name of predictor variable for two groups, the grouping variable or factor with exactly 
      two levels.}
  \item{X1nm}{Value of grouping variable, the level that defines the first of two groups.}
  \item{X2nm}{Value of grouping variable, the level that defines the second of two groups.}
  \item{conf.level}{Confidence level of the interval, expressed as a proportion.}
  \item{digits.d}{Number of decimal digits for which to display numeric values. 
      Suggestion only.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
This function parallels the \code{\link{ttest}} function included in this package, which performs the independent-groups t-test from the data, as well as the standard \code{\link{t.test}} function.  In contrast, \code{\link{stats.t.test}} performs the analysis from the basic summary statistics: the sample size, sample mean and sample standard deviation for either one group or for each of two groups. Accordingly, there is no graphic output from this function as with \code{\link{ttest}}.

This version of \code{stats.t.test} assumes homogeneity of variance in the computation of the standard error of the mean difference. Also, only a two-sided test is provided.  The null hypothesis is a population mean difference of 0.

If the sample size, sample mean and sample standard deviation are presented only for one group, then the hypothesized value should be specified with the \code{mu0} parameter.  For two groups, the null hypothesis for the population mean difference is assumed to be zero.

The number of decimal digits is determined by default from the largest number of decimal digits of the entered descriptive statistics.  The number of decimal digits is then set at that value, plus one more.  Or, override the default with the \code{digits.d} parameter.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{ttest}}, \code{\link{t.test}}.
}

\examples{
# two-groups
# null hypothesis mean difference is zero
stats.t.test(n1=19, n2=15, m1=9.5, m2=8, s1=1.4, s2=1.5, 
           Ynm="Time", Xnm="Gender", X1nm="Men", X2nm="Women", 
           conf.level=0.95, digits.d=2)

# one-group
stats.t.test(n=25, m=350.86, s=2.66, mu0=350, Ynm="Weight")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ smd }

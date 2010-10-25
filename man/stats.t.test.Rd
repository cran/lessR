\name{stats.t.test}
\alias{stats.t.test}

\title{Generic Method for t-test and Standardized Mean Difference from Summary Statistics}

\description{
Performs the independent-groups t-test for the comparison of means from the six basic summary statistics, the sample size, sample mean and sample standard deviation for each group.  Displays the basic t-value and p-value and also many of the intermediate computations, such as the within-group or pooled standard deviation.
}

\usage{
stats.t.test(n1 = NULL, n2 = NULL,  m1 = NULL, m2 = NULL, s1 = NULL, 
    s2 = NULL, Ynm = "Y", Xnm = "X", X1nm = "Group1", X2nm = "Group2", 
    conf.level = 0.95, digits = 2, \dots)
}

\arguments{
  \item{n1}{Sample size for first group.}
  \item{n2}{Sample size for second group.}
  \item{m1}{Sample mean for first group.}
  \item{m2}{Sample mean for second group.}
  \item{s1}{Sample standard deviation for first group.}
  \item{s2}{Sample standard deviation for second group.}
  \item{Ynm}{Name of response variable.}
  \item{Xnm}{Name of predictor variable, the grouping variable or factor with exactly 
      two levels.}
  \item{X1nm}{Value of grouping variable, the level that defines the first group.}
  \item{X2nm}{Value of grouping variable, the level that defines the second group.}
  \item{conf.level}{Confidence level of the interval, expressed as a proportion.}
  \item{digits}{Number of decimal places for which to display numeric values. 
      Suggestion only.}
  \item{\dots}{Further arguments to be passed to or from methods.}
}

\details{
This function parallels the \code{\link{smd.t.test}} function included in this package, which performs the independent-groups t-test from the data.  In contrast, \code{\link{stats.t.test}} performs the analysis from the six basic summary statistics: the sample size, sample mean and sample standard deviation for each group. Accordingly, there is no graphic output from this function as with \code{\link{smd.t.test}}.

This version of \code{stats.t.test} assumes homogeneity of variance in the computation of the standard error of the mean difference. Also, only a two-sided test is provided.  The null hypothesis is a population mean difference of 0.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{smd.t.test}}, \code{\link{t.test}}.
}

\examples{
stats.t.test(n1=19, n2=15, m1=9.5, m2=8, s1=1.4, s2=1.5, 
           Ynm="Y", Xnm="X", X1nm="Group1", X2nm="Group2", 
           conf.level=0.95, digits=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ t.test }
\keyword{ smd }

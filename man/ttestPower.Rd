\name{ttestPower}
\alias{ttestPower}
\alias{ttp}

\title{Compute a Power Curve for a One or Two Group t-test}

\description{
Abbreviation: \code{ttp}

From one or two sample sizes, and either the within-cell (pooled) standard deviation, or one or two separate group standard deviations, generate and calibrate a power curve for either the one-sample t-test or the independent-groups t-test, as well as ancillary statistics.  Uses the standard R function \code{power.t.test} to calculate power and then the \code{\link{ScatterPlot}} function in this package to automatically display the annotated power curve with colors.

For both the one and two-group t-tests, power is calculated from a single sample size and single standard deviation. For the two-sample test, the within-group standard deviation is automatically calculated from the two separate group standard deviations if not provided directly. Similarly, the harmonic mean of two separate sample sizes is calculated if two separate sample sizes are provided.
}

\usage{
ttestPower(n=NULL, s=NULL, n1=NULL, n2=NULL, s1=NULL, s2=NULL, 
         mmd=NULL, msmd=NULL, mdp=.8, mu=NULL, 
         pdf_file=NULL, width=5, height=5, \ldots)

ttp(\ldots)
}


\arguments{
  \item{n}{Sample size for each of the two groups.}
  \item{s}{Within-group, or pooled, standard deviation.}
  \item{n1}{Sample size for Group 1.}
  \item{n2}{Sample size for Group 2.}
  \item{s1}{Sample standard deviation for Group 1.}
  \item{s2}{Sample standard deviation for Group 2.}
  \item{mmd}{Minimum Mean Difference of practical importance, the difference of the
       response variable between two group means. The concept is optional, and only one of \code{mmd} and \code{msmd} is provided.}
  \item{msmd}{For the Standardized Mean Difference, Cohen's \code{d}, the Minimum value of
       practical importance. The concept is optional, and only one of \code{mmd} and \code{msmd} is provided.}
  \item{mdp}{Minimum Desired Power, the smallest value of power considered to provide
       sufficient power. Default is 0.8. If changed to 0 then the concept is dropped from the analysis.}
  \item{mu}{Hypothesized mean, of which a provided value triggers a one-sample 
       analysis.}
  \item{pdf_file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\ldots}{Other parameter values, such as \code{lwd} and \code{lab_cex} from \code{\link{plot}}
        and \code{col.line} and \code{col.bg} from \code{\link{ScatterPlot}}.}
}


\details{
This function relies upon the standard \code{\link{power.t.test}} function to calibrate and then calculate the power curve according to the relevant non-central t-distribution. The \code{\link{Plot}} function from this package, which in turn relies upon the standard \code{\link{plot}} function, plots the power curve. As such, parameters in \code{\link{Plot}} for controlling the different colors and other aspects of the display are also available, as are many of the more basic parameters in the usual \code{\link{plot}} function.

Also plotted, if provided, is the minimal meaningful difference, \code{mmd}, as well as the minimal desired power, \code{mdp}, provided by default.  Relevant calculations regarding these values are also displayed at the console.  One or both concepts can be deleted from the analysis.  Not providing a value \code{mmd} implies that the concept will not be considered, and similarly for setting \code{mdp} to 0.

Invoke the function with the either the within-group (pooled) standard deviation, \code{s}, or the two separate group standard deviations, \code{s1} and \code{s2}, from which \code{s} is computed.  If the separate standard deviations are provided, then also provide the sample sizes, either as a single value of \code{n} or as two separate sample sizes, \code{n1} and \code{n2}.  If separate sample sizes \code{n1} and \code{n2} are entered, their harmonic mean serves as the value of \code{n}.

For power analysis of the two-sample t-test, the null hypothesis is a zero population mean difference.  For a one-sample test, the null hypothesis is specified, and it is this non-null specification of mu that triggers the one-sample analysis. Only non-directional or two-tailed tests are analyzed.

The  effect size that achieves a power of 0.8 is displayed. If a minimal meaningful difference, \code{mmd}, is provided, then the  associated power is also displayed, as well as the needed sample size to achieve a power of 0.8.

If the function is called with no parameter values, that is, as \code{ttp()}, then the values of \code{n1}, \code{n2} and sw must already exist before the function call.  If they do, these values are used in the power computations.  
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Plot}}, \code{\link{plot}}, \code{\link{power.t.test}}.
}
\examples{
# default power curve and colors
ttestPower(n=20, s=5)
# short name
ttp(n=20, s=5)

# default power curve and colors
# plus optional smallest meaningful effect to enhance the analysis
ttestPower(n=20, s=5, mmd=2)

# power curve from both group standard deviations and sample sizes
# also provide the minimum standardized mean difference of 
#   practical importance to obtain corresponding power
ttestPower(n1=14, n2=27, s1=4, s2=6, msmd=.5)
  
# power curve for one sample t-test, triggered by non-null mu
ttestPower(n=20, s=5, mu=30, mmd=2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ power }
\keyword{ t.test }

\name{Prop_test}
\alias{Prop_test}
\alias{prop}

\title{Analysis of Prop_test}

\description{
Abbreviation: \code{prop}

Analyze proportions.
}

\usage{
Prop_test(variable=NULL, success=NULL, by=NULL, data=d,
          n_succ=NULL, n_fail=NULL, n_tot=NULL,
          p0=NULL, digits_d=3)

prop(\ldots)
}

\arguments{
  \item{variable}{Numerical variable to analyze.}
  \item{success}{Value of \code{variable} considered a success.}
  \item{by}{Compare proportions over groups, the values of this
        categorical variable.}
  \item{data}{Data frame that contains the \code{variable} to analyze.}
  \item{n_succ}{Number of successes.}
  \item{n_fail}{Number of trials, either provide this or \code{n}.}
  \item{n_tot}{Number of trials, either provide this or \code{q}.}
  \item{p0}{Value of null hypothesized probability.}
  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{\dots}{Parameter values passed to \code{Prop_test}.} 
}


\details{
Do the test of a proportion with the exact binomial test, either by provided the \code{variable} to analyze and the \code{value} to consider a success, or provide the number of success and either the total number of trials or the number of failures.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{binom.test}}.
}

\examples{
d <- Read("Employee")
Prop_test(Gender, "F")
Prop_test(Gender, "F", by=Dept)

# Same example as for the base R binom.test
Prop_test(n_succ=682, n_fail=243, p=.75, digits_d=2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ proportion }

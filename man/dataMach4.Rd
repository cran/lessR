\name{dataMach4}
\docType{data}
\alias{dataMach4}
\title{Data: Machiavellianism}
\description{Likert data responses to Christie and Geiss's (1970) Mach~IV scale from Hunter, Gerbing and Boster (1982).

All Likert items assessed on a 6-point scale from 0: Strongly Disagree to 5: Strongly Agree. Variable labels, the item content, are included.

To construct composite scale scores, such as the Mach~IV total score, the following items should first be reverse scored: m03, m04, m06, m07, m09, m10, m11, m14, m16, m17, m19.
}

\usage{data(dataMach4)}

\format{A data table with 351 observations.

Gender, 1 column, 0:Male, 1:Female\cr
Mach IV, 20 Likert items: m01, m02, \ldots, m20, see \code{\link{dataMach4_lbl}} for the item content.
}

\source{author}

\references{
Christie, R., & Geis, F. L., (1970).  Studies in Machiavellianism.  New York:  Academic Press.

Hunter, J. E., Gerbing, D. W., and Boster, F. J. (1982). Machiavellian beliefs and personality: The construct invalidity of the Machiavellian dimension. Journal of Personality and Social Psychology, 43, 1293-1305.
}

\examples{
# Read data and variable labels (items)
d <- Read("Mach4")
l <- Read("Mach4_lbl")

# Convert to factors, i.e., categorical with value labels
d <- factors(m01:m20,
            levels=0:5,
            labels=c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree"))
}

\keyword{datasets}


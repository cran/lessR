\name{corEFA}
\alias{corEFA}
\alias{efa}

\title{Exploratory Factor Analysis and Multiple Indicator Measurement Model}

\description{
Abbreviation: \code{efa}

An exploratory factor analysis, provided by the standard R exploratory factor analysis \code{\link{factanal}}, which requires the specified number of factors as an input to the analysis. Then constructs the corresponding multiple indicator measurement model (MIMM) suggested by the exploratory factor analysis loadings, and the \code{lessR} to run the confirmatory analysis of the model.
}

\usage{
corEFA(x=mycor, n.fact, rotate=c("promax", "varimax"), min.load=.2)

efa(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{n.fact}{Number of factors.}
  \item{rotate}{Rotation method.}
  \item{min.load}{Minimum loading to include in suggested factor for
        confirmatory analysis. To ignore, set to NA.}
  \item{\dots}{Parameter values.}
}

\details{
Only the loadings from the exploratory factor analysis are provided, with either an oblique (promax), by default, or an orthogonal (varimax) rotation.  If more information is desired, run \code{\link{factanal}} directly.

Also provides the associated multiple indicator measurement model suggested by the exploratory factor analysis. Each MIMM factor is defined by the items that have the highest loading on the corresponding exploratory factor.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{cr}}.
}

\examples{
# input correlation matrix of perfect two-factor model
# Factor Pattern for each Factor: 0.8, 0.6, 0.4
# Factor-Factor correlation: 0.3
mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("V1", "V2", "V3", "V4", "V5", "V6")
rownames(mycor) <- colnames(mycor) 

# reflect all 3 indicators of the second factor
corEFA(n.fact=2)

# abbreviated form
# use all items to construct the MIMM, regardless of their loadings
efa(n.fact=2, min.load=NA)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ factor analysis }



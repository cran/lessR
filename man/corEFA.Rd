\name{corEFA}
\alias{corEFA}
\alias{efa}

\title{Exploratory Factor Analysis and Multiple Indicator Measurement Model}

\description{
Abbreviation: \code{efa}

A maximum likelihood exploratory factor analysis, provided by the standard R exploratory factor analysis \code{\link{factanal}}, which requires the specified number of factors as an input to the analysis. Then constructs the code to run the corresponding multiple indicator measurement model (MIMM) suggested by the exploratory factor analysis loadings in terms of both the \code{lessR} \code{\link{corCFA}} and the \code{cfa} function from the \code{lavaan} package.
}

\usage{
corEFA(x=mycor, n.factors, rotate=c("promax", "varimax"), 
          min.loading=.2, show.initial=FALSE, sort=TRUE, \ldots)

efa(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{n.factors}{Number of factors.}
  \item{rotate}{Rotation method.}
  \item{min.loading}{Minimum loading to include in suggested factor for
        confirmatory analysis and for the display of the loadings for the
        exploratory analysis. To ignore, set to NA.}
  \item{show.initial}{If \code{TRUE}, then display initial factor extraction
       before rotation.}
  \item{sort}{Sort the input variables by their highest factor loadings (but only
        first just list those items with loadings larger than 0.5).}
  \item{\dots}{Parameter values.}
}

\details{
Only the loadings from the exploratory factor analysis are provided, with either an oblique (promax), by default, or an orthogonal (varimax) rotation.  If more information is desired, run \code{\link{factanal}} directly.

Also provides the associated multiple indicator measurement model suggested by the exploratory factor analysis. Each MIMM factor is defined by the items that have the highest loading on the corresponding exploratory factor.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Yves Rosseel (2012). lavaan: An R Package for Structural Equation
Modeling. Journal of Statistical Software, 48(2), 1-36. URL
http://www.jstatsoft.org/v48/i02/.
}

\seealso{
\code{\link{Correlation}}.
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
colnames(mycor) <- c("X1", "X2", "X3", "X4", "X5", "X6")
rownames(mycor) <- colnames(mycor) 

# default factor analysis of default correlation matrix mycor
#   with two factors extracted
corEFA(n.factors=2)

# abbreviated form
# use all items to construct the MIMM, regardless of their loadings
# show the initial factor extraction
efa(n.factors=2, min.loading=NA, show.initial=TRUE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ factor analysis }



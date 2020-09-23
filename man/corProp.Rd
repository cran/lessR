\name{corProp}
\alias{corProp}
\alias{prop}

\title{Proportionality Coefficients from Correlations}

\description{
Abbreviation: \code{prop}

In the population, indicators of the same factor or latent variable have parallel correlations with all other variables. Of course, in the presence of sampling error, this parallelism will only be approximate. To assess this parallelism, proportionality coefficients are computed for each pair of variables in the input correlation matrix.  Also output is a heat map of the resulting matrix of proportionality coefficients. Each graph is based on a default color theme. The original default is \code{lightbronze}, but other color palettes can be generated as well.
}

\usage{
corProp(R=mycor, 
          main=NULL, heat_map=TRUE, bottom=NULL, right=NULL, 
          pdf_file=NULL, width=5, height=5, \ldots)

prop(\ldots)
}

\arguments{
  \item{R}{Correlation matrix.}
  \item{main}{Graph title. Set to \code{main=""} to turn off.}
  \item{heat_map}{If \code{TRUE}, display a heat map of the item
        correlations with the diagonal ignored.}
  \item{bottom}{Number of lines of bottom margin.}
  \item{right}{Number of lines of right margin.}
  \item{pdf_file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\dots}{Parameter values_}
}

\details{
Proportionality coefficients indicate the extent of proportionality between two variables.  Perfect proportionality of two variables is consistent with both variables being indicators of the same latent variable or factor and indicators of no other factor.

In the current version the diagonal of the input correlation matrix is ignored.  To maintain parallelism, the diagonal element of 1.00 would need to be replaced the corresponding communalities, which first requires a factor analysis.  
}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 11, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


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

# proportionality coefficients of correlation matrix mycor
# indicators of the same factor have proportional correlations
corProp()

# abbreviated form
prop()

# calculate and store proportionality coefficients in myprop
# order the proportionality coefficients to help identify factors
myprop <- corProp()
corReorder(myprop)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ proportionality }



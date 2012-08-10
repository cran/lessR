\name{corProp}
\alias{corProp}
\alias{prop}

\title{Proportionality Coefficients from Correlations}

\description{
Abbreviation: \code{prop}

Each graph is based on a default color theme. The original default is "blue", but \code{corProp} allows other color palettes to be corProp as default as well.
}

\usage{
corProp(x=mycor, 
          main=NULL, heat.map=TRUE, bottom=3,right=3, 
          colors=c("blue", "gray", "rose", "green", "gold", "red"),
          pdf.file=NULL, pdf.width=5, pdf.height=5)

prop(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{main}{Graph title. Set to \code{main=""} to turn off.}
  \item{heat.map}{If \code{TRUE}, display a heat map of the item
        correlations with item communalities in the diagonal.}
  \item{bottom}{Number of lines of bottom margin.}
  \item{right}{Number of lines of right margin.}
  \item{colors}{Sets the color palette.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Parameter values.}
}

\details{
Proportionality coefficients indicate the extent of proportionality between two variables.  Perfect proportionality of two variables is consistent with both variables being indicators of the same latent variable or factor and indicators of no other factor.
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
\keyword{ factor analysis }



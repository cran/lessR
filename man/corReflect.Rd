\name{corReflect}
\alias{corReflect}
\alias{reflect}

\title{Reflect Specified Variables in a Correlation Matrix}

\description{
Abbreviation: \code{reflect}

Reflects the specified variables by multiplying each correlation of the variable by -1.
Usually a prelude to a factor analysis, such as provided by \code{\link{corCFA}}.
}

\usage{
corReflect(x=mycor, vars,
          main=NULL, heat.map=TRUE, bottom=3,right=3, 
          colors=c("blue", "gray", "rose", "green", "gold", "red"),
          pdf.file=NULL, pdf.width=5, pdf.height=5)

reflect(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{vars}{List of the re-ordered variables, each variable listed by its ordinal
       position in the input correlation matrix.}
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
Reflects the specified variables by multiplying each correlation of the variable by -1.
The original data from which the correlations are computed is unmodified unless the output of the function is written into the input correlation matrix, by default \code{mycor}.

Specify which variables, the items, are to be reflected with a listing of each variable in terms of the ordinal position in the correlation matrix to be analyzed.
For convenience, a list of this ordering is obtained from \code{\link{corListVars}}.  For the listing of the variables, commas separate consecutive variables and the entire list is specified with the R combine or \code{\link{c}} function, preceded by \code{vars} and an equals sign. For example, if the variables to be reflected consist of the second, fifth and twenty-first variables in the input correlation matrix, then in the \code{corReflect} function call specify \code{vars=c(2,5,21)}. 
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
corReflect(vars=c(4,5,6))

# abbreviated form
reflect(vars=c(4,5,6))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }




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
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Parameter values.}
}

\details{
Reflects the specified variables by multiplying each correlation of the variable by -1.
The original data from which the correlations are computed is unmodified unless the output of the function is written into the input correlation matrix, by default \code{mycor}.

Define the constituent variables, the items, with a listing of each variable by its name in the correlation matrix. If the specified variables are in consecutive order in the input correlation matrix, the list can be specified by listing the first variable, a colon, and then the last variable.  To specify multiple variables, a single variable or a list, separate each by a comma, then invoke the R combine or \code{\link{c}} function. For example, if the list of variables in the input correlation matrix is from m02 through m05, and the variable Anxiety, then define the list in the \code{corReflect} function call according to \code{vars=c(m02:m05,Anxiety)}. 

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
corReflect(vars=c(V4,V5,V6))

# abbreviated form
reflect(vars=c(V4,V5,V6))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }





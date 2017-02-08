\name{corReorder}
\alias{corReorder}
\alias{reord}

\title{Reorder Variables in a Correlation Matrix}

\description{
Abbreviation: \code{reord}

Re-arranges the order of the variables in the input correlation matrix.  If no variable list is specified then the variables are re-ordered according to first providing the variable with the largest sum of squared correlations of all the variables, then the variable that has the highest correlation with the first variable, and so forth.
}

\usage{
corReorder(R=mycor, vars=NULL, first=0,
          heat.map=TRUE, main=NULL, bottom=3,right=3,
          pdf.file=NULL, width=5, height=5)

reord(\ldots)
}

\arguments{
  \item{R}{Correlation matrix.}
  \item{vars}{List of the re-ordered variables, each variable listed by its ordinal
       position in the input correlation matrix.}
  \item{first}{The first variable listed in the ordered matrix.}
  \item{main}{Graph title. Set to \code{main=""} to turn off.}
  \item{heat.map}{If \code{TRUE}, display a heat map of the item
        correlations with item communalities in the diagonal.}
  \item{bottom}{Number of lines of bottom margin.}
  \item{right}{Number of lines of right margin.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\dots}{Parameter values.}
}

\details{
Reorder and/or delete variables in the input correlation matrix.  

Define the constituent variables, the items, with a listing of each variable by its name in the correlation matrix. If the specified variables are in consecutive order in the input correlation matrix, the list can be specified by listing the first variable, a colon, and then the last variable.  To specify multiple variables, a single variable or a list, separate each by a comma, then invoke the R combine or \code{\link{c}} function. For example, if the list of variables in the input correlation matrix is from m02 through m05, and the variable Anxiety, then define the list in the \code{corReorder} function call according to \code{vars=c(m02:m05,Anxiety)}. 


Or, define the ordering of the variables according to the following algorithm. If no variable list is specified then the variables are re-ordered such that the first variable is that which has the largest sum of squared correlations of all the variables, then the variable that has the highest correlation with the first variable, and so forth.

In the absence of a variable list, the first variable in the re-ordered matrix can be specified with the \code{first} option.

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
colnames(mycor) <- c("V1", "V2", "V3", "V4", "V5", "V6")
rownames(mycor) <- colnames(mycor)

# leave only the 3 indicators of the second factor
#  in reverse order
#replace original mycor
mycor <- corReorder(vars=c(V6,V5,V4))

#  by their sums of squares
mynewcor <- corReorder()

# reorder the variables according to the ordering algorithm
#  with the 4th variable listed first
mynewcor <- corReorder(first=2)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }





\name{corReorder}
\alias{corReorder}
\alias{reord}

\title{Reorder Variables in a Correlation Matrix}

\description{
Abbreviation: \code{reorder}

Re-arranges the order of the variables in the input correlation matrix.  If no variable list is specified then the variables are re-ordered according to first providing the variable with the largest sum of squared correlations of all the variables, then the variable that has the highest correlation with the first variable, and so forth.
}

\usage{
corReorder(x=mycor, vars=NULL, first=0,
          heat.map=TRUE, main=NULL, bottom=3,right=3,
          colors=c("blue", "gray", "rose", "green", "gold", "red"),
          pdf.file=NULL, pdf.width=5, pdf.height=5)

reord(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{vars}{List of the re-ordered variables, each variable listed by its ordinal
       position in the input correlation matrix.}
  \item{first}{The first variable listed in the ordered matrix.}
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
Reorder and/or delete variables in the input correlation matrix.  

Explicitly specify the ordering of the specified variables, the items, with a listing of each variable in terms of the ordinal position in the correlation matrix to be analyzed. For convenience, a list of this ordering is obtained from \code{\link{corListVars}}. For the listing of the variables, commas separate consecutive variables and the entire list is specified with the R combine or \code{\link{c}} function, preceded by \code{vars} and an equals sign. For example, if the re-ordered correlation matrix consists of the second, fifth and twenty-first variables in the input correlation matrix, then in the \code{corReorder} function call specify \code{vars=c(2,5,21)}. 

Or, define the ordering of the variables according to the following algorithm. If no variable list is specified then the variables are re-ordered such that the first variable is that which has the largest sum of squared correlations of all the variables, then the variable that has the highest correlation with the first variable, and so forth.

In the absence of a variable list, the first variable in the re-ordered matrix can be specified with the \code{first} option.

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

# leave only the 3 indicators of the second factor
#  in reverse order
corReorder(vars=c(6,5,4))

# reorder the variables according to the ordering algorithm
corReorder()

# abbreviated form
# retain only the first three variables
reord(vars=c(1:3))

# reorder the variables according to the ordering algorithm
#  with the 4th variable listed first
corReorder(first=4)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }





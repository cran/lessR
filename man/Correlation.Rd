\name{Correlation}
\alias{Correlation}
\alias{cr}
\alias{cr.brief}

\title{Correlation Analysis}

\description{
Abbreviation: \code{cr, cr.brief}

For two variables the correlation coefficient with hypothesis test and confidence interval, or for a data frame, or list of variables from a data frame, the correlation matrix. The default computed coefficient(s) are the standard Pearson's product-moment correlation. For the default missing data technique of pairwise deletion, an analysis of missing data for each computed correlation coefficient is provided. For a correlation matrix a statistical summary of the missing data across all cells is provided. 
}

\usage{
Correlation(x, y, data=mydata,
         miss=c("pairwise", "listwise", "everything"),
         show.n=NULL, brief=FALSE, n.cat=getOption("n.cat"),
         digits.d=NULL, graphics=FALSE,
         main=NULL, bottom=3, right=3,
         pdf=FALSE, pdf.width=5, pdf.height=5, \ldots)

cr.brief(\ldots, brief=TRUE) 

cr(\ldots) 
}


\arguments{
  \item{x}{First variable, or list of variables for a correlation matrix.}
  \item{y}{Second variable or not specified if the first argument is a list.}
  \item{data}{Optional data frame that contains the variables of interest,
       default is \code{mydata}.}
  \item{miss}{Basis for deleting missing data values.}
  \item{show.n}{For pairwise deletion, show the matrix of sample sizes for each
       correlation coefficient, regardless of sample size.}
  \item{brief}{Peta ins to a single correlation coefficient analysis. If 
       \code{FALSE}, then the sample covariance and number of non-missing
       and missing observations are displayed.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{graphics}{If \code{TRUE}, generate a scatter plot matrix and heat map.}
  \item{main}{Graph title of heat map. Set to \code{main=""} to turn off.}
  \item{bottom}{Number of lines of bottom margin of heat map.}
  \item{right}{Number of lines of right margin of heat map.}
  \item{pdf}{If \code{TRUE}, generate scatter plot matrix and heat map
       and write to pdf files.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for internally called functions, which
       include \code{method="spearman"} and \code{method="kendall"} and also
       \code{alternative="less"} and \code{alternative="more"}.}
}


\details{
When two variables are specified, both \code{x} and y, the output is the correlation coefficient with hypothesis test, for a null hypothesis of 0, and confidence interval. Also displays the sample covariance. Based on R functions \code{\link{cor}}, \code{\link{cor.test}}, \code{\link{cov}}.

In place of two variables \code{x} and \code{y}, \code{x} can be a complete data frame, either specified with the name of a data frame, or blank to rely upon the default data frame \code{mydata}. Or, \code{x} can be a list of variables from the input data frame. In these situations \code{y} is missing. Any non-numeric variables in the data frame or specified variable list are automatically deleted from the analysis. 

Set \code{graphics=TRUE} to generate a heat map and scatter plot matrix to standard graphics windows. Set \code{pdf=TRUE} to generate these graphics but have them directed to their respective pdf files.

For treating missing data, the default is \code{pairwise}, which means that an observation is deleted only for the computation of a specific correlation coefficient if one or both variables are missing the value for the relevant variable(s).  For \code{listwise} deletion, the entire observation is deleted from the analysis if any of its data values are missing.  For the more extreme \code{everything} option, any missing data values for a variable result in all correlations for that variable reported as missing.
}

\value{
If a correlation matrix is computed, the matrix is returned. Usually assign the name of \code{mycor} to the output matrix, as in following examples. This matrix is ready for input into any of the \code{lessR} functions that analyze correlational data, including confirmatory factor analysis by \code{\link{corCFA}} and also exploratory factor analysis, either the standard R function \code{\link{factanal}} or the \code{lessR} function \code{\link{corEFA}} 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{cor.test}}, \code{\link{cov}}.
}

\examples{
# data
n <- 12
f <- sample(c("Group1","Group2"), size=n, replace=TRUE)
x1 <- round(rnorm(n=n, mean=50, sd=10), 2)
x2 <- round(rnorm(n=n, mean=50, sd=10), 2)
x3 <- round(rnorm(n=n, mean=50, sd=10), 2)
x4 <- round(rnorm(n=n, mean=50, sd=10), 2)
mydata <- data.frame(f,x1, x2, x3, x4)
rm(f); rm(x1); rm(x2); rm(x3); rm(x4)

# correlation and covariance
Correlation(x1, x2)
# short name
cr(x1, x2)
# brief form of output
cr.brief(x1, x2)

# Spearman rank correlation, one-sided test
Correlation(x1, x2, method="spearman", alternative="less")

# correlation matrix of the numerical variables in mycor
mycor <- Correlation()

# correlation matrix of Kendall's tau coefficients
mycor <- cr(method="kendall")

# correlation matrix of specified variables in mycor with graphics
mycor <- Correlation(x1:x3, graphics=TRUE)

# analysis with data not from data frame mycor
data(attitude)
mycor <- Correlation(rating, learning, data=attitude)

# analysis of entire data frame that is not mycor
data(attitude)
mycor <- Correlation(attitude)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }



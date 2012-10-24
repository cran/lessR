\name{Correlation}
\alias{Correlation}
\alias{cr}
\alias{cr.brief}

\title{Correlation Analysis}

\description{
Abbreviation: \code{cr, cr.brief}

Correlation coefficient with hypothesis test and confidence interval for two variables, or the correlation matrix for a data frame or list of variables from a data frame, generally with more than two variables. The computed coefficient(s) are the standard Pearson's product-moment correlation. For the default missing data technique of pairwise deletion, an analysis of missing data for each computed correlation coefficient is provided, and also a statistical summary of the missing data across all cells. 
}

\usage{
Correlation(x, y, dframe=mydata, 
         miss=c("pairwise", "listwise", "everything"),
         show.n=NULL, brief=FALSE, n.cat=getOption("n.cat"),
         digits.d=NULL, heat.map=TRUE, main=NULL, bottom=3, right=3,
         pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)

cr.brief(\ldots, brief=TRUE) 

cr(\ldots) 
}


\arguments{
  \item{x}{First variable.}
  \item{y}{Second variable.}
  \item{dframe}{Optional data frame that contains one or both of the variables of 
       interest, default is \code{mydata}.}
  \item{miss}{Basis for deleting missing data values.}
  \item{show.n}{For pairwise deletion, show the matrix of sample sizes for each
       correlation coefficient, regardless of sample size.}
  \item{brief}{If \code{FALSE}, then the sample covariance and number of non-missing
       and missing observations are displayed.}
  \item{n.cat}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{heat.map}{If \code{TRUE} and a matrix analyzed, displays a heat map of the
       matrix of correlation coefficients.}
  \item{main}{Graph title of heat map. Set to \code{main=""} to turn off.}
  \item{bottom}{Number of lines of bottom margin of heat map.}
  \item{right}{Number of lines of right margin of heat map.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for internally called functions.}
}


\details{
When two variables are specified, both x and y, the output is the correlation coefficient with hypothesis test, for a null hypothesis of 0, and confidence interval. Also displays the sample covariance. Based on R functions \code{\link{cor}}, \code{\link{cor.test}}, \code{\link{cov}}.

In place of two variables x and y, x can be a complete data frame, either specified with the name of a data frame, or blank to rely upon the default data frame \code{mydata}. Or, x can be a list of variables from the input data frame. In these situations \code{y} is missing. Any non-numeric variables in the data frame or specified variable list are automatically deleted from the analysis. 

The computed coefficient(s) are the standard Pearson's product-moment correlation. Use the standard R functions \code{\link{cor}} and \code{\link{cor.test}} to obtain Spearman and Kendall correlation coefficients.

For treating missing data, the default is \code{pairwise}, which means that an observation is deleted only for the computation of a specific correlation coefficient if one or both variables are missing the value for the relevant variable(s).  For \code{listwise} deletion, the entire observation is deleted from the analysis if any of its data values are missing.  For the more radical \code{everything} option, any missing data values for a variable result in all correlations for that variable reported as missing.

Text output to the console provides feedback, and the correlation matrix itself is written to a matrix called \code{mycor}, stored in the user's workspace.  This matrix is ready for input into any of the \code{lessR} functions that analyze correlational data, including confirmatory factor analysis by \code{\link{corCFA}} and also exploratory factor analysis, either the standard R function \code{\link{factanal}} or the \code{lessR} function \code{\link{corEFA}}.
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

# correlation matrix of the numerical variables in mydata
Correlation()

# correlation matrix of specified variables in mydata
Correlation(x1:x3)

# analysis with data not from data frame mydata
data(attitude)
Correlation(rating, learning, dframe=attitude)

# analysis of entire data frame that is not mydata
data(attitude)
Correlation(attitude)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }



\name{Correlation}
\alias{Correlation}
\alias{cr}
\alias{cr.brief}
\alias{cr.default}
\alias{cr.data.frame}

\title{Correlation Analysis}

\description{
Abbreviation: \code{cr, cr.brief}

Correlation coefficient with hypothesis test and confidence interval for two variables, or the correlation matrix for a data frame with more than two variables. 
}

\usage{
Correlation(x, y, dframe=mydata, \ldots)

\method{cr}{default}(x, y, dframe=mydata, brief=FALSE, \ldots) 

\method{cr}{data.frame}(x, ncut=4, digits.d=NULL, \ldots) 

cr.brief(\ldots, brief=TRUE) 

cr(\ldots) 
}


\arguments{
  \item{x}{First variable.}
  \item{y}{Second variable.}
  \item{dframe}{Optional data frame that contains one or both of the variables of interest, 
        default is \code{mydata}.}
  \item{brief}{If \code{FALSE}, then the sample covariance and number of non-missing and missing observations are displayed.}
  \item{ncut}{When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.}
  \item{digits.d}{Specifies the number of decimal digits to display in the output.}
  \item{\dots}{Other parameter values for internally called functions.}
}


\details{
Correlation coefficient with hypothesis test, for a null hypothesis of 0, and confidence interval. Also displays the sample covariance. Based on R functions \code{\link{cor}}, \code{\link{cor.test}}, \code{\link{cov}}.

In place of two variables x and y, x can be a complete data frame. All the variables in the data frame must be numeric. If not, then an appropriate error message is displayed.  Given such a data frame, the correlation matrix of all the variables in the data frame is displayed.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{cor.test}}, \code{\link{cov}}.
}

\examples{
# data
n <- 12
f <- sample(c("Group1","Group2"), size=n, replace=TRUE)
x <- round(rnorm(n=n, mean=50, sd=10), 2)
y <- round(rnorm(n=n, mean=50, sd=10), 2)
z <- round(rnorm(n=n, mean=50, sd=10), 2)
mydata <- data.frame(f,x,y,z)
rm(f); rm(x); rm(y); rm(z)

# correlation and covariance
Correlation(x,y)
# short name
cr(x,y)
# brief form of output
cr.brief(x,y)

# correlation matrix
# first remove the categorical variable f from mydata
mydata <- subset(mydata, select=c(x:z))
# now analyze remaining variables x, y and z
Correlation(mydata)

# analysis with data not from data frame mydata
data(attitude)
Correlation(rating, learning, dframe=attitude)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }



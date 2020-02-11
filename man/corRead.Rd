\name{corRead}
\alias{corRead}
\alias{rd.cor}

\title{Read Specified Correlation Matrix}

\description{
Abbreviation: \code{rd.cor}

A wrapper for base~R \code{\link{read.table}}. Read a correlation matrix into R. All coefficients for each variable must be on one physical row.  No variable names are in the file to be read.
}

\usage{
corRead(from=NULL, var_names=NULL, \dots)

rd.cor(\ldots)
}

\arguments{
  \item{from}{File reference, either omitted to browse for the data file,
        or a full path name or web URL, included in quotes.  A URL
        begins with \code{http://}.}
  \item{var_names}{The names of the variables in the matrix.}
  \item{\dots}{Parameter values for base R \code{\link{read.table}.}}
}

\details{
Read a correlation, or any square, matrix into R. All coefficients for each variable must be on one row.  No variable names are in the file to be read. The coefficients within each row, that is, for a single variable, are delimited by a white space, such as one or more blanks. 

The standard R function that reads the matrix is \code{\link{read.table}}.

By default the variables are named X1, X2, etc.  If the \code{var_names} option is invoked, then the specified names refer to the respective rows and columns of the matrix.  Here it may be convenient to name the variables with the \code{lessR} function \code{\link{to}}.

The alternative is to calculate the correlations from the data, such as with the \code{lessR} function \code{\link{Correlation}} or the standard R function \code{\link{cor}}.
}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 8, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Correlation}}, \code{\link{read.table}}.
}

\examples{
# browse for the data file because ref is omitted
# name the variables with the lessR function to
# mycor <- corRead(var_names=to("m",20))

# abbreviated form
# read a matrix with 4 variables and specify the names
# mycor <- rd.cor(var_names=c("m06","m07","m09","m10"))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }



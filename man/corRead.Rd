\name{corRead}
\alias{corRead}
\alias{rd.cor}
\alias{rad.cor}

\title{Read Specified Correlation Matrix into an R Matrix Named mycor}

\description{
Abbreviation: \code{rad.cor}

Read a correlation matrix into R. The resulting matrix is named \code{mycor}. All coefficients for each variable must be on one row.  No variable names are in the file to be read.
}

\usage{
corRead(ref=NULL, names=NULL)

rd.cor(\ldots)
rad.cor(\ldots)
}

\arguments{
  \item{ref}{File reference, either omitted to browse for the data file,
        or a full path name or web URL, included in quotes.  A URL
        begins with \code{http://}.}
  \item{names}{The names of the variables in the matrix.}
  \item{\dots}{Parameter values.}
}

\details{
Read a correlation, or any square, matrix into R. The resulting matrix is named \code{mycor}. All coefficients for each variable must be on one row.  No variable names are in the file to be read. The coefficients within each row, that is, for a single variable, are delimited by a white space, such as one or more blanks. 

The standard R function used to read the matrix is \code{\link{read.table}}.

By default the variables are named V1, V2, etc.  If the \code{names} option is invoked, then the specified names are attached to the respective rows and columns of the matrix.  Here it may be convenient to name the variables with the \code{lessR} function \code{\link{to}}.

The alternative is to calculate the correlations from the data, such as with the \code{lessR} function \code{\link{Correlation}} or the standard R function \code{\link{cor}}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Correlation}}, \code{\link{read.table}}.
}

\examples{
# browse for the data file because ref is omitted
# name the variables with the lessR function to
# corRead(names=to("m",20))

# abbreviated form
# read a matrix with 4 variables and specify the names
# rd.cor(names=c("m06","m07","m09","m10"))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }



\name{rad}
\alias{rad}

\title{Read, Attach and Display Contents of a csv File}

\description{
Reads the contents of the specified csv data file into an R dataframe called \code{mydata}.  Identifies the file by either browsing for the file on the local computer system, or as indicated by a path name or a web URL.  Then attaches and lists the first and last three rows of data as well as the variable names and the dimensions of the resulting data frame.  The purpose of \code{rad} is to save some keyboard entry, speeding up the process of reading a csv data file and reducing entry mistakes.
}

\usage{
rad(ref=NULL, display=TRUE, show.R=FALSE, ...)
}


\arguments{
  \item{ref}{File reference, either null to specify the default of 
       code{file.choose()} to browse for the csv data file, or a full path name or web URL, included in quotes.  A URL begins with \code{http://}.}
  \item{display}{The output of each step is displayed at the console, but this option can
        be turned off.}
  \item{show.R}{Display the R instructions that yielded the lessR output, albeit without
        the lessR formatting.}
  \item{...}{Other parameter values consistent with the usual \code{read.csv}
       function, such as row.names and header.}
}


\details{
One way to create a csv data file is with MS Excel or other worksheet application.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

All numeric data should be displayed in the General format, so that the only non-digit character for a numeric data value is a decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of these extra characters which R will not properly read as part of a numeric data value.

To create the csv file from a worksheet, under the File option, do a Save As and choose the csv format.

---

Given a csv data file, read the data into an R data frame called \code{mydata} with \code{rad}, which first invokes one of two R statements. 
Call with no arguments, as in \code{rad()}, to invoke:

    \code{mydata <- read.csv(file.choose())}
    
Or, call with non-null \code{ref} option, \code{rad("file_reference")}, 
to invoke:

    \code{mydata <- read.csv("file_reference")}
    
Then, \code{rad} invokes the subsequent R statements: \code{attach(mydata, warn.conflicts=FALSE)}, 
\code{head(mydata, n=3)} and \code{tail(mydata, n=3)}. Output of these statements is 
directed to the console.  Also listed are the R statements invoked by \code{rad}.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{read.csv}}, \code{\link{attach}}, \code{\link{head}}, \code{\link{tail}}.
}

\examples{
# to browse for a csv data file on the computer system, invoke rad with 
# the ref argument empty, which, in turn, invokes read.csv(file.choose()),
# and then automatically invokes the attach, head and tail statements
# rad()

# same as above, but include standard read.csv options to indicate 
#  no variable names in first row of the csv data file 
#   and then provide the names
# also indicate that the first column is an ID field
# rad(header=FALSE, col.names=c("X", "Y"), row.names=1)

# read a csv data file from the web
# then attach and list variable names and some values
rad("http://web.pdx.edu/~gerbing/data/twogroup.csv")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }

\name{rad}
\alias{rad}

\title{Read, Attach and Display Contents of a csv File}

\description{
Reads the contents of the specified csv data file into an R dataframe called \code{mydata}.  Identifies the file by either browsing for the file on the local computer system with \code{rad()}, or as indicated by a path name or a web URL.  Then attaches and lists the first and last three rows of data as well as the variable names and the dimensions of the resulting data frame.  In addition, perform an analysis of missing data, listing the number of missing values for each variable and for each observation.
}

\usage{
rad(ref=NULL, display=TRUE, show.R=FALSE, no.attach=FALSE, 
         n.cut=1, miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
         format=c("csv", "SPSS"), \dots)
}


\arguments{
  \item{ref}{File reference, either null to specify the default of 
       \code{file.choose()} to browse for the csv data file, or a full path name or web URL, included in quotes.  A URL begins with \code{http://}.}
  \item{display}{The output of each step is displayed at the console, but this option can
        be turned off.}
  \item{show.R}{Display the R instructions that yielded the lessR output, albeit without
        the lessR formatting.}
  \item{no.attach}{Off by default, but if \code{TRUE} then do not attach the R data frame, 
        \code{mydata} by default.}
  \item{n.cut}{For the missing value analysis, list the row name and number of missing values
        if the number of missing exceeds or equals this cutoff.}
  \item{miss.show}{For the missing value analysis, the number of rows, one row per observation,
        that has as many or missing values as \code{n.cut}.}
  \item{miss.zero}{For the missing value analysis, list the variable name or the row name
        even for values of 0. By default only variables and rows with missing data are listed.}
  \item{miss.matrix}{For the missing value analysis, if there is any missing data, 
        list a version of the complete data table with a 0 for a non-missing value and a 1 for a missing value.}
  \item{format}{Format of the data in the file, which by default is a \code{csv} file, and as option can be an SPSS \code{sav} file.}
  \item{...}{Other parameter values consistent with the usual \code{read.csv}
       function, such as row.names and header.}
}


\details{
CREATE csv FILE\cr
By default \code{rad} reads \code{csv} data files. One way to create a csv data file is by entering the data into a text editor. A more structured method is to use a worksheet application such as MS Excel, LibreOffice Calc.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

All numeric data in the worksheet should be displayed in the General format, so that the only non-digit character for a numeric data value is a decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of these extra characters which R will not properly read as part of a numeric data value.

To create the csv file from Excel, under the File option, do a Save As and choose the csv format.

MECHANICS\cr
Given a csv data file, read the data into an R data frame called \code{mydata} with \code{rad}, which first invokes one of two R statements. 
Call with no arguments, as in \code{rad()}, to invoke:

    \code{mydata <- read.csv(file.choose())}
    
Or, call with non-null \code{ref} option, \code{rad("file_reference")}, 
to invoke:

    \code{mydata <- read.csv("file_reference")}
    
Then, \code{rad} invokes the subsequent R statements: \code{attach(mydata, warn.conflicts=FALSE)}, 
\code{head(mydata, n=3)} and \code{tail(mydata, n=3)}. Output of these statements is 
directed to the console.  Also listed are the R statements invoked by \code{rad}.

Because \code{rad} calls the standard R function \code{\link{read.csv}}, which just provides a wrapper for \code{\link{read.table}}, the usual options that work with \code{\link{read.table}}, such as \code{row.names} also can be passed through \code{rad}. 

SPSS DATA\cr
To read data in the SPSS \code{.sav} format, \code{rad} calls the \code{read.spss} function from the \code{foreign} package. To invoke this option, specify \code{format="SPSS"}.

MISSING DATA\cr
By default, \code{rad} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. When reading the data, R automatically sets any empty numeric and logical values as missing.  Null character string values are left as null, unless the R \code{na.strings=""} option is invoked. Of course any other valid value can be set to missing as well. 

To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss.zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss.matrix=TRUE} option. 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

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
# rad("http://web.pdx.edu/~gerbing/data/twogroup.csv")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }

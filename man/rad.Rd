\name{rad}
\alias{rad}
\alias{rad.brief}
\alias{rad.labels}
\alias{rad.both}
\alias{rad2}
\alias{rad}

\title{Read, Attach and Display Contents of a Data File}

\description{
Reads the contents of the specified data file and/or variable labels into an R data frame, such as in standard \code{csv}.  For the data this data frame is called \code{mydata}, and for the variable labels it is called \code{mylabels}.  Identify the file by either browsing for the file on the local computer system with \code{rad()}, or as indicated by an argument as a character string in the form of a path name or a web URL.  The function also lists the first and last three rows of data as well as the variable names and the dimensions of the resulting data frame and the data type for each variable.  In addition, perform an analysis of missing data, listing the number of missing values for each variable and for each observation.
}

\usage{
rad(ref=NULL, brief=FALSE, show.R=FALSE, attach=FALSE,
         n.mcut=1, miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
         format=c("csv", "SPSS"), data=TRUE, labels=FALSE, 
         missing="", max.lines=30, \dots)

rad.brief(ref=NULL, brief=TRUE, \ldots) 

rad.labels(ref=NULL, data=FALSE, labels=TRUE, \ldots) 

rad.both(ref=NULL, data=TRUE, labels=TRUE, \ldots) 

rad2(ref=NULL, sep=";", dec=",", \ldots)
}


\arguments{
  \item{ref}{File reference, either null to specify the default of 
       \code{file.choose()} to browse for the csv data file, or a full path name or web URL, 
       included in quotes.  A URL begins with \code{http://}.}
  \item{brief}{Extent of the results.}
  \item{show.R}{Display the R instructions that yielded the lessR output, albeit without
        the lessR formatting.}
  \item{attach}{Do not attach data set by default, \code{mydata} by default.}
  \item{n.mcut}{For the missing value analysis, list the row name and number of missing values
        if the number of missing exceeds or equals this cutoff.}
  \item{miss.show}{For the missing value analysis, the number of rows, one row per observation,
        that has as many or missing values as \code{n.mcut}.}
  \item{miss.zero}{For the missing value analysis, list the variable name or the row name
        even for values of 0. By default only variables and rows with missing data are listed.}
  \item{miss.matrix}{For the missing value analysis, if there is any missing data, 
        list a version of the complete data table with a 0 for a non-missing value and a 1 for 
        a missing value.}
  \item{format}{Format of the data in the file, which by default is a \code{csv} file, and as 
        option can be an SPSS \code{sav} file, which also reads the variable labels if present.
        Also set to \code{TRUE} if the file to be read has a file type of \code{.sav}.}
  \item{data}{If \code{TRUE}, then read data, otherwise only variable labels are read.}
  \item{labels}{If \code{TRUE}, then the second row of information in the data file, after the 
        variable names in the first row, consists of the variable lables. Data begins on the
        third row.}
  \item{sep}{Character that separates adjacent values in a text file of data.}
  \item{dec}{Character that serves as the decimal separator in a number.}
  \item{missing}{Missing value code, which by default is literally a missing data value in the
        data table.}
  \item{max.lines}{Maximum number of lines to list of the data and labels.}
  \item{...}{Other parameter values consistent with the usual \code{read.table}
       function, such as sep, row.names and header.}
}


\details{
CREATE csv FILE\cr
By default \code{rad} reads \code{csv} data files. One way to create a csv data file is by entering the data into a text editor. A more structured method is to use a worksheet application such as MS Excel, LibreOffice Calc.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

All numeric data in the worksheet should be displayed in the General format, so that the only non-digit character for a numeric data value is a decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of these extra characters which R will not properly read as part of a numeric data value.

To create the csv file from a standard worksheet application such as Microsoft Excel or LibreOffice Calc, first convert any numeric data to general format to remove characters such as dollar signs and commas, and then under the File option, do a Save As and choose the csv format.

Invoke the \code{sep=""} option to read tab-delimited data. Do \code{help(read.table)} to view the other options that can also be implemented from \code{rad}.

MECHANICS\cr

Specify the file as with the \code{\link{rad}} function for reading the data into a data frame.  If no arguments are passed to the function, then interactively browse for the file.  Or, enclose within quotes a full path name or a URL for reading the labels on the web.

Given a csv data file, read the data into an R data frame called \code{mydata} with \code{rad}, which first invokes one of two R statements. 
Call with no arguments, as in \code{rad()}, to invoke:

    \code{mydata <- read.csv(file.choose())}
    
Or, call with non-null \code{ref} option, \code{rad("file_reference")}, 
to invoke:

    \code{mydata <- read.csv("file_reference")}
    
Then, \code{rad} invokes the subsequent R statements:  
\code{head(mydata, n=3)} and \code{tail(mydata, n=3)}. Output of these statements is 

Because \code{rad} calls the standard R function \code{\link{read.csv}}, which just provides a wrapper for \code{\link{read.table}}, the usual options that work with \code{\link{read.table}}, such as \code{row.names} also can be passed through \code{rad}. 

SPSS DATA\cr
To read data in the SPSS \code{.sav} format, \code{rad} calls the \code{read.spss} function from the \code{foreign} package. To invoke this option, specify \code{format="SPSS"}. If the file has a file type of \code{.sav}, that is, the file specification ends in \code{.sav}, then the \code{format} is automatically set to \code{"SPSS"}.

MISSING DATA\cr
By default, \code{rad} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. When reading the data, \code{rad} automatically sets any empty values as missing.  Note that this is different from the R default in \code{\link{read.table}} in which an empty value for character string variables are treated as a regular data value. Any other valid value for any data type can be set to missing as well with the \code{missing} option. To mimic the standard R default for missing character values, set \code{missing=NA}. 

To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss.zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss.matrix=TRUE} option. 

VARIABLE LABELS\cr
Standard R does not provide for variable labels, but \code{lessR} provides for a data frame called \code{mylabels} which stores variable labels.  A labels data frame can list the label for some or all of the variables in the data frame that contains the data for the analysis.  One way to enter the variable labels is to read them from their own file with the \code{{rad.labels}} version of the \code{rad} function, which sets \code{labels=TRUE} and \code{data=FALSE}.  Another way is to include the labels directly in the data file, as the second row of information, after the variable names in the first row and before the first row of data, in the third row of the file.  To to this, set \code{lables=TRUE}, or, equivantley, invoke the \code{rad.both} version of \code{rad}, which reads both the variable labels and the data from the same file. The web survey application Qualtrics downloads \code{csv} files in this format.

The \code{lessR} functions that provide analysis, such as \code{\link{hst}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{\link{label}} function, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  

For a file that contains only labels, each row of the file, including the first row, consists of the variable name, a comma, and then the label, that is, standard \code{csv} format such as obtained with the \code{csv} optiion from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, mydata, need have a label, and the variables with their corresponding labels can be listed in any order. An example follows.

I2,"This instructor presents material in a clear and organized manner."\cr
I4,"Overall, this instructor was highly effective in this class."\cr
I1,"This instructor has command of the subject."\cr
I3,"This instructor relates course materials to real world situations."\cr

The quotes here are not needed because there are no commas in the enclosed character strings.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{read.csv}}, \code{\link{attach}}, \code{\link{head}}, \code{\link{tail}}.
}

\examples{
# remove the # sign before each of the following rad statements to run

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
# rad("http://web.pdx.edu/~gerbing/data/twogroup.csv")

# read a csv data file with -99 and XXX set to missing
# rad(missing=c(-99, "XXX"))

# do not display any output
# rad.brief()

# read tab-delimited (or any other white-space) data
# rad(sep="")

# read variable labels only, no data 
# rad.labels()

# read data and variable labels
# rad.both()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }

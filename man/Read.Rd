\name{Read}
\alias{Read}
\alias{rad}
\alias{rad2}
\alias{rad.brief}
\alias{rad.labels}
\alias{rad.both}

\title{Read and Display Contents of a Data File and Optional Variable Labels}

\description{
Abbreviation: \code{rad}, \code{rad2}, \code{rad.brief}, \code{rad.labels}, \code{rad.both}

Reads the contents of the specified data file and/or variable labels into an R data frame, by default \code{mydata}. The format of the file can be standard \code{csv} data file, a fixed width formatted data file, or native SPSS or R data file.  Any optional variable labels are read into a data frame called \code{mylabels}.  Identify the file by either browsing for the file on the local computer system with \code{Read()}, or as indicated by an argument as a character string in the form of a path name or a web URL.  The function also lists the first and last three rows of data as well as the variable names and the dimensions of the resulting data frame and the data type for each variable.  In addition, perform an analysis of missing data, listing the number of missing values for each variable and for each observation.

Also see the \code{lessR} function \code{\link{corRead}} and its alternate form \code{\link{rad.cor}} to read a correlation matrix.
}

\usage{
Read(ref=NULL, lessR.data=NULL, brief=FALSE, n.mcut=1,
         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
         format=c("csv", "SPSS", "R"), data=TRUE, labels=FALSE, 
         missing="", max.lines=30, widths=NULL, dframe=mydata, \dots)

rad2(\ldots, sep=";", dec=",")

rad(\ldots) 

rad.brief(\ldots, brief=TRUE) 

rad.labels(\ldots, data=FALSE, labels=TRUE) 

rad.both(\ldots, data=TRUE, labels=TRUE) 
}


\arguments{
  \item{ref}{File reference, either omitted to browse for the data file, or a full path name or web URL, 
       included in quotes.  A URL begins with \code{http://}.}
  \item{lessR.data}{The name of the data set included with lessR.}
  \item{brief}{Extent of the results.}
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
        an option can be an SPSS \code{.sav} file, which also reads the variable labels if present,
        or a native R data file with a file type of \code{.rda}.}
  \item{data}{If \code{TRUE}, then read data, otherwise only variable labels are read.}
  \item{labels}{If \code{TRUE}, then the second row of information of a \code{csv} data file,
        after the variable names in the first row, consists of the variable labels. Data begins
        on the third row.}
  \item{sep}{Character that separates adjacent values in a text file of data.}
  \item{dec}{Character that serves as the decimal separator in a number.}
  \item{missing}{Missing value code, which by default is literally a missing data value in the
        data table.}
  \item{max.lines}{Maximum number of lines to list of the data and labels.}
  \item{widths}{Specifies the width of the successive columns for fixed width formatted data.}
  \item{dframe}{The name of the data frame that will contain the data read from the 
       external data file.}
  \item{...}{Other parameter values consistent with the usual \code{\link{read.table}}
       function, such as sep, row.names and header.}
}


\details{
CREATE csv FILE\cr
By default \code{Read} reads \code{csv} data files, native R files with file type of \code{.rda} and native SPSS files with file type \code{.sav}. One way to create a csv data file is by entering the data into a text editor. A more structured method is to use a worksheet application such as MS Excel, LibreOffice Calc.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

All numeric data in the worksheet should be displayed in the General format, so that the only non-digit character for a numeric data value is a decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of these extra characters which R will not properly read as part of a numeric data value.

To create the csv file from a standard worksheet application such as Microsoft Excel or LibreOffice Calc, first convert any numeric data to general format to remove characters such as dollar signs and commas, and then under the File option, do a Save As and choose the csv format.

Invoke the \code{sep=""} option to read tab-delimited data. Do \code{help(read.table)} to view the other options that can also be implemented from \code{Read}.

MECHANICS\cr
Specify the file as with the \code{\link{Read}} function for reading the data into a data frame.  If no arguments are passed to the function, then interactively browse for the file.  Or, enclose within quotes a full path name or a URL for reading the labels on the web.

Given a csv data file, read the data into an R data frame called \code{mydata} with \code{Read}. Because \code{Read} calls the standard R function \code{\link{read.csv}}, which just provides a wrapper for \code{\link{read.table}}, the usual options that work with \code{\link{read.table}}, such as \code{row.names} also can be passed through \code{Read}. 

SPSS DATA\cr
To read data in the SPSS \code{.sav} format, \code{Read} calls the \code{\link{read.spss}} function from the \code{foreign} package.  If the file has a file type of \code{.sav}, that is, the file specification ends in \code{.sav}, then the \code{format} is automatically set to \code{"SPSS"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="SPSS"}.

R DATA\cr
By convention only, data files in native R format have a file type of \code{.rda}. To read a native R data file, if the file type is \code{.rda}, the \code{format} is automatically set to \code{"R"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="R"}. Create a native R data file by saving the current data frame, usually \code{mydata}, with the \code{lessR} function \code{\link{Write}}. 

When read back into a working R session, the data is restored as the complete data frame of the same name from which it was saved. However, if the data frame that was saved was not \code{mydata}, then the \code{dframe} option must be invoked to inform \code{Read} where the name of the data frame. Without this information, the data frame will still be created, but the program will terminate early without providing user feedback.

lessR DATA\cr
\code{lessR} has some data sets included with the package.  \code{Read} will read each such data set by specifying its name with the \code{lessR.data} parameter. Also, each included data set begins with the prefix \code{dat}, which can be deleted when specifying the name of the data set. This option is a replacement for the standard R \code{\link{data}} function, offering the added information provided by \code{Read} and the automatic creation of the \code{mydata} data frame consistent with the other uses of \code{Read}.

FIXED WIDTH FORMATTED DATA\cr
Sometimes the width of the columns are the same for all the data values of a variable, such as a data file of Likert scale responses from 1 to 5 on a 50 survey items such that the data consist of 50 columns with no spaces or other delimiter to separate adjacent data values. To read this data, based upon the R function \code{\link{read.fwf}}, invoke the \code{widths} option of that function.  

MISSING DATA\cr
By default, \code{Read} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. When reading the data, \code{Read} automatically sets any empty values as missing.  Note that this is different from the R default in \code{\link{read.table}} in which an empty value for character string variables are treated as a regular data value. Any other valid value for any data type can be set to missing as well with the \code{missing} option. To mimic the standard R default for missing character values, set \code{missing=NA}. 

To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss.zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss.matrix=TRUE} option. 

VARIABLE LABELS\cr
Standard R does not provide for variable labels, but \code{lessR} provides for a data frame called \code{mylabels} which stores variable labels.  A labels data frame can list the label for some or all of the variables in the data frame that contains the data for the analysis.  One way to enter the variable labels is to read them from their own file with \code{Read} with \code{labels=TRUE} and \code{data=FALSE}, or with the short form \code{{rad.labels}}.  Another way is to include the labels directly in the data file, as the second row of information, after the variable names in the first row and before the first row of data, in the third row of the file.  To to this, set \code{labels=TRUE}, or, equivalently, invoke the short form \code{rad.both} , which reads both the variable labels and the data from the same file. The web survey application Qualtrics downloads \code{csv} files in this format.

The \code{lessR} functions that provide analysis, such as \code{\link{hst}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{\link{label}} function, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  

For a file that contains only labels, each row of the file, including the first row, consists of the variable name, a comma, and then the label, that is, standard \code{csv} format such as obtained with the \code{csv} optiion from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, \code{mydata} by default, need have a label, and the variables with their corresponding labels can be listed in any order. An example follows.

I2,"This instructor presents material in a clear and organized manner."\cr
I4,"Overall, this instructor was highly effective in this class."\cr
I1,"This instructor has command of the subject."\cr
I3,"This instructor relates course materials to real world situations."\cr

The quotes here are not needed because there are no commas in the enclosed character strings.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{read.csv}},\code{\link{read.spss}},\code{\link{read.fwf}}, 
\code{\link{corRead}}.
}

\examples{
# remove the # sign before each of the following Read statements to run

# to browse for a csv data file on the computer system, invoke Read with 
# the ref argument empty, which, in turn, invokes read.csv(file.choose()),
# and then automatically invokes the attach, head and tail statements
# Read()
# short name
# rad()

# same as above, but include standard read.csv options to indicate 
#  no variable names in first row of the csv data file 
#   and then provide the names
# also indicate that the first column is an ID field
# Read(header=FALSE, col.names=c("X", "Y"), row.names=1)

# read a csv data file from the web
# Read("http://web.pdx.edu/~gerbing/data/twogroup.csv")

# read a csv data file with -99 and XXX set to missing
# Read(missing=c(-99, "XXX"))

# do not display any output
# Read.brief()

# read tab-delimited (or any other white-space) data
# Read(sep="")

# read into a data frame called good.data instead of mydata
# Read(dframe=good.data)

# read the built-in data set datEmployee
Read(lessR.data="Employee")

# read variable labels only, no data 
# Read.labels()

# read data and variable labels
# Read.both()

# read a data file that consists of a 
#   5 column ID field, 2 column Age field
#   and 75 single columns of data, no spaces between columns
#   name the variables with lessR function: to
#   the variable names are Q01, Q02, ..., Q74, Q75
# Read(widths=c(5,2,rep(1,75)), col.names=c("ID", "Age", to("Q", 75)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }



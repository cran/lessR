\name{Read}
\alias{Read}
\alias{rd}
\alias{Read2}

\title{Read and Display Contents of a Data File and Optional Variable Labels}

\description{
Abbreviation: \code{rd}, \code{Read2}

Reads the contents of the specified data file with optional variable labels into an R data table (frame). The format of the file can be standard \code{csv} data file with any delimiter including the default comma, a fixed width formatted data file, or native SPSS or R data file.  Identify the file by either browsing for the file on the local computer system with \code{Read()}, or indicate with an argument as a character string in the form of a path name or a web URL.  The function also provides feedback regarding the data that was read which includes the variable names, the dimensions of the resulting data frame, the data type for each variable, and the values of the variables in the data file for the first and last rows of the data. In addition, an analysis of missing data is provided, listing the number of missing values for each variable and for each observation.

Also see the \code{lessR} function \code{\link{corRead}} to read a correlation matrix.
}

\usage{
Read(ref=NULL, format=c("csv", "SPSS", "R", "lessR"),

         labels=NULL, widths=NULL, missing="", n.mcut=1, 

         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
      
         max.lines=30, quiet=getOption("quiet"), \dots)

rd(\ldots) 

Read2(\ldots, sep=";", dec=",")
}


\arguments{
  \item{ref}{File reference, either omitted to browse for the data file, or a full path name
       or web URL, included in quotes.  A URL begins with \code{http://}.}
  \item{format}{Format of the data in the file, which by default is a \code{csv} file, and as 
        an option can be an SPSS \code{.sav} file, which also reads the variable labels if
        present, or a native R data file with a file type of \code{.rda}, or a (native R) data
        file part of \code{lessR}.}
  \item{labels}{File name for the file of variable labels. Either a full path name, or just
       the file name if in the same directory as the data file. Or, if \code{row2}, then
       the labels are in the second line of the data file.}
  \item{widths}{Specifies the width of the successive columns for fixed width formatted data.}
  \item{missing}{Missing value code, which by default is literally a missing data value in the
        data table.}
  \item{n.mcut}{For the missing value analysis, list the row name and number of missing values
        if the number of missing exceeds or equals this cutoff.}
  \item{miss.show}{For the missing value analysis, the number of rows, one row per observation,
        that has as many or missing values as \code{n.mcut}.}
  \item{miss.zero}{For the missing value analysis, list the variable name or the row name
        even for values of 0. By default only variables and rows with missing data are listed.}
  \item{miss.matrix}{For the missing value analysis, if there is any missing data, 
        list a version of the complete data table with a 0 for a non-missing value and a 1 for 
        a missing value.}
  \item{sep}{Character that separates adjacent values in a text file of data.}
  \item{dec}{Character that serves as the decimal separator in a number.}
  \item{max.lines}{Maximum number of lines to list of the data and labels.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{set}} function.}
  \item{...}{Other parameter values consistent with the usual \code{\link{read.table}}
       function, such as row.names and header.}
}


\details{
CREATE csv FILE\cr
By default \code{Read} reads \code{csv} data files, native R files with file type of \code{.rda} and native SPSS files with file type \code{.sav}. Invoke the \code{widths} option to allow for the reading of fixed width formatted data. Calls the \code{lessR} function \code{\link{details}} to provide the feedback regarding details of the data frame that was read.

One way to create a csv data file is by entering the data into a text editor. A more structured method is to use a worksheet application such as MS Excel, LibreOffice Calc.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

All numeric data in the worksheet should be displayed in the General format, so that the only non-digit character for a numeric data value is a decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of these extra characters which R will not properly read as part of a numeric data value.

To create the csv file from a standard worksheet application such as Microsoft Excel or LibreOffice Calc, first convert any numeric data to general format to remove characters such as dollar signs and commas, and then under the File option, do a Save As and choose the csv format.

Invoke the \code{sep=""} option to read tab-delimited data. Do \code{help(read.table)} to view the other options that can also be implemented from \code{Read}.

MECHANICS\cr
Specify the file as with the \code{\link{Read}} function for reading the data into a data frame.  If no arguments are passed to the function, then interactively browse for the file.  Or, enclose within quotes a full path name or a URL for reading the labels on the web.

Given a csv data file, read the data into an R data frame called \code{mydata} with \code{Read}. Because \code{Read} calls the standard R function \code{\link{read.csv}}, which just provides a wrapper for \code{\link{read.table}}, the usual options that work with \code{\link{read.table}}, such as \code{row.names} also can be passed through \code{Read}. 

SPSS DATA\cr
To read data in the SPSS \code{.sav} format, \code{Read} calls the \code{\link{read.spss}} function from the \code{foreign} package.  If the file has a file type of \code{.sav}, that is, the file specification ends in \code{.sav}, then the \code{format} is automatically set to \code{"SPSS"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="SPSS"}. Any variable labels in the SPSS file are read and stored in the resulting \code{R} data table (frame).

R DATA\cr
By convention only, data files in native R format have a file type of \code{.rda}. To read a native R data file, if the file type is \code{.rda}, the \code{format} is automatically set to \code{"R"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="R"}. Create a native R data file by saving the current data frame, usually \code{mydata}, with the \code{lessR} function \code{\link{Write}}. 

lessR DATA\cr
\code{lessR} has some data sets included with the package.  \code{Read} will read each such data set by specifying its name and setting \code{format="lessR"}. Also, each included data set begins with the prefix \code{dat}, which can be deleted when specifying the name of the data set. This option is a replacement for the standard R \code{\link{data}} function, offering the added information provided by \code{Read}.

FIXED WIDTH FORMATTED DATA\cr
Sometimes the width of the columns are the same for all the data values of a variable, such as a data file of Likert scale responses from 1 to 5 on a 50 item survey such that the data consist of 50 columns with no spaces or other delimiter to separate adjacent data values. To read this data set, based upon the R function \code{\link{read.fwf}}, invoke the \code{widths} option of that function.  

MISSING DATA\cr
By default, \code{Read} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. When reading the data, \code{Read} automatically sets any empty values as missing.  Note that this is different from the R default in \code{\link{read.table}} in which an empty value for character string variables are treated as a regular data value. Any other valid value for any data type can be set to missing as well with the \code{missing} option. To mimic the standard R default for missing character values, set \code{missing=NA}. 

To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss.zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss.matrix=TRUE} option. 

VARIABLE LABELS\cr
Standard R does not provide for variable labels, but \code{lessR} does. Variable labels can be provided for some or all of the variables in the data frames.  One way to enter the variable labels is to read them from their own file with \code{Read} with \code{labels} set to the full path name or \code{URL} of the labels file, or just the file name if the labels file is in the same directory as the data file.  Another method is to include the labels directly in the data file.  To to this, specify the file of variable labels with the \code{label="row2"} option. The web survey application Qualtrics downloads \code{csv} files in this format.

For a file that contains only labels, each row of the file, including the first row, consists of the variable name, a comma, and then the label, that is, standard \code{csv} format such as obtained with the \code{csv} option from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, usually \code{mydata}, need have a label, and the variables with their corresponding labels can be listed in any order. An example follows.

I2,This instructor presents material in a clear and organized manner.\cr
I4,Overall, this instructor was highly effective in this class.\cr
I1,This instructor has command of the subject.\cr
I3,This instructor relates course materials to real world situations.\cr

If there is a comma in the variable label, then the label needs to be enclosed in quotes.

The \code{lessR} functions that provide analysis, such as \code{\link{Histogram}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{\link{label}} function, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  
}

\value{
The read data frame is returned, usually assigned the name of \code{mydata} as in the examples below.  This is the default name for the data frame input into the lessR data analysis functions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{read.csv}},\code{\link{read.spss}},\code{\link{read.fwf}}, 
\code{\link{corRead}}, \code{\link{details}}.
}

\examples{
# remove the # sign before each of the following Read statements to run

# to browse for a csv data file on the computer system, invoke Read with 
#   the ref argument empty
# mydata <- Read()
# abbreviated name
# mydata <- rd()

# same as above, but include standard read.csv options to indicate 
#  no variable names in first row of the csv data file 
#   and then provide the names
# also indicate that the first column is an ID field
# mydata <- Read(header=FALSE, col.names=c("X", "Y"), row.names=1)

# read a csv data file from the web
# mydata <- Read("http://web.pdx.edu/~gerbing/data/twogroup.csv")

# read a csv data file with -99 and XXX set to missing
# mydata <- Read(missing=c(-99, "XXX"))

# do not display any output
# mydata <- rd.quiet()

# read tab-delimited (or any other white-space) data
# mydata <- Read(sep="")

# read the built-in data set datEmployee
mydata <- Read("Employee", format="lessR")

# read a data file that consists of a 
#   5 column ID field, 2 column Age field
#   and 75 single columns of data, no spaces between columns
#   name the variables with lessR function: to
#   the variable names are Q01, Q02, ..., Q74, Q75
# mydata <- Read(widths=c(5,2,rep(1,75)), col.names=c("ID", "Age", to("Q", 75)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }





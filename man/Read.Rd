\name{Read}
\alias{Read}
\alias{rd}
\alias{rd.brief}
\alias{Read2}

\title{Read Contents of a Data File with Optional Variable Labels and Feedback}

\description{
Abbreviation: \code{rd},  \code{rd.brief}, \code{Read2}

Reads the contents of the specified data file with optional variable labels into an R data table (frame). By default the format of the file is detected from its filetype: comma or tab separated value text file from \code{.csv}, SPSS data file from \code{.sav}, SAS data from from \code{.sas7bdat}, or R data file from \code{.rda}, and Excel file from \code{.xls} or \code{.xlsx} using the \code{gdata} package.  If no filetype is recognized then Read defaults to reading a comma separated or tab-limited text data file.  Specify a fixed width formatted text data file to be read with the required R \code{widths} option. Identify the data file by either browsing for the file on the local computer system with \code{Read()}, or identify the file with the first argument a character string in the form of a path name or a web URL (except for .Rda files which must be on the local computer system).

Variable labels can be added to the data table when reading a text file. Any variable labels in a native SPSS of native R file are automatically included. See the \code{details} section below for more information. Variable labels can also be added and modified individually with the \code{lessR} function \code{\link{label}}, and more comprehensively with the \code{\link{VariableLabels}} function.

The function also provides feedback regarding the data that is read, which includes the variable names, the dimensions of the resulting data frame, the data type for each variable, and the values of the variables in the data file for the first and last rows of the data. In addition, an analysis of missing data is provided, listing the number of missing values for each variable and for each observation. The brief form just lists the input files, the variable name table, and any variable labels.

The \code{lessR} function \code{\link{corRead}} reads a correlation matrix.
}

\usage{
Read(ref=NULL, format=c("csv", "SPSS", "R", "Excel", "SAS", "lessR"),

         labels=NULL, widths=NULL, missing="", n.mcut=1, 

         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE, 
      
         max.lines=30, sheet=1,

         brief=TRUE, quiet=getOption("quiet"),

         fun.call=NULL, \dots)

rd(\ldots) 

rd.brief(\ldots, brief=TRUE)

Read2(\ldots, sep=";", dec=",")
}


\arguments{
  \item{ref}{File reference, either omitted to browse for the data file, or (except for .Rda
       files) a full path name or web URL, included in quotes.  A URL begins with
       \code{http://}.}
  \item{format}{Format of the data in the file, which by default is a \code{csv} file, which
        also will recognize tab-delimited text. As an option can be an Excel \code{.xls} or
        \code{.xlsx} file or an SPSS \code{.sav} file, which also reads the variable labels
        if present, or a native R data file with a file type of \code{.rda}, or a (native R)
        data file part of \code{lessR}.}
  \item{labels}{File name for the file of variable labels. Either a full path name, or just
       the file name if in the same directory as the data file, or no reference between the 
       quotes, which allows the user to browse for the labels file. Or, if \code{row2}, then
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
  \item{sheet}{For Excel files, specifies the work sheet to read. The default is the
        first work sheet.}
  \item{brief}{If \code{TRUE}, display only variable names table plus any variable labels.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change the corresponding system
       default with \code{\link{theme}} function.}
  \item{fun.call}{Function call. Used with \code{Rmd} to pass the function call when
        obtained from the abbreviated function call \code{rd}.}
  \item{...}{Other parameter values define with the R read functions, such as the
       \code{read.table} function for text files, with row.names and header.}
}


\details{
By default \code{Read} reads text data files which are either comma delimited, \code{csv}, or tab-dilimited data files, native Excel files of type \code{.xls} or \code{.xlsx}, native R files with file type of \code{.rda}, native SAS files with file type \code{.sas7bdat}, and native SPSS files with file type \code{.sav}. Invoke the \code{widths} option to allow for the reading of fixed width formatted data. Calls the \code{lessR} function \code{\link{details}} to provide feedback regarding details of the data frame that was read. Variables defined by non-numeric variables are read as factors if their values are not unique, otherwise they are read as character strings.

CREATE csv FILE\cr
One way to create a csv data file is to enter the data into a text editor. A more structured method is to use a worksheet application such as MS Excel, LibreOffice Calc.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

All numeric data in the worksheet should be displayed in the General format, so that the only non-digit character for a numeric data value is a decimal point.  The General format removes all dollar signs and commas, for example, leaving only the pure number, stripped of these extra characters which R will not properly read as part of a numeric data value.

To create the csv file from a standard worksheet application such as Microsoft Excel or LibreOffice Calc, first convert any numeric data to general format to remove characters such as dollar signs and commas, and then under the File option, do a Save As and choose the csv format.

Call \code{help(read.table)} to view the other options that can also be implemented from \code{Read}.

MECHANICS\cr
Specify the file as with the \code{\link{Read}} function for reading the data into a data frame.  If no arguments are passed to the function, then interactively browse for the file.  Or, enclose within quotes a full path name or a URL for reading the labels on the web.

Given a csv data file, or tab-delimited text file, read the data into an R data frame called \code{mydata} with \code{\link{Read}}. Because \code{\link{Read}} calls the standard R function \code{read.csv}, which serves as a wrapper for \code{read.table}, the usual options that work with \code{read.table}, such as \code{row.names}, also can be passed through the call to \code{Read}. 

SPSS DATA\cr
Relies upon \code{\link{read.spss}} from the \code{foreign} package. To read data in the SPSS \code{.sav} format.  If the file has a file type of \code{.sav}, that is, the file specification ends in \code{.sav}, then the \code{format} is automatically set to \code{"SPSS"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="SPSS"}. Any variable labels in the SPSS file are read and stored in the resulting \code{R} data table (frame). However, SPSS allows value labels for integer variables, so to preserve the variable labels in R the resulting variable is typed as a factor. To preserve the integer type, invoke the \code{\link{read.spss}} option \code{use.value.labels=FALSE}.

R DATA\cr
Relies upon the standard R function \code{load}. By convention only, data files in native R format have a file type of \code{.rda}. To read a native R data file, if the file type is \code{.rda}, the \code{format} is automatically set to \code{"R"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="R"}. Create a native R data file by saving the current data frame, usually \code{mydata}, with the \code{lessR} function \code{\link{Write}}.

Excel DATA\cr
Relies upon the function \code{\link{read_excel}} from Hadley Wickham's \code{readxl} package. Files with a file type of \code{.xls} or \code{.xlsx} are assigned a \code{format} of \code{"Excel"}. The \code{read_excel} parameter \code{sheet} specifies the ordinal position of the worksheet in the Excel file, with a default value of 1. Although not recognized by \code{\link{read_excel}}, the \code{lessR} implementation recognizes the \code{row.names} parameter. Also, \code{\link{read_excel}} does not return integer types, and only character variables are returned instead of factors for non-numeric data. Post-processing is done to result in the data types be the same that \code{\link{Read}} provides for other file types. As such, for reading very large files, it would be faster to use \code{\link{read_excel}} directly.    

lessR DATA\cr
\code{lessR} has some data sets included with the package.  \code{Read} reads each such data set by specifying its name and setting \code{format="lessR"}. Also, each included data set begins with the prefix \code{dat}, which can be deleted when specifying the name of the data set. This option is a replacement for the standard R \code{data} function, offering the added information provided by \code{\link{Read}}.

FIXED WIDTH FORMATTED DATA\cr
Relies upon \code{read.fwf}. Applies to data files in which the width of the column of data values of a variable is the same for each data value and there is no delimiter to separate adjacent data values.  An example is a data file of Likert scale responses from 1 to 5 on a 50 item survey such that the data consist of 50 columns with no spaces or other delimiter to separate adjacent data values. To read this data set, invoke the \code{widths} option of \code{read.fwf}.  

MISSING DATA\cr
By default, \code{Read} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. When reading the data, \code{Read} automatically sets any empty values as missing.  Note that this is different from the R default in \code{read.table} in which an empty value for character string variables are treated as a regular data value. Any other valid value for any data type can be set to missing as well with the \code{missing} option. To mimic the standard R default for missing character values, set \code{missing=NA}. 

To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss.zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss.matrix=TRUE} option. 

VARIABLE LABELS\cr
Unlike standard R, \code{lessR} provides for variable labels, which can be provided for some or all of the variables in a data frame.  One way to enter the variable labels is to read them from their own file with \code{Read} with \code{labels} set to the full path name or \code{URL} of the labels file, or just the file name if the labels file is in the same directory as the data file. The user browses for the labels file if  \code{label=""}. Another method is to include the labels directly in text data file.  To to this, specify the label of variable labels with the \code{label="row2"} option. The web survey application Qualtrics downloads \code{csv} files in this format. Can also read the variable labels separately with the \code{\link{VariableLabels}} function, including both from external files and the console.

These \code{labels} options work for \code{csv} files and Excel files, identified by the filetypes \code{.xls} or \code{.xlsx}. Reading from an Excel file, however, requires the use of the \code{read.xls} funciton from the \code{gdata} package, which also requires that the scripting language Perl be installed. See the \code{Excel DATA} section above for more information.

Variable labels in an SPSS data file or R data file are automatically read into the corresponding R data frame. The labels are stored within the data frame, so if the data frame is written to an external file as a native R data file, the labels are also written as part of that file.

For a file that contains only labels, each row of the file, including the first row, consists of the variable name, a comma if a \code{csv} file, and then the label. For the \code{csv} form of the file, this is the standard \code{csv} format such as obtained with the \code{csv} option from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, usually \code{mydata}, need have a label, and the variables with their corresponding labels can be listed in any order. An example of this file follows for four variables, I1 through I4, and their associated labels.

I2,This instructor presents material in a clear and organized manner.\cr
I4,Overall, this instructor was highly effective in this class.\cr
I1,This instructor has command of the subject.\cr
I3,This instructor relates course materials to real world situations.\cr

If there is a comma in the variable label, then the label needs to be enclosed in quotes.

The \code{lessR} functions that provide analysis, such as \code{\link{Histogram}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{lessR} function \code{\link{label}}, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  

Variable units may also be added to the third column of a variable label file. These are used for generating a better natural language text in the generation of R~Markdown files with the \code{Rmd} option on supporting functions such as \code{\link{Regression}}. For currency (USD), indicate with a unit of: dollar.
}

\value{
The read data frame is returned, usually assigned the name of \code{mydata} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Gerbing, D. W. (2013). R Data Analysis without Programming, Chapter 2, NY: Routledge.

Wickham, H. (2015). Read Excel Files. \url{https://CRAN.R-project.org/package=readxl} 
}


\seealso{
\code{\link{read.csv}}, \code{\link{read.spss}}, \code{\link{read_excel}},  
\code{\link{read.fwf}}, \code{\link{corRead}}, \code{\link{label}},
 \code{\link{details}}, \code{\link{VariableLabels}}.
}

\examples{
# remove the # sign before each of the following Read statements to run

# to browse for a data file on the computer system, invoke Read with 
#   the ref argument empty
# mydata <- Read()
# abbreviated name
# mydata <- rd()
# reduced output to the console
# mydata <- rd.brief()

# browse for a file and then read the variable labels from
#  the specified label file, here a Excel file with two columns,
#  the first column of variable names and the second column the 
#  corresponding labels
# mydata <- Read(labels="employee_lbl.xlsx")

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
# mydata <- Read(quiet=TRUE)
# display full output
# mydata <- Read(brief=FALSE)

# read the built-in data set datEmployee
mydata <- Read("Employee", format="lessR")

# read a data file organized by columns, with a 
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





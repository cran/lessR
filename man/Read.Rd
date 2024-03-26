\name{Read}
\alias{Read}
\alias{rd}
\alias{rd_lbl}
\alias{Read2}

\title{Read Contents of a Data File with Optional Variable Labels and Feedback}

\description{
Abbreviation: \code{rd}, \code{rd_lbl}, \code{Read2}

Reads the contents of the specified data file into an R data table, what R calls a data frame.  By default the format of the file is detected from its filetype: comma or tab separated value text file from \code{.csv}, SPSS data file from \code{.sav}, SAS data from from \code{.sas7bdat}, or R data file from \code{.rda}, and Excel file from \code{.xls}, \code{.xlsx} using Alexander Walker's \code{\link{openxlsx}} package, or \code{.ods} using Gerrit-Jan Schutten and Chung-hong Chan plus other contributor's \code{readODS} package. Specify a fixed width formatted text data file to be read with the required R \code{widths} option. Identify the data file by either browsing for the file on the local computer system with \code{Read()}, or identify the file with the first argument a character string in the form of a path name or a web URL (except for .Rda files which must be on the local computer system).

Any variable labels in a native SPSS are automatically included in the data file. See the \code{details} section below for more information. Variable labels can also be added and modified individually with the \code{lessR} function \code{\link{label}}, and more comprehensively with the \code{\link{VariableLabels}} function.

The function provides feedback regarding the data that is read by invoking the \code{lessR} function \code{\link{details}}. The default brief form of this function invoked by default only lists the input files, the variable name table, and any variable labels.

The \code{lessR} function \code{\link{corRead}} reads a correlation matrix.
}

\usage{
Read(from=NULL, format=NULL, var_labels=FALSE, widths=NULL,

         missing="", n_mcut=1,
         miss_show=30, miss_zero=FALSE, miss_matrix=FALSE, 
      
         max_lines=30, sheet=1, row_names=NULL,

         brief=TRUE, quiet=getOption("quiet"),

         fun_call=NULL, \dots)

rd(\dots) 
rd_lbl(\dots, var_labels=TRUE)
Read2(\dots, sep=";", dec=",")
}


\arguments{
  \item{from}{File reference included in quotes, either empty to browse 
       for the data file, a full path name or web URL, or the name of a
       data file included with lessR, such as \code{"Employee"}. 
       A URL begins with \code{http://}.}
  \item{format}{Format of the data in the file, not usually specified because set
        by default according to the
        file type of the file to read: \code{.csv}, \code{.tsv} or \code{.txt}
        read as a text file, \code{.xls}, \code{.xlsx} read as an Excel file, or
        \code{.ods} as an OpenDocument Spreadsheet file.
        \code{.feather} and \code{.parquet} for the \code{arrow} formats for
          feather and parquet dat files.
        \code{.sav} reads as an SPSS file, which also reads the variable labels
        if present, \code{.sas7bdat} reads as a SAS file, and \code{.rda} reads as a
        native R data file. If the data file is not identified by one of these
        file types, then explicitly set by setting to one of the following values:
        \code{"csv"}, \code{"tsv"}, \code{"Excel"}, \code{"feather"}, 
        \code{"parquet"}, \code{"R"}, \code{"SPSS"},
        or \code{"SAS"}.}
  \item{var_labels}{Set \code{TRUE} if reading a csv or Excel file of 
       variable labels into the data frame \code{l} in which each
       row consists of a variable name in the first
       column and the corresponding variable label in the second column, and perhaps
       units in the third row if using \code{\link{Regression}} function to
       generate automatic markdown files of discursive text.}
  \item{widths}{Specifies the width of the successive columns for fixed width
       formatted data.}\cr

  \item{missing}{Missing value code, which by default is literally a missing data
        value in the data table.}
  \item{n_mcut}{For the missing value analysis, list the row name and number of
        missing values if the number of missing exceeds or equals this cutoff.
        Requires \code{brief=FALSE}.}
  \item{miss_show}{For the missing value analysis, the number of rows, one row per
        observation, that has as many or missing values as \code{n_mcut}. 
        Requires \code{brief=FALSE}.}
  \item{miss_zero}{For the missing value analysis, list the variable name or the
        row name even for values of 0, that is rows with no missing data.
        By default only variables and rows with missing data are listed.
        Requires \code{brief=FALSE}.}
  \item{miss_matrix}{For the missing value analysis, if there is any missing data, 
        list a version of the complete data table with a 0 for a non-missing value
        and a 1 for a missing value.}\cr

  \item{sep}{Character that separates adjacent values in a text file of data.}
  \item{dec}{Character that serves as the decimal separator in a number.}
  \item{max_lines}{Maximum number of lines to list of the data and labels.}
  \item{sheet}{For Excel files, specifies the work sheet to read. Provide either
       the worksheet number according to its position, or its name enclosed in
       quotes. The default is the first work sheet.}
  \item{row_names}{\code{FALSE} by default so no row names from the input data.
       Set to \code{TRUE} to convert the first column of input data to row names.
       For reading \code{.csv} files, can also set to the integer number of the
       column to convert to row names. For Excel and ODS files, only acceptable
       value is 1 for the first column.}
  \item{brief}{If \code{TRUE}, display only variable names table plus any variable
       labels.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change the corresponding
       system default with \code{\link{style}} function.}
  \item{fun_call}{Function call. Used with \code{Rmd} to pass the function call when
        obtained from the abbreviated function call \code{rd}.}
  \item{\dots}{Other parameter values define with the R read functions, such as the
       \code{read.table} function for text files, with row.names and header.}
}


\details{
By default \code{Read} reads text data files which are either comma delimited, \code{csv}, or tab-delimited data files, native Excel files of type \code{.xls} or \code{.xlsx}, native ODS files of type \code{.ods}, native R files with file type of \code{.rda}, native SAS files with file type \code{.sas7bdat}, and native SPSS files with file type \code{.sav}. Invoke the \code{widths} option to allow for the reading of fixed width formatted data. Calls the \code{lessR} function \code{\link{details}} to provide feedback regarding details of the data frame that was read. By default, variables defined by non-numeric variables are read as character strings. To read as \code{factors} specify \code{stringsAsFactors} as \code{FALSE}, unless all the values of a variable a non-numeric and unique, in which case the variable is classified as a character string.

CREATE csv FILE\cr
One way to create a csv data file is to enter the data into a text editor. A more structured method is to use a worksheet application such as MS Excel, LibreOffice Calc, or Apple Numbers.  Place the variable names in the first row of the worksheet. Each column of the worksheet contains the data for the corresponding variable. Each subsequent row contains the data for a specific observation, such as for a person or a company.  

Call \code{help(read.table)} to view the other R options that can also be implemented from \code{Read}.

MECHANICS\cr
Specify the file as with the \code{\link{Read}} function for reading the data into a data frame.  If no arguments are passed to the function, then interactively browse for the file.

Given a csv data file, or tab-delimited text file, read the data into an R data frame called \code{d} with \code{\link{Read}}. Because \code{\link{Read}} calls the standard R function \code{read.csv}, which serves as a wrapper for \code{read.table}, the usual options that work with \code{read.table}, such as \code{row.names}, also can be passed through the call to \code{Read}. 

SPSS DATA\cr
Relies upon \code{read_spss} from the \code{haven} package to read data in the SPSS \code{.sav} or \code{.zsav} format.  If the file has a file type of \code{.sav}, that is, the file specification ends in \code{.sav}, then the \code{format} is automatically set to \code{"SPSS"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="SPSS"}. Each (usually) integer variable with value labels is converted into two R variables: the original numeric code with the original variable name, and also the corresponding factor with the variable labels named with the original name plus the suffix \code{_f}. The variable labels are also displayed for copying into a variable label file. See the SPSS section from \code{vignette("Read")}. 

R DATA\cr
Relies upon the standard R function \code{load}. By convention only, data files in native R format have a file type of \code{.rda}. To read a native R data file, if the file type is \code{.rda}, the \code{format} is automatically set to \code{"R"}. To invoke this option for a relevant data file of any file type, explicitly specify \code{format="R"}. Create a native R data file by saving the current data frame, usually \code{d}, with the \code{lessR} function \code{\link{Write}}.

Excel DATA\cr
Relies upon the function \code{\link{read.xlsx}} from Alexander Walker's \code{openxlsx} package. Files with a file type of \code{.xlsx} are assigned a \code{format} of \code{"Excel"}. The \code{read.xlsx} parameter \code{sheet} specifies the ordinal position of the worksheet in the Excel file, with a default value of 1. The \code{row.names} parameter can only have a value of 1. Dates stored in Excel as an Excel date type are automatically read as an R Date type. See the help file for \code{\link{read.xlsx}} for additional parameters, such as \code{sheet} for the name or number of the worksheet to read and \code{startRow} for the row number for which to start reading data.  

lessR DATA\cr
\code{lessR} has some data sets included with the package: "BodyMeas", "Cars93", "Employee", "Jackets", "Learn", "Mach4", "Reading", and "StockPrice".  \code{Read} reads each such data set by specifying its name, such as \code{Read("Employee")}. No specificaiton of \code{format} and no provided filetype, just enter the name of the data set. 

FIXED WIDTH FORMATTED DATA\cr
Relies upon \code{read.fwf}. Applies to data files in which the width of the column of data values of a variable is the same for each data value and there is no delimiter to separate adjacent data values_  An example is a data file of Likert scale responses from 1 to 5 on a 50 item survey such that the data consist of 50 columns with no spaces or other delimiter to separate adjacent data values_ To read this data set, invoke the \code{widths} option of \code{read.fwf}.  

MISSING DATA\cr
By default, \code{Read} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. When reading the data, \code{Read} automatically sets any empty values as missing.  Note that this is different from the R default in \code{read.table} in which an empty value for character string variables are treated as a regular data value. Any other valid value for any data type can be set to missing as well with the \code{missing} option. To mimic the standard R default for missing character values, set \code{missing=NA}. 

To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss_zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss_matrix=TRUE} option. 

VARIABLE LABELS\cr
Unlike standard R, \code{lessR} provides for variable labels, which can be provided for some or all of the variables in a data frame. Store the variable labels in a separate data frame \code{l}. The variable labels file that is read by \code{Read} consists of one row for each variable for which a variable label is provided. Each row consists of either two columns, the variable name in the first column and the associated variable label in the second column, or three columns with the third column the variable units. Use the units in conjunction for enhanced readability with the automatic markdown generated by the \code{Rmd} parameter for the \code{\link{Regression}} function. The format of the file can be \code{csv} or \code{xlsx}. The data frame \code{Read} constructs from this input consists of one variable, called \code{label}, with the variable names as row names. 

The \code{lessR} legacy approach is to store the variable labels directly with the data in the same data frame. The problem with this approach is that any transformations of the data with any function other than \code{lessR} transformation functions remove the variable labels. The option for reading the variable labels with the \code{labels} option of \code{Read} statement is retained for compatibility.

Reading the data from an SPSS file, however, retains the SPSS variable labels as part of the data file. The \code{lessR} data analysis functions will properly process these variable labels, but any non-\code{lessR} data transformations will remove the labels from the data frame. To retain the labels, copy them to the \code{l} data frame with the \code{\link{VariableLabels}} function with the name of the data frame as the sole argument.

The \code{lessR} functions that provide analysis, such as \code{\link{Histogram}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{lessR} function \code{\link{label}}, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  
}

\value{
The read data frame is returned, usually assigned the name of \code{d} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 1, NY: CRC Press.

Alexander Walker (2018). openxlsx: Read, Write and Edit XLSX Files. R package version 4.1.0. https://CRAN.R-project.org/package=openxlsx
}


\seealso{
\code{\link{read.csv}}, \code{\link{read.xlsx}},  
\code{\link{read.fwf}}, \code{\link{corRead}}, \code{\link{label}},
 \code{\link{details}}, \code{\link{VariableLabels}}.
}

\examples{
# remove the # sign before each of the following Read statements to run

# to browse for a data file on the computer system, invoke Read with 
#   the from argument empty
# d <- Read()
# abbreviated name
# d <- rd()

# read the variable labels from
#  the specified label file, here a Excel file with two columns,
#  the first column of variable names and the second column the 
#  corresponding labels
# l <- Read("Employee_lbl", var_labels=TRUE)

# read a csv data file from the web
# d <- Read("http://web.pdx.edu/~gerbing/data/twogroup.csv")

# read a csv data file with -99 and XXX set to missing
# d <- Read(missing=c(-99, "XXX"))

# do not display any output
# d <- Read(quiet=TRUE)
# display full output
# d <- Read(brief=FALSE)

# read the built-in data set dataEmployee
d <- Read("Employee")

# read a data file organized by columns, with a 
#   5 column ID field, 2 column Age field
#   and 75 single columns of data, no spaces between columns
#   name the variables with lessR function: to
#   the variable names are Q01, Q02, ..., Q74, Q75
# d <- Read(widths=c(5,2,rep(1,75)), col.names=c("ID", "Age", to("Q", 75)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }





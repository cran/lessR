\name{Write}
\alias{Write}
\alias{wrt}
\alias{wrt_r}
\alias{wrt_x}

\title{Write the Contents of a Data Frame to an External File}

\description{
Abbreviation: \code{wrt}, \code{wrt_r}, \code{wrt_x}

Writes the contents of the specified data frame, such as with the default \code{d}, to the current working directory as either the default \code{csv} data file and also tab limited and space unlimited text files, an Excel data table, an OpenDocument Spreadsheet file, an arrow \code{feather} or \code{parquet} file, or a native R data file of the specified data frame. If the write is for a \code{.csv}, or \code{.tsv}, or \code{.prn} text file, then any variable labels are written to a second \code{csv} file with "_lbl" appended to the file name. Any variable labels and variable units are automatically included in a native R data file.
}

\usage{
Write(data=d, to=NULL, 

         format=c("csv", "txt", "tsv", "prn",
                  "Excel", "ODS", "R", "SPSS", "feather", "parquet"),

         row_names=NULL, quote="if_needed", missing=NULL, dec=".", sep=NULL,  

         ExcelTable=FALSE, ExcelColWidth=TRUE,

         quiet=getOption("quiet"), \dots)

wrt(\dots)

wrt_r(\dots, format="R")
wrt_x(\dots, format="Excel")
}


\arguments{
  \item{data}{Data frame of which the contents are to be written to an
        external data file, that is, no quotes.}
  \item{to}{Name of the output file as a character string, that is, with quotes.
        If not included in the name, the file type is automatically added to the
        name according to the specified \code{format}. Or, specify the file name
        with the file extension from which the \code{format} is derived.
        If omitted, then the file name is the data frame name.}
  \item{format}{Format of file to be written with \code{.csv} as the default.}
  \item{row_names}{Format of file to be written with \code{.csv} as the default.
       Set to \code{TRUE} by default unless writing to Excel or csv file
       and row names are just the integers from 1 to the number of rows.}\cr
  \item{quote}{Specifies how character data values are to be quoted. The default is
       \code{"if_needed"}, which puts quotes around character data values that
        include spaces, commas, tabs, and other white spaces. The value of \code{TRUE}
        quotes character data values, needed or not.}
  \item{missing}{The data value indicates missing for text files. Defaults to a
        blank space except for \code{prn} files, which is then \code{NA}.}
  \item{dec}{The character that represents the decimal point for text files.}
  \item{sep}{The character that separates adjacent data values for text files.
        Defaults to \code{","}.}\cr

  \item{ExcelTable}{If \code{TRUE}, write the Excel file as an Excel table.}
  \item{ExcelColWidth}{\code{TRUE} by default but calculation of column widths for
       large files takes more time, so option to turn off.}\cr

  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{style}} function.}
  \item{\dots}{Other parameter values for \code{csv} files consistent with the
       usual \code{\link{write.table}}, including \code{na=""} to write missing
       data to a \code{csv} file as blanks instead of \code{NA}.}
}


\details{
The default file name is the name of the data frame to be written, otherwise use \code{to} to specify the name. To specify the file type of the output data file, do so with any available file type provided as part of the file name for the output file, or by the value of the \code{format} parameter. Can specify the file name without the file type, which, if no \code{format} is provided, \code{Write} adds automatically the \code{.csv} file extension. The name of the file that is written, as well as the name of the working directory into which the file was written, are displayed at the console. 

The following table lists various file formats along with the associated R packages and functions for writing them. The default text file format, \code{.txt}, defaults to \code{dec="."} and \code{sep=","}, that is, North American \code{csv} format, but can be customize as needed.

\tabular{llll}{
  \strong{Extension}  \tab \strong{Format} \tab \strong{Package} \tab \strong{Function} \cr
  .txt     \tab Text, customize dec and sep  \tab R utils  \tab \code{write.table()} \cr
  .csv     \tab Text, comma-separated values  \tab R utils  \tab \code{write.table()} \cr
  .tsv     \tab Text, tab-separated values  \tab R utils  \tab \code{write.table()} \cr
  .prn     \tab Text, space-separated values  \tab R utils  \tab \code{write.table()} \cr
  .xlsx    \tab Excel  \tab openxlsx  \tab \code{writeData()} or \code{writeDataTable()} \cr
  .ods     \tab ODS  \tab readODS  \tab \code{write_ods()} \cr
  .feather \tab Feather  \tab arrow  \tab \code{write_feather()} \cr
  .parquet \tab Parquet  \tab arrow  \tab \code{write_parquet()} \cr
  .rda     \tab R data  \tab R base  \tab \code{save()} \cr
  .sav     \tab SPSS  \tab haven  \tab \code{write_sav()} \cr
}

\code{Write} is designed to work in conjunction with the function \code{\link{Read}} from this package, which reads a csv or other text file, fixed width format, or native SPSS or R data files into the data frame \code{d}. \code{Write} relies upon the R functions \code{\link{write.csv}} and \code{\link{save}}.

When writing the data frame in native R format, the specified name of the resulting \code{.rda} file is distinct from the name of the data frame as stored within R.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}, \code{\link{write.table}}, \code{\link{save}}.
}

\examples{
# create data frame called d
#n <- 12
#X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
#Y <- rnorm(n=n, mean=50, sd=10)
#d <- data.frame(X,Y)

# write the current contents of default data frame d to GoodData.csv
# Write(d, "GoodData")
# short name
# write the default data frame d to the R data file d.rda
#  wrt_r(d)

# write the data as an Excel data table in an Excel file
# Write(d, "GoodData", format="Excel")
# with abbreviation
# wrt_x(d, "GoodData")

# access the R data frame warpbreaks
#   then, write the file warpbreaks.rda
# data(warpbreaks)
# wrt_r(warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }

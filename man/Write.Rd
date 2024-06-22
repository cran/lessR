\name{Write}
\alias{Write}
\alias{wrt}
\alias{wrt_r}
\alias{wrt_x}

\title{Write the Contents of a Data Frame to an External File}

\description{
Abbreviation: \code{wrt}, \code{wrt_r}, \code{wrt_x}

Writes the contents of the specified data frame, such as with the default \code{d}, to the current working directory as either the default \code{csv} data file, an Excel data table, an OpenDocument Spreadsheet file, an arrow \code{feather} or \code{parquet} file, or a native R data file of the specified data frame. If the write is for a \code{csv} file, then any variable labels are written to a second \code{csv} file with "_lbl" appended to the file name. Any variable labels and variable units are automatically included in a native R data file.
}

\usage{
Write(data=d, to=NULL, 

         format=c("csv", "R", "Excel", "ODS", "SPSS", "feather", "parquet"),

         rowNames=NULL,

         ExcelTable=FALSE, ExcelColWidth=TRUE,

         quiet=getOption("quiet"), \dots)

wrt(\dots)

wrt_r(\dots, format="R")
wrt_x(\dots, format="Excel")
}


\arguments{
  \item{to}{Name of the output file as a character string, that is, with quotes.
        If not included in the name, the file type is automatically added to the
        name, either \code{.csv} or \code{.rda}, depending of the value of
        \code{format}.}
  \item{data}{Data frame to be written as an object, that is, no quotes.}
  \item{format}{Format of file to be written with \code{.csv} as the default.}
  \item{rowNames}{Format of file to be written with \code{.csv} as the default.
       Set to \code{TRUE} by default unless writing to Excel or csv file
       and row names are just the integers from 1 to the number of rows.}\cr

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
Can specify the file name without the file type, which \code{Write} adds automatically, \code{.csv} for a comma separated values data file and \code{.rda} for a native R data file. The default file name is the name of the data frame to be written. The name of the file that is written, as well as the name of the working directory into which the file was written, are displayed at the console. 

An Excel file is written using functions from Alexander Walker's \code{openxlsx} package.

\code{Write} is designed to work in conjunction with the function \code{\link{Read}} from this package, which reads a csv, fixed width format, or native SPSS or R data files into the data frame \code{d}. \code{Write} relies upon the R functions \code{\link{write.csv}} and \code{\link{save}}.

When writing the data frame in native R format, the specified name of the resulting \code{.rda} file is distinct from the name of the data frame as stored within R.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}, \code{\link{write.csv}}, \code{\link{save}}.
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
#  wrt_r()

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

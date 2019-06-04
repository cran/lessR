\name{details}
\alias{details}
\alias{db}

\title{Display Contents of a Data File and Optional Variable Labels}

\description{
Abbreviation: \code{db}

Provides feedback regarding a data frame which includes the variable names, the dimensions of the resulting data frame, the data type for each variable, and the values of the variables in the data file for the first and last rows of the data. In addition, an analysis of missing data is provided, listing the number of missing values for each variable and for each observation.
}

\usage{
details(data=d, n_mcut=1, miss_zero=FALSE, max_lines=30,
         miss_show=30, miss_matrix=FALSE, brief=getOption("brief"))

db(\ldots, brief=TRUE)
}

\arguments{
  \item{data}{Data frame for which to provide the details.}
  \item{n_mcut}{For the missing value analysis, list the row name and number of missing values
        if the number of missing exceeds or equals this cutoff.}
  \item{miss_zero}{For the missing value analysis, list the variable name
       or the row name even for values of 0. By default only variables and
       rows with missing data are listed.}
  \item{max_lines}{Maximum number of lines to list of the data and labels.}
  \item{miss_show}{For the missing value analysis, the number of rows, one
       row per observation, that has as many or missing values as \code{n_mcut}.}
  \item{miss_matrix}{For the missing value analysis, if there is any missing data, 
       list a version of the complete data table with a 0 for a non-missing]
        value and a 1 for a missing value.}
  \item{brief}{If \code{TRUE}, display only variable names table plus any
        variable labels. The default for ``details brief'' abbreviation \code{db}.}
  \item{\dots}{Further arguments to be passed to or from methods consistent
       with the R \code{\link{read.table}} function. For example, can set
       \code{stringsAsFactors} as \code{TRUE}.}
}

\details{
MISSING DATA\cr
By default, \code{details} provides a list of each variable and each row with the display of the number of associated missing values, indicated by the standard R missing value code NA. To not list the variable name or row name of variables or rows without missing data, invoke the \code{miss_zero=FALSE} option, which can appreciably reduce the amount of output for large data sets. To view the entire data table in terms of 0's and 1's for non-missing and missing data, respectively, invoke the \code{miss_matrix=TRUE} option. 

VARIABLE LABELS\cr
Standard R does not provide for variable labels, but \code{lessR} does. Variable labels can be provided for some or all of the variables in the data frames.  One way to enter the variable labels is to read them from their own file with \code{details} with \code{labels} set to the full path name or \code{URL} of the labels file, or just the file name if the labels file is in the same directory as the data file.  Another method is to include the labels directly in the data file.  To to this, specify the file of variable labels with the \code{label="row2"} option. The web survey application Qualtrics downloads \code{csv} files in this format.

For a file that contains only labels, each row of the file, including the first row, consists of the variable name, a comma, and then the label, that is, standard \code{csv} format such as obtained with the \code{csv} option from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, usually \code{d}, need have a label, and the variables with their corresponding labels can be listed in any order. An example follows.

I2,This instructor presents material in a clear and organized manner.\cr
I4,Overall, this instructor was highly effective in this class.\cr
I1,This instructor has command of the subject.\cr
I3,This instructor relates course materials to real world situations.\cr

If there is a comma in the variable label, then the label needs to be enclosed in quotes.

The \code{lessR} functions that provide analysis, such as \code{\link{Histogram}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{\link{label}} function, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}.
}

\examples{
# read the built-in data set datEmployee
# this provides an automatic call to details
d <- Read("Employee")

# manually request the details for d
details()

# manually request just variable names, labels for d
db()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ read }
\keyword{ csv }





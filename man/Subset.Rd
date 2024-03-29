\name{Subset}
\alias{Subset}

\title{Subset the Values of One or More Variables}

\description{
Abbreviation: \code{subs}

Deprecated, use \code{\link{.}} instead in conjunction with base R \code{link{Extract}}.

Based directly on the standard R \code{\link{subset}} function to only include or exclude specified rows or data, and for specified columns of data.  Output provides feedback and guidance regarding the specified subset operations. Rows of data may be randomly extracted, and also with the code provided to generate a hold out validation sample created. The hold out sample is created from the original data frame, usually named d, so the subset data frame must be directed to a data frame with a new name or the data re-read to construct the holdout sample. Any existing variable labels are retained in the subset data frame.

}

\usage{
Subset(rows, columns, data=d, holdout=FALSE,
       random=0, quiet=getOption("quiet"), \dots)
}

\arguments{
  \item{rows}{Specify the rows, i.e., observations, to be included or deleted,
       such as with a logical expression or by direct specification of the
       numbers of the corresponding rows of data.}
  \item{columns}{Specify the columns, i.e., variables, to be included or
        deleted.}
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{d} by default.}
  \item{holdout}{Create a hold out sample for validation if rows is a proportion
       or an integer to indicate random extraction of rows of data.}
  \item{random}{If an integer or proportion, specifies number of rows to data
        to randomly extract.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{\dots}{The list of variables, each of the form, \code{variable = equation}.
       Each \code{variable} can be the name of an existing variable in the data frame
       or a newly created variable.}
}

\details{
\code{Subset} creates a subset data frame based on one or more rows of data and one or more variables in the input data frame, and lists the first five rows of the revised data frame. Guidance and feedback regarding the subsets are provided by default. The first five lines of the input data frame are listed before the subset operation, followed by the first five lines of the output data frame.

The argument \code{rows} can be a logical expression based on values of the variables, or it can be an integer or proportion to indicate random extraction of rows. An integer specifies the number of rows to retain, and a proportion specifies the corresponding proportion, which is then rounded to an integer. If \code{holdout=TRUE}, then the code to create a hold out data frame with a subsequent \code{Subset} analysis is also created. Copy and run this code on the original data frame to create the hold out sample.

To indicate retaining an observation, specify at least one variable name and the value of the variable for which to retain the corresponding observations, using two equal signs to indicate the logical equality.  If no rows are specified, all rows are retained. Use the base R \code{\link{row.names}} function to identify rows by their row names, as illustrated in the examples below.

To indicate retaining a variable, specify at least one variable name. To specify multiple variables, separate adjacent variables by a comma, and enclose the list within the standard R combine function, \code{\link{c}}.  A single variable may be replaced by a range of consecutive variables indicated by a colon, which separates the first and last variables of the range.  To delete a variable or variables, put a minus sign, -, in front of the \code{c}.
}

\value{
The subset of the data frame is returned, usually assigned the name of \code{d} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}
\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{subset}}, \code{\link{factor}}.
}

\examples{
# construct data frame
d <- read.table(text="Severity Description
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# only include those with a value of Moderate for Description
d <- Subset(rows=Description=="Moderate")

# locate, that is, display only, the 2nd and 4th rows of data
Subset(row.names(d)=="2" | row.names(d)=="4")

# retain only the first and fourth rows of data, store in myd
myd <- Subset(c(1,4))

# delete only the first and fourth rows of data, store in myd
myd <- Subset(-c(1,4))

# built-in data table warpbreaks has several levels of wool
#   and breaks plus continuous measure tension
# retain only the A level of wool and the L level of tension,
#   and the one variable breaks
d <- Subset(wool=="A" & tension=="L", columns=breaks, data=warpbreaks)

# delete Years and Salary 
d <- Read("Employee", quiet=TRUE)
d <- Subset(columns=-c(Years, Salary))

# locate, display only, a specified row by its row.name
d <- Read("Employee", quiet=TRUE)
Subset(row.names(d)=="Fulton, Scott")

# randomly extract 60\% of the data
# generate code to create the hold out sample of the rest
d <- Read("Employee", quiet=TRUE)
mysubset <- Subset(random=.6, holdout=TRUE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ subset }



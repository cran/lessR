\name{Merge}
\alias{Merge}
\alias{mrg}

\title{Merge Two Data Frames Horizontally or Vertically}

\description{
Abbreviation: \code{mrg}

A horizontal merge combines data frames horizontally, that is, adds variables (columns) to an existing data frame, such as with a common shared ID field. Performs the horizontal merge based directly on the standard R \code{\link{merge}} function. The vertical merge is based on the \code{\link{rbind}} function in which the two data frames have the same variables but different cases (rows), so the rows build vertically, stacked on top of each other. 

The advantages of this \code{lessR} function is that it provides a single function for merging data frames, adds text output to the standard R functions that provide feedback regarding properties of the merge, and provides more detailed and presumably more useful error messages.

}

\usage{
Merge(data1, data2, by=NULL, quiet=getOption("quiet"), \dots)

mrg(\dots)
}

\arguments{
  \item{data1}{The name of the first data frame from which to create the merged 
       data frame.}
  \item{data2}{The name of the second data frame from which to create the merged 
       data frame.}
  \item{by}{If a variable specified, then signals a horizontal merge with the
       ID field by which the data frames are merged as an inner join, that is,
       only rows of data are retained that both match on the ID. Specify
       \code{"rows"} to merge vertically.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{\dots}{Additional arguments available in the base R \code{\link{merge}}
       function such as \code{all.x=TRUE} for an left outer join, which retains
       all rows of the first data frame even if not matched by a row in the
       second data table. Specify a right outer join with \code{all.y=TRUE}
       and a full outer join, in which all records from both data frames are
       retained, with \code{all=TRUE}.}
}

\details{
\code{Merge} creates a merged data frame from two input data frames.

If \code{by} is specified the merge is horizontal. That is the variables in the second input data frame are presumed different from the variables in the first input data frame. The merged data frame is the combination of variables from both input data frames, with the rows aligned by the value of \code{by}, an ID field common to both data frames. The result is a \emph{natural join}, a specific instance of an \emph{inner join} in which merging occurs according a common variable.

Invoke \code{\link{merge}} parameters \code{all.x}, \code{all.y}, and \code{all}, set to \code{TRUE} for the corresponding condition. These parameters set, respectively, a \emph{left-outer join}, \emph{right-outer join}, and a \emph{outer join} in which all records from both data frames are retained regardless if a matching row in the other data frame. 

Set \code{by} to \code{"rows"} for a vertical merge. The variables are presumed the same in each input data frame.  The merged data frame consists of the rows of both input data frames. The rows of the first data frame are stacked upon the rows of the second data frame. 

Guidance and feedback regarding the merge are provided by default. The first five lines of each of the input data frames are listed before the merge operation, followed by the first five lines of the output data frame.
}

\value{
The merged data frame is returned, usually assigned the name of \code{d} as in the examples below. This is the default name for the data frame input into the \code{lessR} data analysis functions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{merge}}, \code{\link{rbind}}.
}

\examples{
# Horizontal
#-----------
d <- Read("Employee", quiet=TRUE)
Emp1a <- d[1:4, .(Years, Gender, Dept, Salary)]
Emp1b <- d[1:4, .(JobSat, Plan)]
# horizontal merge
d <- Merge(Emp1a, Emp1b, by="row.names")
# suppress output to console
d <- Merge(Emp1a, Emp1b, by="row.names", quiet=TRUE)

# Vertical
#---------
d <- Read("Employee", quiet=TRUE)
Emp2a <- d[1:4,]
Emp2b <- d[7:10,]
# vertical merge
d <- Merge(Emp2a, Emp2b, by="rows")


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ merge }



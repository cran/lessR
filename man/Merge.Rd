\name{Merge}
\alias{Merge}
\alias{mrg}

\title{Merge Two Data Frames Horizontally or Vertically}

\description{
Abbreviation: \code{mrg}

Based directly on the standard R \code{\link{merge}} function for a horizontal merge that combines data frames horizontally, that is, adds variables (columns) to an existing data frame according to a common shared ID field. The vertical merge is based on the \code{\link{rbind}} function in which the two data frames have the same variables but different cases (observations), so the rows build vertically, stacked on top of each other. Any existing variable labels in the input data frames are retained in the merged data frame.

The advantages of this \code{lessR} function is that it provides a single function for merging data frames, adds text output to the standard R functions that provide feedback regarding properties of the merge, provides more detailed and presumably more useful error messages, and preserves any variable labels that might exist in one or both of the merged data frames.

}

\usage{
Merge(data1, data2, by=NULL, quiet=getOption("quiet"), \ldots)

mrg(\ldots)
}

\arguments{
  \item{data1}{The name of the first data frame from which to create the merged 
       data frame.}
  \item{data2}{The name of the second data frame from which to create the merged 
       data frame.}
  \item{by}{If specified, then signals a horizontal merge and the ID field by
        which the data frames are merged. Specify \code{"row.names"} for merging
        according to the row names.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{theme}} function.}
  \item{\dots}{Additional arguments available in the base R \code{\link{merge}}
       function such as \code{all.x=TRUE} for retaining all rows of the first 
       data frame even if not matched by a row in the second data table.}
}

\details{
\code{Merge} creates a merged data frame from two input data frames.

If \code{by} is specified the merge is horizontal. That is the variables in the second input data frame are presumed different from the variables in the first input data frame. The merged data frame is the combination of variables from both input data frames, with the rows aligned by the value of \code{by}, an ID field common to both data frames. 

If \code{by} is not provided, then the merge is vertical. The variables are presumed the same in each input data frame.  The merged data frame consists of the rows of both input data frames. The rows of the first data frame are stacked upon the rows of the second data frame. 

Guidance and feedback regarding the merge are provided by default. The first five lines of each of the input data frames are listed before the merge operation, followed by the first five lines of the output data frame.
}

\value{
The merged data frame is returned, usually assigned the name of \code{mydata} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{merge}}, \code{\link{rbind}}.
}

\examples{
# Horizontal
#-----------
mydata <- Read("Employee", format="lessR", quiet=TRUE)
Emp1a <- Subset(1:4, columns = c("Years", "Gender", "Dept", "Salary"))
Emp1b <- Subset(1:4, columns = c("Satisfaction", "HealthPlan"))
# horizontal merge
mydata <- Merge(Emp1a, Emp1b, by="row.names")
# suppress output to console
mydata <- Merge(Emp1a, Emp1b, by="row.names", quiet=TRUE)

# Vertical
#---------
mydata <- Read("Employee", format="lessR", quiet=TRUE)
Emp2a <- Subset(1:4)
Emp2b <- Subset(7:10)
# vertical merge
mydata <- Merge(Emp2a, Emp2b)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ merge }



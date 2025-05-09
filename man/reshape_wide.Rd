\name{reshape_wide}
\alias{reshape_wide}

\title{Reshape a Long-Form Data Frame to Wide-Form}

\description{
Takes the variables in a long-form data frame, \code{widen}, \code{response}, and \code{ID}, and transforms to a wide form data frame. All other variables are deleted in the transformed data frame. A simple wrapper for Base R \code{\link{reshape}} with sensible parameter names and sensible defaults, and able to specify a range of variables to transform. Conversion currently limited to converting based on a single grouping variable.
}

\usage{
reshape_wide(data, widen, response, ID, prefix=NULL, sep="_", ...)
}

\arguments{
  \item{data}{Data frame that contains the variables to analyze as a
        wide-form single column.}
  \item{widen}{Name of the (single) grouping variable in the input long-form column.}
  \item{response}{Name of the variable of the response values in the input
        long-form column.}
  \item{ID}{Name of the ID field in the long-form column.}
  \item{prefix}{If \code{TRUE}, prefix the column names in the wide form
        of each corresponding level of the  
        \code{widen} variable with the name of the \code{response}. Unless
        the values of \code{widen} are numeric, the default is \code{FALSE},
        just using the level names as the column names.}
  \item{sep}{If \code{prefix} is \code{TRUE}, the separator between the
        name of the level and the name of the \code{response} variable, with
        default \code{"_"}.}
  \item{\dots}{Older parameter values to be converted internally.}
}

\details{

Here is the correspondence between the original \code{\link{reshape}} parameter names and the \code{reshape_wide} parameter names.

\tabular{ll}{
reshape \tab reshape_wide\cr
----------- \tab ----------------\cr
\code{v.names} \tab \code{response}\cr
\code{timevar} \tab \code{widen}\cr
\code{idvar} \tab \code{ID}\cr
----------- \tab ----------------\cr
}
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{reshape}}.
}

\examples{
d <- Read("Anova_rb")  # already in wide-form
dl <- reshape_long(d, sup1:sup4)  # convert to long-form

# convert back to wide form 
reshape_wide(dl, widen=Group, response=Response, ID=Person)

# with the name of the response prefixed to the column names
reshape_wide(dl, widen=Group, response=Response, ID=Person,
             prefix=TRUE, sep=".")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ wide-form }

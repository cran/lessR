\name{reshape_wide}
\alias{reshape_wide}

\title{Reshape a Wide-Form Data Frame to Wide-Form}

\description{
A simple wrapper for Base R \code{\link{reshape}} with sensible parameter names and sensible defaults, and able to specify a range of variables to transform.
}

\usage{
reshape_wide(data, group, response, ID, prefix=NULL, sep="_")
}

\arguments{
  \item{data}{Data frame that contains the variables to analyze
        wide-form single column.}
  \item{group}{Name of the grouping variable in the input long-form column.}
  \item{response}{Name of the variable of the response values in the input
        long-form column.}
  \item{ID}{Name of the ID field in the long-form column.}
  \item{prefix}{If \code{TRUE}, prefix the column names in the wide form
        of the levels of each corresponding level of the  
        \code{group} variable with the name of the \code{response}. Unless
        the values of \code{group} are numeric, the default is \code{FALSE},
        just using the levels as the column names.}
  \item{sep}{If \code{prefix} is \code{TRUE}, the separator between the
        name of the level and the name of the \code{response} variable, with
        default \code{"_"}.}
}

\details{
\code{reshape_wide} takes the variables in the long-form  \code{group}, \code{response}, and \code{ID} and transforms to wide form.

Here is the correspondence between the original \code{\link{reshape}} parameter names and the \code{reshape_wide} parameter names.

\tabular{ll}{
reshape \tab reshape_wide\cr
----------- \tab ----------------\cr
\code{v.names} \tab \code{response}\cr
\code{timevar} \tab \code{group}\cr
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

# convert back to long form 
reshape_wide(dl, group="Group", response="Value", ID="Person")

# with the name of the response prefixed to the column names
reshape_wide(dl, group="Group", response="Value", ID="Person",
             prefix=TRUE, sep=".")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ wide-form }

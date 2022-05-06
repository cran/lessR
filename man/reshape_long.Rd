\name{reshape_long}
\alias{reshape_long}

\title{Reshape a Wide-Form Data Frame to Long-Form}

\description{
A simple wrapper for Base R \code{\link{reshape}} with sensible parameter names and sensible defaults, and able to specify a range of variables to transform.
}

\usage{
reshape_long(data, transform, group="Group", response="Response", ID="ID",
           prefix=ID, sep="")
}

\arguments{
  \item{data}{Data frame that contains the variables to reshape.}
  \item{transform}{The wide-form column variable names to transform to a
        long-form single column.}
  \item{group}{Name of the grouping variable in the new
        long-form column.}
  \item{response}{Name of the variable of the response values in the new
        long-form column.}
  \item{ID}{Name of the newly created ID field in the new long-form
        column, the original row number from the wide-form. If \code{NULL},
        the default value, then not created.}
  \item{prefix}{The prefix added to the value of ID for each row of data.}
  \item{sep}{Any potential separator of the ID prefix from the given value
       of the ID.}
}

\details{
\code{reshape_long} takes the \code{transform} variables in the wide-form from
which it creates three new columns, \code{group}, \code{response}, and \code{ID}.

The correspondence between the original  \code{\link{reshape}} parameter names and the \code{reshape_long} parameter names is shown in the following table.

\tabular{ll}{
reshape \tab reshape_long\cr
----------- \tab ----------------\cr
\code{varying} \tab \code{transform}\cr
\code{v.names} \tab \code{response}\cr
\code{timevar} \tab \code{group}\cr
\code{times} \tab \code{transform}\cr
\code{idvar} \tab \code{ID}\cr
----------- \tab ----------------\cr
}
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{reshape}}.
}

\examples{
d <- Read("Anova_rb")

# with the default variable names in the long-form
reshape_long(d, c("sup1", "sup2", "sup3", "sup4"))

# with a variable range and custom variable names in the long-form
reshape_long(d, sup1:sup4, group="Supplement", response="Reps", ID="Person")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ long-form }

\name{reshape_long}
\alias{reshape_long}

\title{Reshape a Wide-Form Data Frame to Long-Form}

\description{
A simple wrapper for Base R \code{\link{reshape}} with sensible parameter names and sensible defaults, and able to specify a range of variables to transform.
}

\usage{
reshape_long(data, transform, group="Group", response="Response", ID="ID",
           prefix=ID, sep="", shape=c("rect", "square"))
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
        then not created.}
  \item{prefix}{The prefix added to the value of ID for each row of data.}
  \item{sep}{Any potential separator of the ID prefix from the given value
       of the ID.}
  \item{shape}{In general, the input data is a rectangular, wide-form data frame.
       However, it can also be a symmetric, square correlation or 
       covariance matrix.}
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

For a correlation or related symmetric, square input matrix, the applicable parameters are the \code{data} parameter to specify the input matrix, \code{group} to name the two columns that contain the variable names, and \code{response} to specify the name of the variable with data values that are the input correlations or related. 

The name entered for \code{group} is repeat it for the first two columns output with a suffix of 1 and 2, respectively. For example, \code{group="Item"} becomes \code{"Item1"} and \code{"Item2"}. If not name for \code{group} is entered, the default variable names are \code{Col} and \code{Row}.
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

# input correlation matrix
R <- matrix(nrow=3, ncol=3, byrow=TRUE,
c(1.000,0.480,0.320,
  0.480,1.000,0.240,
  0.320,0.240,1.000))
colnames(R) <- c("X1", "X2", "X3")
rownames(R) <- colnames(R)
# reshape to long
dl <- reshape_long(R, shape="square", group="Item", response="Cor")
dl

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ long-form }

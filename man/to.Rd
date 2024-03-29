\name{to}
\alias{to}

\title{Create a Sequence of Numbered Variable Names with a Common Prefix and Width}

\description{
Generates sequentially numbered variable names, all starting with the same prefix, usually in conjunction with reading data values into R. The advantage over the standard R function  \code{\link{paste0}} is that \code{to} maintains equal widths of the names, such as m08 instead of m8 if some values are m10 or larger up to m99.
}

\usage{
to(prefix, until, from=1, same_size=TRUE, \dots)
}


\arguments{
  \item{prefix}{Character string that begins each variable name.}
  \item{until}{Last name in the sequence, the one with the last number.}
  \item{from}{First name in the sequence, the one with the initial number.}
  \item{same_size}{If \code{TRUE}, pads the beginning of each number for the variable
        name with leading zeros so that all names are of the same width.}
  \item{\dots}{Other parameter values.}
}


\details{
Some data sets, particularly those from surveys, have sequentially numbered variable names, each beginning with the same prefix, such as the first later of the name of a set of related attitude items. This function generates the string of such variable names, generally intended for use in a \code{read} statement for reading the data and then naming the variables, or for a subsequent assignment of the names with a \code{\link{names}}. Relies upon the R \code{\link{paste}} function.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{paste}}.
}

\examples{
# generate: "m01" "m02" "m03" "m04" "m05" "m06" "m07" "m08" "m09" "m10"
to("m", 10)

# generate: "m1"  "m2"  "m3"  "m4"  "m5"  "m6"  "m7"  "m8"  "m9"  "m10"
to("m",10, same_size=FALSE)
# equivalent to standard R function
paste0("m", 1:10)

# generate a 10 x 10 data frame
d <- data.frame(matrix(rnorm(100), nrow=10))
# name the variables in the data frame
names(d) <- to("m", 10)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ names }

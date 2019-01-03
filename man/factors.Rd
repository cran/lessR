\name{factors}
\alias{factors}
\alias{doFactors}

\title{Create Factor Variables Across a Sequential Range or Vector of Variables}

\description{Creates factors for many variables. Specify a range from a given \code{start} variable and \code{end} variable.  Applies only to variables in a data frame, \code{d} by default.
}

\usage{
factors(x, levels, labels=NULL, data=d, ordered=FALSE,
          new=FALSE, suffix=".f", var.labels=FALSE, \dots)

doFactors(\dots)
}


\arguments{
  \item{x}{Name of variable(s) to convert to a factor. List a single
        variable or a vector}
  \item{levels}{Levels for which to define the factor.}
  \item{labels}{Value labels to assign to the levels. If not
        present then assumes the character version of the levels.}
  \item{data}{The data frame of interest.}
  \item{ordered}{If \code{FALSE}, factor levels are not ordered.}
  \item{new}{If \code{FALSE}, original variables are replaced,
        otherwise new factor variables are created.}
  \item{suffix}{The appended suffix to newly created variables from 
        the original variable names when \code{new} is \code{TRUE}.}
  \item{var.labels}{Just create new variable labels for newly created
        factor variables, without doing a factor conversion,
        presumably after a previous run with factors converted to new
        factor variables.}
  \item{\dots}{Other parameter values.}
}

\details{
Invokes the base R \code{\link{Extract}}, \code{\link{which}} and  \code{\link{lapply}} functions to easily define a sequential range of variables within a data frame and then converts the specified variables to factors according to the specified levels and labels.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
# get the data, variables Gender plus m01 through m20, 20 Mach IV items
# coded as integers from 0 to 5 on 6-pt Likert scales
d <- rd("Mach4", in.lessR=TRUE, quiet=TRUE)

# single variable converted to a factor
d <- factors(Gender, 0:1, c("Male", "Female"))

# Define the labels
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                "Slightly Agree", "Agree", "Strongly Agree")

# Convert the integer responses to factors for the 20 Mach IV items
d <- factors(m01:m20, levels=0:5, labels=LikertCats)

# read the data again and this time also the variable labels
d <- rd("Mach4", in.lessR=TRUE, quiet=TRUE)
mylabels <- rd("dataMach4_lbl", in.lessR=TRUE)

# convert specified variables to factors according to the given vector
#   of three variables only
# leave the original variables unmodified, create new variables
d <- factors(c(m06, m07, m20), levels=0:5, labels=LikertCats, new=TRUE)
# now copy the variable labels from the original integer variables to the 
#   newly created factor variables
mylabels <- factors(c(m06, m07, m20), var.labels=TRUE)


}

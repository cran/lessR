\name{Recode}
\alias{Recode}
\alias{rec}

\title{Recode the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{rec}

Recodes the values of an integer or factor variable in a data frame. The factor variable represents categorical data.  The values of the factor are entered as character strings. The values of the original variable may be overwritten with the recoded values, or the recoded values can be designated to be placed in a new variable, indicated by the \code{new.name} option.

There is no provision to recode integer values to character strings because that task is best accomplished with the standard R \code{\link{factor}} function. 
}

\usage{
Recode(x, new.name=NULL, old, new, dframe=mydata)

rec(\ldots)
}

\arguments{
  \item{x}{Variable to be recoded.}
  \item{new.name}{Name of the new variable that contains the recoded values. If
       not provided, then the values of the original variable are replaced}
  \item{old}{The values of the variable that are to be recoded.}
  \item{new}{The recoded values, which match one-to-one with the values in \code{old}.}
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{\dots}{Parameter values.}
}

\details{
Specify the values to be recoded with the required \code{old} parameter, and the corresponding recoded values with the required \code{new} parameter.  Use \code{new.name} to specify the name of the variable that contains the recoded values. If \code{new.name} is not present, then the values of the original variable are overwritten with the recoded values.

Not all of the existing values of the variable to be recoded need be specified.  Any value not specified is unchanged in the values of the recoded variable.

Two diagnostic checks are performed before the recode. First, it is verified that the same number of values exist in the \code{old} and \code{new} lists of values.  Second, it is verified that all of the values specified to be recoded in fact exist in the original data. 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{transform}}, \code{\link{factor}}.
}

\examples{
# construct data frame
mydata <- read.table(text="Severity Satisfaction
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# recode Severity into a new variable called SevereNew
Recode(Severity, new.name="SevereNew", old=c(1:4), new=c(10,20,30,40))

# abbreviated form,  replace original with recoded
# another option to generate list of values
rec(Severity, old=c(1:4), new=seq(10,40,by=10))

# construct data frame
# recode Satisfaction, leave original variable unmodified
Recode(Satisfaction, new.name="SatisNew",
       old=c("Mild", "Moderate", "Severe"), new=c("M", "O", "S"))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ recode }





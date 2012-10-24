\name{Recode}
\alias{Recode}
\alias{rec}

\title{Recode the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{rec}

Recodes the values of one or more integer variables in a data frame, by default \code{mydata}. The values of the factor are entered as character strings. The values of the original variable may be overwritten with the recoded values, or the recoded values can be designated to be placed in a new variable, indicated by the \code{new.name} option. Valid values may be converted to missing, and missing values may be converted to valid values.

There is no provision to recode integer values to character strings because that task is best accomplished with the standard R \code{\link{factor}} function. 
}

\usage{
Recode(old.vars, new.vars=NULL, old, new, brief=FALSE, keep=TRUE, dframe=mydata)

rec(\ldots)
}

\arguments{
  \item{old.vars}{One or more variables to be recoded.}
  \item{new.vars}{Name of the new variable or variables that contain the recoded values. If
       not provided, then the values of the original variable are replaced.}
  \item{old}{The values of the variables that are to be recoded. If the value is
       \code{"missing"} then any existing missing values are replaced by the value specified
       with \code{new}.}
  \item{new}{The recoded values, which match one-to-one with the values in \code{old}. If the 
       value is \code{"missing"} then instead any values specified in \code{old} are
       converted to missing.}
  \item{brief}{If \code{TRUE}, then less output is provided.}
  \item{keep}{If \code{TRUE}, then the data frame with the transformed
       variable(s) replaces the input data frame.}
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{\dots}{Parameter values.}
}

\details{
Specify the values to be recoded with the required \code{old} parameter, and the corresponding recoded values with the required \code{new} parameter.  Use \code{new.name} to specify the name of the variable that contains the recoded values. If \code{new.name} is not present, then the values of the original variable are overwritten with the recoded values.

Given the focus on a single data frame within the \code{lessR} system, the input data frame has a default value of the standard \code{mydata}, and by default writes the revised data frame over the input data frame, without the need for an assignment statement. Set \code{keep=FALSE} to not have the output data frame overwrite the input data frame. To save the recoded data in this situation, explicitly assign the result of the \code{Recode}, the recoded data frame, to the specified data frame.

Not all of the existing values of the variable to be recoded need be specified.  Any value not specified is unchanged in the values of the recoded variable. Unless otherwise specified, missing values are unchanged.  To modify missing values, set \code{old="missing"} to covert missing data values to the specified value data value given in \code{new}.  Or, set \code{new="missing"} to covert the one or more existing valid data values specified in \code{old} to missing data values.

Diagnostic checks are performed before the recode. First, it is verified that the same number of values exist in the \code{old} and \code{new} lists of values.  Second, it is verified that all of the values specified to be recoded in fact exist in the original data. 

Also, if the levels of a factor were to be recoded with \code{Recode}, then the factor attribute would be lost as the resulting recoded variable would be character strings. Accordingly, this type of transformation is not allowed, and instead should be accomplished with the \code{Transform} and \code{factor} functions as shown in the examples.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{transform}}, \code{\link{factor}}.
}

\examples{
# construct data frame
mydata <- read.table(text="Severity Description
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# recode Severity into a new variable called SevereNew
Recode(Severity, new.vars="SevereNew", old=1:4, new=c(10,20,30,40))

# abbreviated form,  replace original with recoded
# another option, the sequence function, to generate list of values
rec(Severity, old=1:4, new=seq(10,40,by=10))

# reverse score four Likert variables: m01, m02, m03, m10 
# data in a different data frame than mydata
data(dataMach4)
Recode(c(m01:m03,m10), old=0:5, new=5:0, dframe=dataMach4)

# for four Likert variables, convert any 0 or 1 to missing
# use Read to put data into mydata dataframe
Read(lessR.data="Employee")
Recode(HealthPlan, old=1, new="missing")

# for four Likert variables, convert any missing value to 99
Read(lessR.data="Employee")
Recode(c(Years, Salary), old="missing", new=99)

# recode levels of a factor with the Transform and factor functions
# using Recode destroys the factor attribute, converting to
#   character strings instead, so Recode does not allow
Read(lessR.data="Employee")
Transform(
  Gender=factor(Gender, levels=c("F", "M"), labels=c("Female", "Male"))
)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ recode }







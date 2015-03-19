\name{Recode}
\alias{Recode}
\alias{rec}

\title{Recode the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{rec}

Recodes the values of one or more integer variables in a data frame. The values of the original variable may be overwritten with the recoded values, or the recoded values can be designated to be placed in a new variable, indicated by the \code{new.name} option. Valid values may be converted to missing, and missing values may be converted to valid values. Any existing variable labels are retained in the recoded data frame.

There is no provision to recode integer values to character strings because that task is best accomplished with the standard R \code{\link{factor}} function. 
}

\usage{
Recode(old.vars, new.vars=NULL, old, new, data=mydata,
       quiet=getOption("quiet"))

rec(\ldots)
}

\arguments{
  \item{old.vars}{One or more variables to be recoded.}
  \item{new.vars}{Name of the new variable or variables that contain the recoded values, each
       name in quotes. If not provided, then the values of the original variable are replaced.}
  \item{old}{The values of the variables that are to be recoded. If the value is
       \code{"missing"} then any existing missing values are replaced by the value specified
       with \code{new}.}
  \item{new}{The recoded values, which match one-to-one with the values in \code{old}. If the 
       value is \code{"missing"} then instead any values specified in \code{old} are
       converted to missing.}
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{mydata} by default.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{set}} function.}
  \item{\dots}{Parameter values.}
}

\details{
Specify the values to be recoded with the required \code{old} parameter, and the corresponding recoded values with the required \code{new} parameter.  Use \code{new.vars} to specify the name of the variable that contains the recoded values. If \code{new.vars} is not present, then the values of the original variable are overwritten with the recoded values.

Not all of the existing values of the variable to be recoded need be specified.  Any value not specified is unchanged in the values of the recoded variable. Unless otherwise specified, missing values are unchanged.  To modify missing values, set \code{old="missing"} to covert missing data values to the specified value data value given in \code{new}.  Or, set \code{new="missing"} to covert the one or more existing valid data values specified in \code{old} to missing data values.

Diagnostic checks are performed before the recode. First, it is verified that the same number of values exist in the \code{old} and \code{new} lists of values.  Second, it is verified that all of the values specified to be recoded in fact exist in the original data. 

If the levels of a factor were to be recoded with \code{Recode}, then the factor attribute would be lost as the resulting recoded variable would be character strings. Accordingly, this type of transformation is not allowed, and instead should be accomplished with the \code{Transform} and \code{factor} functions as shown in the examples.
}

\value{
The recoded data frame is returned, usually assigned the name of \code{mydata} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
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
mydata <- Recode(Severity, new.vars="SevereNew", old=1:4, new=c(10,20,30,40))

# abbreviated form,  replace original with recoded
# another option, the sequence function, to generate list of values
mydata <- rec(Severity, old=1:4, new=seq(10,40,by=10))

# reverse score four Likert variables: m01, m02, m03, m10 
mydata <- Read("Mach4", format="lessR")
mydata <- Recode(c(m01:m03,m10), old=0:5, new=5:0)

# convert any 1 for HealthPlan to missing
# use Read to put data into mydata data frame
# write results to newdata data frame
mydata <- Read("Employee", format="lessR")
newdata <- Recode(HealthPlan, old=1, new="missing")

# for Years and Salary convert any missing value to 99
mydata <- Recode(c(Years, Salary), old="missing", new=99)


# ------------------------------------
# convert between factors and integers
# ------------------------------------

# recode levels of a factor that should remain a factor
#   with the Transform and factor functions
# using Recode destroys the factor attribute, converting to
#   character strings instead, so Recode does not allow
mydata <- Read("Employee", format="lessR")
mydata <- Transform(
    Gender=factor(Gender, levels=c("F", "M"), labels=c("Female", "Male"))
)

# recode levels of a factor to convert to integer first by
#   converting to integer with Transform and as.numeric
# here Gender has values M and F in the data
# integers start with 1 through the number of levels, can use
#    Recode to change this if desired, such as to 0 and 1
mydata <- Read("Employee", format="lessR")
mydata <- Transform(Gender=as.numeric(Gender))
mydata <- Recode(Gender, old=c(1,2), new=c(0,1))

# recode integer values to levels of a factor with value labels
#   with the Transform function instead of Recode
# here Gender has values 0 and 1 in the data
mydata <- Read("Mach4", format="lessR")
mydata <- Transform(
      Gender=factor(Gender, levels=c(0,1), labels=c("Male","Female"))
      )
# ------------------------------------
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ recode }







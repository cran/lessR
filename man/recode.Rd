\name{recode}
\alias{recode}

\title{Recode the Values of an Integer or Factor Variable}

\description{
Recodes the values of one or more integer variables in a data frame. The values of the original variable may be overwritten with the recoded values, or the recoded values can be designated to be placed in a new variable, indicated by the \code{new_name} option. Valid values may be converted to missing, and missing values may be converted to valid values. Any existing variable labels are retained in the recoded data frame.

There is no provision to recode integer values to character strings because that task is best accomplished with the standard R \code{\link{factor}} function. 
}

\usage{
recode(old_vars, new_vars=NULL, old, new, data=d,
       quiet=getOption("quiet"), \ldots)
}

\arguments{
  \item{old_vars}{One or more variables to be recoded.}
  \item{new_vars}{Name of the new variable or variables that contain the recoded
       values, each name in quotes. If not provided, then the values of the
       original variable are replaced.}
  \item{old}{The values of the variables that are to be recoded. If the value is
       \code{"missing"} then any existing missing values are replaced by the
       value specified with \code{new}.}
  \item{new}{The recoded values, which match one-to-one with the values in \code{old}. If the 
       value is \code{"missing"} then instead any values specified in \code{old} are
       converted to missing.}
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{d} by default.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{\dots}{Parameter values_}
}

\details{
Specify the values to be recoded with the required \code{old} parameter, and the corresponding recoded values with the required \code{new} parameter. There must be a 1-to-1 correspondence between the two sets of values, such as 0:5 recoded to 5:0, six items in the \code{old} set and six items in the \code{new} set. 

Use \code{new_vars} to specify the name of the variable that contains the recoded values. If \code{new_vars} is not present, then the values of the original variable are overwritten with the recoded values.

Not all of the existing values of the variable to be recoded need be specified.  Any value not specified is unchanged in the values of the recoded variable. 

Unless otherwise specified, missing values are unchanged.  To modify missing values, set \code{old="missing"} to covert missing data values to the specified value data value given in \code{new}.  Or, set \code{new="missing"} to covert the one or more existing valid data values specified in \code{old} to missing data values.

Diagnostic checks are performed before the recode. First, it is verified that the same number of values exist in the \code{old} and \code{new} lists of values_  Second, it is verified that all of the values specified to be recoded in fact exist in the original data. 

If the levels of a factor were to be recoded with \code{recode}, then the factor attribute would be lost as the resulting recoded variable would be character strings. Accordingly, this type of transformation is not allowed, and instead should be accomplished with the \code{Transform} and \code{factor} functions as shown in the examples.
}

\value{
The recoded data frame is returned, usually assigned the name of \code{d} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{transform}}, \code{\link{factor}}.
}

\examples{
# construct data frame
d <- read.table(text="Severity Description
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE, stringsAsFactors=FALSE)

# recode Severity into a new variable called SevereNew
d <- recode(Severity, new_vars="SevereNew", old=1:4, new=c(10,20,30,40))

# reverse score four Likert variables: m01, m02, m03, m10 
d <- Read("Mach4")
d <- recode(c(m01:m03,m10), old=0:5, new=5:0)

# convert any 1 for Plan to missing
# use Read to put data into d data frame
# write results to newdata data frame
d <- Read("Employee")
newdata <- recode(Plan, old=1, new="missing")

# for Years and Salary convert any missing value to 99
d <- recode(c(Years, Salary), old="missing", new=99)


# ------------------------------------
# convert between factors and integers
# ------------------------------------

# recode levels of a factor that should remain a factor
#   with the Transform and factor functions
# using recode destroys the factor attribute, converting to
#   character strings instead, so Recode does not allow
d <- Read("Employee")
d <- Transform(
    Gender=factor(Gender, levels=c("F", "M"), labels=c("Female", "Male"))
)

# recode levels of a factor to convert to integer first by
#   converting to integer with Transform and as.numeric
# here Gender has values M and F in the data
# integers start with 1 through the number of levels, can use
#   recode() to change this if desired, such as to 0 and 1
d <- Transform(Gender=as.numeric(Gender))
d <- recode(Gender, old=c(1,2), new=c(0,1))

# recode integer values to levels of a factor with value labels
#   with the Transform function instead of recode()
# here Gender has values 0 and 1 in the data
d <- Read("Mach4")
d <- Transform(
      Gender=factor(Gender, levels=c(0,1), labels=c("Male","Female"))
      )
# ------------------------------------
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ recode }







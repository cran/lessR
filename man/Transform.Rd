\name{Transform}
\alias{Transform}
\alias{trans}

\title{Transform the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{trans}

Based closely on the standard R \code{\link{transform}} function. Unlike the R function, output is provided that provides feedback and guidance regarding the specified transformation(s), and variable labels are accommodated. Existing variable labels are retained in the transformed data frame. If the transformation creates a new variable, then provide a corresponding variable label with the \code{lessR} function \code{\link{label}}. 
}

\usage{
Transform(data=mydata, quiet=getOption("quiet"), \ldots)

trans(\ldots)
}

\arguments{
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{mydata} by default.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{\dots}{The list of transformations, each of the form, \code{variable = equation}. Each \code{variable} can be the name of an existing variable in the data frame or a newly created variable.}
}

\details{
The first five rows of the data frame are listed before the transformation, and the first five values of the transformed variables are listed after the transformation. The default input data frame is \code{mydata}.

Guidance and feedback regarding the transformations are provided by default. The first five lines of the input data frame are listed before the transformation, then the specified transformations are listed, followed by the first five lines of the transformed data frame.

Multiple transformations can be defined with a single statement. Note that a newly created transformed variable cannot then be used to define another transformed variable in the same \code{Transform} statement.  Instead, the transformed variable that depends on an earlier created transformed variable must be defined in its own \code{Transform} statement.
}

\value{
The transformed data frame is returned, usually assigned the name of \code{mydata} as in the examples below.  This is the default name for the data frame input into the \code{lessR} data analysis functions.
}
\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{transform}}, \code{\link{factor}}.
}

\examples{
# construct data frame
mydata <- read.table(text="Status Severity
1 Mild
4 Moderate
3 Moderate
2 Mild
1 Severe", header=TRUE)

# replace Status with a transformed version
mydata <- Transform(Status=Status-1)

# abbreviated form
# replace original with recoded
mydata <- trans(StatusNew=Status-1)

# replace Status with a transformed version
# leave input mydata unmodified
# save transformed data frame to the created data frame called newdata
newdata <- Transform(Status=Status-1)

# construct data frame
# recode Status into a factor
mydata <- Transform(Status=factor(Status, labels=c("OK","Hurts","Painful","Yikes")))

# read lessR data set dataEmployee into data frame mydata
mydata <- Read("Employee", format="lessR")
# multiple transformations in one statement
#  Months is a new variable
#  Salary is a new version of the old Salary
#  Satisfaction was read as non-numeric, so as a factor, but is also ordinal
#  HealthPlan was read as numeric values 0,1,2, now converted to a factor
mydata <- Transform(
  Months=Years*12, 
  Salary=Salary/1000,
  HealthPlan=factor(HealthPlan,
     levels=c(0,1,2), labels=c("GoodHealth", "YellowCross", "BestCare"))
)
# new variable Months now exists
# if relevant, supply a corresponding variable label
# mydata <- label(Months, "Months Employed in the Company")
# confirm
db()


# ----------------------------
# transformations with factors
# ----------------------------

# transform a nominal variable to ordinal, re-order the categories
mydata <- Transform(Satisfaction=
  factor(Satisfaction, levels=c("low", "med", "high"), ordered=TRUE))

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
# Gender is now a factor to illustrate
mydata <- Transform(Gender=as.numeric(Gender))
mydata <- Recode(Gender, old=c(1,2), new=c(0,1))

# recode integer values to levels of a factor with value labels
#   with the Transform function instead of Recode
# here Gender has values 0 and 1 in the data
mydata <- Read("Mach4", format="lessR")
mydata <- Transform(
      Gender=factor(Gender, levels=c(0,1), labels=c("Male","Female"))
      )
# ----------------------------
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ transform }







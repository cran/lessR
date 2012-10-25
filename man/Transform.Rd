\name{Transform}
\alias{Transform}
\alias{trans}

\title{Transform the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{trans}

A copy of the standard R \code{\link{transform}} function except that the transformed variable is by default written to the input data frame, which is then saved automatically.  The intent is to provide a function that is easier to use and suffices when the focus is on a single data frame. Also, output is provided that provides feedback and guidance regarding the specified transformation(s).
}

\usage{
Transform(dframe=mydata, brief=FALSE, keep=TRUE, \ldots)

trans(\ldots)
}

\arguments{
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{brief}{If \code{TRUE}, then no text output is provided.}
  \item{keep}{If \code{TRUE}, the default, then the data frame with the transformed
       variable(s) replaces the input data frame.}
  \item{\dots}{The list of transformations, each of the form, \code{variable = equation}. Each \code{variable} can be the name of an existing variable in the data frame or a newly created variable.}
}

\details{
The standard \code{\link{transform}} function creates a transformation based on one or more variables in the input data frame, which must be specified, and this is saved in the output data frame specified with an assignment statement. The first five rows of the data frame are listed before the transformation, and the first five values of the transformed variables are listed after the transformation.

Given the focus on a single data frame within the \code{lessR} system, the input data frame has a default value of the standard \code{mydata}, and by default writes the revised data frame over the input data frame, without the need for an assignment statement. Set \code{keep=FALSE} to not have the output data frame overwrite the input data frame. To save the transformed data in this situation, explicitly assign the result of the \code{Transform}, the transformed data frame, to the specified data frame, as shown in the examples.

Also guidance and feedback regarding the transformations are provided by default. The first six lines of the input data frame are listed before the transformation, then the specified transformations are listed, followed by the first six lines of the transformed data frame.

Multiple transformations can be defined with a single statement. Note that a newly created transformed variable cannot then be used to define another transformed variable in the same \code{Transform} statement.  Instead, the transformed variable that depends on an earlier created transformed variable must be defined in its own \code{Transform} statement.
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
Transform(Status=Status-1)

# abbreviated form
# replace original with recoded
trans(StatusNew=Status-1)

# replace Status with a transformed version
# leave input mydata unmodified
# save transformed data frame to the created data frame called newdata
newdata <- Transform(Status=Status-1, keep=FALSE)

# construct data frame
# recode Status into a factor
Transform(Status=factor(Status, labels=c("OK","Hurts","Painful","Yikes")))

# read lessR data set datEmployee into data frame mydata
Read(lessR.data="Employee")
# multiple transformations in one statement
#  Months is a new variable
#  Salary is a new version of the old Salary
#  Satisfaction was read as non-numeric, so as a factor, but now is ordinal
#  HealthPlan was read as numeric values 0,1,2, now converted to a factor
Transform(
  Months=Years*12, 
  Salary=Salary/1000,
  Satisfaction=factor(Satisfaction, levels=c("low", "med", "high"), ordered=TRUE),
  HealthPlan=factor(HealthPlan,
     levels=c(0,1,2), labels=c("GoodHealth", "YellowCross", "BestCare"))
)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ transform }







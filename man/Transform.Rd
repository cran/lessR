\name{Transform}
\alias{Transform}
\alias{trans}

\title{Transform the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{trans}

A copy of the standard R \code{\link{transform}} function except that the transformed variable is by default written to the input data frame, which is then saved automatically.  The intent is to provide a function that is easier to use and suffices when the focus is on a single data frame. Also, output is provided that provides feedback and guidance regarding the specified transformation(s).
}

\usage{
Transform(dframe=mydata, brief=FALSE, save.dframe=TRUE, \ldots)

trans(\ldots)
}

\arguments{
  \item{dframe}{The name of the data frame that contains the variable with
       values to be recoded, which is \code{mydata} by default.}
  \item{brief}{If \code{TRUE}, then no text output is provided.}
  \item{save.dframe}{If \code{TRUE}, then the data frame with the transformed
       variable(s) replaces the input data frame.}
  \item{\dots}{The list of transformations, each of the form, \code{variable = equation}. Each \code{variable} can be the name of an existing variable in the data frame or a newly created variable.}
}

\details{
The standard \code{\link{transform}} function creates a transformation based on one or more variables in the input data frame, which must be specified, and this is saved in the output data frame specified with an assignment statement.  Given the focus on a single data frame within the \code{lessR} system, the input data frame has a default value of the standard \code{mydata}, and by default writes the revised data frame over the input data frame, without the need for an assignment statement. However, the behavior of the standard  \code{\link{transform}} function can be mimicked by setting \code{save.dframerame=FALSE}.

Also guidance and feedback regarding the transformations are provided by default. The first six lines of the input data frame are listed before the transformation, then the specified transformations are listed, followed by the first six lines of the transformed data frame.
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

# replace Severity with a transformed version
Transform(Severity=Severity-1)

# abbreviated form
# replace original with recoded
trans(SeverityNew=Severity-1)

# replace Severity with a transformed version
# leave input mydata unmodified
# save transformed data frame to the created data frame called newdata
newdata <- Transform(Severity=Severity-1, save.dframe=FALSE)

# construct data frame
# recode Severity into a factor
Transform(Severity=factor(Severity, labels=c("OK","Hurts","Painful","Yikes")))

# data in a different data frame than mydata
# multiple transformations in one statement
data(datEmployee)
Transform(Years=Years+100, 
          Salary000=Salary/1000,
          HealthPlan=factor(HealthPlan, labels=c("lo", "hi", "best")),
          dframe=datEmployee)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ transform }







\name{VariableLabels}
\alias{VariableLabels}
\alias{vl}

\title{Return or Assign a Variable Label}

\description{
Assign and/or display variable labels, displayed in the output of analyses either as text output at the console or graphics, such as an axis label on a graph.  The variable labels can be assigned individually, or for some or all variables. To assign a single variable label, invoke the \code{value} option and assign the output to a specified data frame. 
}

\usage{
VariableLabels(x, value=NULL, data=mydata, quiet=getOption("quiet"))
}


\arguments{
  \item{x}{The file reference or character string variable (see examples)
    from which to obtain the variable labels, or a variable name for which
    to assign or obtain the corresponding variable label.}
  \item{value}{The variable label assigned to a specific variable,
  otherwise \code{NULL}.}
  \item{data}{Data frame that contains the variable(s) of interest. The output
    of the function is assigned to this data frame.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system
    default with \code{\link{set}} function.}

vl{\ldots}
}


\details{
Standard R does not provide for variable labels, but \code{lessR} does.  Read the labels by specifying a file reference as the first argument of the function call, or read the labels with the \code{lessR} \code{\link{Read}} function, as explained in the corresponding documentation. Individual variable labels can also be assigned with this function. Not all variables need have a label, and the variables with their corresponding labels can be listed or assigned in any order.

When all or some of the labels are read, either from the console or an external \code{csv} or \code{Excel} file, each line of the file contains the variable name and then the associated variable label. The file types of \code{.csv} and \code{.xlsx} in the file reference listed in the first position of the function call are what trigger the interpretation of the argument as a file reference. 
}

\value{
The data frame with the variable labels is returned. 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}.
}

\examples{
# read the internal lessR data frame that contains variable labels
mydata <- Read("Employee", format="lessR", brief=TRUE)
# processing with a standard R transformation removes the labels
mydata <- transform(mydata, Years = Years)
# show that the labels are now gone
details.brief()


# construct and read variable labels from console
lbl <- "
Years,time of company employment
Gender,Male or Female
Dept,department employed
Salary,annual salary
Satisfaction,satisfaction with work environment
HealthPlan,1=GoodHealth 2=YellowCross 3=BestCare
"
mydata <- VariableLabels(lbl)

# read variable labels from a csv file
mydata <- Read("Employee", format="lessR", brief=TRUE)
mydata <- transform(mydata, Years = Years)  # remove the labels 
# mydata <- VariableLabels("http://lessRstats.com/data/employee_lbl.csv")

# add/modify a single variable label
mydata <- VariableLabels(Salary, "bucks buck")
details.brief()

# list the contents of a single variable label
VariableLabels(Salary)

# display all variable labels
VariableLabels()

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labels }
\keyword{ csv }





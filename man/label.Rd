\name{label}
\alias{label}

\title{Assign Variable Labels [Superseded by VariableLabels]}

\description{
Deprecated, replaced by \code{\link{VariableLabels}}. Display a variable label for output, either text output at the console or graphics, such as a title on a graph.  To return a variable label generally applies to standard R functions such that the functions can access \code{lessR} variable labels. Or, the variable name and label can be listed on the standard output. To assign a variable label, invoke the \code{value} option and assign the output to a specified data frame. 
}

\usage{
label(x, value=NULL, data=d)
}


\arguments{
  \item{x}{The variable for which to obtain the corresponding variable label.}
  \item{value}{If assigned, then the specified data frame is updated with this
       assigned label.}
  \item{data}{Data frame that contains the variable of interest. The output
       of the function is assigned to this data frame.}
}


\details{
Standard R does not provide for variable labels, but \code{lessR} does.  Read the labels with the \code{lessR} \code{\link{Read}} function, as explained in the corresponding documentation. Individual variable labels can also be assigned with this function. Not all variables need have a label, and the variables with their corresponding labels can be listed or assigned in any order.

The function provides two different modes. The first mode is to return the variable name and label for an existing variable label. One such use is to provide the function as an argument to an existing R function call to access a \code{lessR} variable label. For example, use the function as the argument for \code{main} in graphics output, where \code{main} is the title of the graph. This mode is triggered by not invoking the \code{value} option.

The second mode is to assign a variable label to an existing variable. Invoke this mode by specifying a variable label with the \code{value} option. The function accesses the entire specified data frame, and then modifies the specified variable label. As such, assign the output of the function to the data frame of interest, such as the default \code{d}. One use of this function is to add a variable label to a data frame that contains a new variable created by a transformation of the existing variables. 
}

\value{
The specified value of \code{value} is returned.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}.
}

\examples{
# read the data and variable labels
#d <- rd("http://lessRstats.com/data/employee.xlsx")
#l <- vl("http://lessRstats.com/data/employee_lbl.xlsx")

# variable label as the title of a graph for non-lessR functions
# base R
#hist(d$Salary, xlab=label(Salary))
# ggplot2
#ggplot(d, aes(Salary)) + geom_histogram(binwidth=10000) + labs(x=label(Salary))

# assign a new label for the variable Years in d
#d <- label(Years, "Years Worked")
# verify
#label(Years)
# or view all variable labels in d
#db()

#d <- Read("Employee")
# specify a label of variable in a data frame other than d
#myd <- Subset(Gender=="M")
#myd <- label(Gender, "Only is Male", data=myd)
#db(myd)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labels }
\keyword{ csv }





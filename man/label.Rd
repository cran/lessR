\name{label}
\alias{label}

\title{Apply a Variable Label to a non-lessR Function}

\description{
Display a variable label for output, either text output at the console or graphics, such as a title on a graph.  Generally applies to standard R functions as \code{lessR} functions automatically provide variable labels on the output when the labels have been read with the data.
}

\usage{
label(x, data=mydata)
}


\arguments{
  \item{x}{The variable for which to obtain the corresponding variable label.}
  \item{data}{Data frame that contains the variable of interest.}
}


\details{
Standard R does not provide for variable labels, but \code{lessR}does.  Read the labels \code{\link{Read}} function and setting \code{labels=TRUE}. This setting can be accomplished with the short name for the \code{\link{Read}} function, \code{\link{rd}}.

Each row of the file that contains the labels, including the first row, consists of the variable name, a comma, and then the label, that is, standard \code{csv} format such as obtained with the \code{csv} option from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, \code{mydata} by default, need have a label, and the variables with their corresponding labels can be listed in any order. Here is an example of this file for the \code{dat.employee} data file included in this \code{lessR} package.\cr

Years,"Years Employed in the Company"\cr
Gender,"Male or Female"\cr
Dept,"Department Employed"\cr
Salary,"Annual Salary (USD)"\cr
Satisfaction,"Satisfaction with Work Environment"\cr
HealthPlan,"1=GoodHealth, 2=YellowCross, 3=BestCare"\cr
Not all variables need have a label, and the variables with their corresponding labels can be listed in any order.

The \code{label} function is automatically accessed by the \code{lessR} functions that provide data analysis, such as automatically providing the title of a graph as the corresponding variable label.  This function can also be added to standard R function calls as well, such as an argument for \code{main} in graphics output, where \code{main} is the title of the graph.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}.
}

\examples{
# read the data with variable labels
mydata <- Read("Employee", format="lessR")

# variable label as the title of a graph from a standard R function
# the data are not attached, so for standard R functions, must
#   identify the relevant data frame, such as with function: with
with(mydata, barplot(table(Dept), main=label(Dept)))
with(mydata, hist(Salary, main=label(Salary)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labels }
\keyword{ csv }





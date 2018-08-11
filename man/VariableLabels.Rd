\name{VariableLabels}
\alias{VariableLabels}
\alias{vl}

\title{Create or Display Variable Labels}

\description{
Assign and/or display variable labels stored in the data frame \code{mylabels}. Variable labels enhance output of analyses either as text output at the console or as graphics, such as an axis label on a graph.  The variable labels can be assigned individually, or for some or all variables. 
}

\usage{
VariableLabels(x, value=NULL, quiet=getOption("quiet"))
}


\arguments{
  \item{x}{The file reference or character string variable (see examples)
    from which to obtain the variable labels, or a variable name for which
    to assign or obtain the corresponding variable label in conjunction with
    the \code{value} parameter. Can also be a data frame from which to
    extract any existing variable labels.}
  \item{value}{The variable label assigned to a specific variable,
  otherwise \code{NULL}.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system
    default with \code{\link{style}} function.}

vl{\ldots}
}


\details{
Unlike standard R, \code{lessR} provides for variable labels, here stored in the data frame \code{mylabels}. To read the labels from an external file, specify a file reference as the first argument of the function call. Or create a character string of variable names and labels and specify the character string as the first argument to the function call. To assign an individual variable label with this function specify the variable name as the first argument followed by the label in quotes. Not all variables need have a label, and the variables with their corresponding labels can be listed or assigned in any order. If the \code{mylabels} data frame is created or modified, the output of the function must be assigned to \code{mylabels}, as shown in the following examples.

When all or some of the labels are read, either from the console or an external \code{csv} or \code{Excel} file, each line of the file contains the variable name and then the associated variable label. The file types of \code{.csv} and \code{.xlsx} in the file reference listed in the first position of the function call are what trigger the interpretation of the argument as a file reference. 

For a file that contains only labels, each row of the file, including the first row, consists of the variable name, a comma if a \code{csv} file, and then the label. For the \code{csv} form of the file, this is the standard \code{csv} format such as obtained with the \code{csv} option from a standard worksheet application such as Microsoft Excel or LibreOffice Calc. Not all variables in the data frame that contains the data, usually \code{mydata}, need have a label, and the variables with their corresponding labels can be listed in any order. An example of this file follows for four variables, I1 through I4, and their associated labels.

I2,This instructor presents material in a clear and organized manner.\cr
I4,Overall, this instructor was highly effective in this class.\cr
I1,This instructor has command of the subject.\cr
I3,This instructor relates course materials to real world situations.\cr

If there is a comma in the variable label, then the label needs to be enclosed in quotes.

The \code{lessR} functions that provide analysis, such as \code{\link{Histogram}} for a histogram, automatically include the variable labels in their output, such as the title of a graph.  Standard R functions can also use these variable labels by invoking the \code{lessR} function \code{\link{label}}, such as setting \code{main=label(I4)} to put the variable label for a variable named I4 in the title of a graph.  

Variable units may also be added to the third column of a variable label file. These are used for generating a better natural language text in the generation of R~Markdown files with the \code{Rmd} option on supporting functions such as \code{\link{Regression}}. For currency (USD), indicate with unit: dollar.
a
}

\value{
The data frame with the variable labels is returned. 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Read}}.
}

\examples{

# read file and then variable labels from csv files
# mylabels <- Read("http://lessRstats.com/data/employee.csv")
# mylabels <- VariableLabels("http://lessRstats.com/data/employee_lbl.csv")

# construct and read variable labels from console
lbl <- "
Years, Years of Company Employment
Gender, Male or Female
Dept, Department Employed
Salary, Annual Salary (USD)
JobSat, JobSat with Work Environment
Plan, 1=GoodHealth 2=YellowCross 3=BestCare
"
mylabels <- VariableLabels(lbl)
mylabels

# add/modify a single variable label
mylabels <- VariableLabels(Salary, "Annual Salaries in USD")
mylabels

# list the contents of a single variable label
VariableLabels(Salary)

# display all variable labels
VariableLabels()

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labels }
\keyword{ csv }


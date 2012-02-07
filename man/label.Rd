\name{label}
\alias{label}

\title{Get a Variable Label for Display on Output}

\description{
Display a variable label for output, either text output at the console or graphics, such as a title on a graph.  Generally applies to standard R functions as \code{lessR} functions automatically provide varible labels on the output when the labels are present in the \code{mylabels} data frame.
}

\usage{
label(x, dframe=mylabels)
}


\arguments{
  \item{x}{The variable for which to obtain the corresponding variable label.}
  \item{dframe}{Data frame that contains the variable of interest, which is \code{mylabels}.}
}


\details{
Standard R does not provide for variable labels, but \code{lessR} provides for a data frame called \code{mylabels} which stores variable labels.  Read the labels into this data frame with a version of the \code{\link{rad}} function.  The variable labels can either be in a separate file by themselves, \code{\link{rad.labels}}, or in the second row of the data file, \code{\link{rad.both}}. Each row of the file that contains the labels, including the first row, consists of the variable name, a comma, and then the label, that is, standard \code{csv} format. Not all variables need have a label, and the variables with their corresponding labels can be listed in any order.

The \code{\link{rad}} function automatically creates the \code{mylabels} data frame. The structure of the data frame consists of the variable names which serve as the row names.  The labels themselves are a variable named \code{label}, but this information is only necessary if constructing the labels data frame manually, such as is done in the examples.

This function is automatically accessed by the \code{lessR} functions that provide data analysis, such as automatically providing the title of a graph as the corresponding variable label.  This function can also be added to standard R function calls as well, such as an argument for \code{main} in graphics output, where \code{main} is the title of the graph.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{rad}}, \code{\link{plot}}.
}

\examples{
# create data frame mydata (usually read from a file with rad)
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(X,Y)
rm(X); rm(Y);

# create data frame mylabels (usually read from a file with rad)
vname <- character(length=2);  vname[1] <- "X";  vname[2] <- "Y"
label <- character(length=2)
label[1] <- "Grouping Variable"
label[2] <- "Response Variable"
mylabels <- data.frame(label)
row.names(mylabels) <- vname

# variable label as the title of a graph from a standard R function
# the data are not attached, so identify using the with function
with(mydata, barplot(table(X), main=label(X)))
with(mydata, hist(Y, main=label(Y)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labels }
\keyword{ csv }

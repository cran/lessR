\name{Help}
\alias{Help}
\alias{hlp}

\title{Help System for Statistics by Topic that Suggests Related Functions}

\description{
Abbreviation: \code{hlp}

R works by entering function names and arguments.  R provides extensive help for each available function based on a function's name, but these names are not apparent to someone who has not memorized them or has access to their definitions.  To alleviate this problem, this help system suggests and briefly explains various function calls regarding a requested topic for statistical analysis.
}

\usage{
Help(topic=NULL)

hlp(\ldots)
}


\arguments{
  \item{topic}{Message reference, either null to specify a list of available topics or a specific argument to reference a specific help message.}
  \item{\dots}{Other parameter values to be passed to Help.}
}

\details{
\code{Help()} displays a list of help topics, listed below.

\code{Help("topic")} generally displays the available functions relevant for the specified topic.

\code{Help("help.to.pdf")} generates a pdf file of the full contents of all the help topics.

\code{Help("lessR")} calls up the link to the lessR manual and news, which includes current updates.

---

\code{data}: Create a csv data file from a worksheet application.

\code{Read}: Read an external data file in csv format.

\code{Write}: Write the contents of mydata to a data file in csv format.

\code{library}: Many libraries of functions developed by others can be added to R.

---

\code{Histogram}: Histogram, box plot, dot plot and density curve..

\code{BarChart}: Bar chart or pie chart of a categorical variable.

\code{Plot}: Scatterplot for one or two variables, line chart.

\code{RunChart}: Run chart or time series chart.

---

\code{SummaryStats}: Summary statistics for one and two variables.

\code{one.sample}: Analysis of a single sample of data.

\code{ttest}: The mean difference and related statistics.

\code{ANOVA}: Compare means across two or more groups.

\code{power}: Power analysis for the t-test.

\code{Correlation}: Correlation analysis.

\code{Regression}: Regression analysis.

---

\code{prob}: Probabilities for Normal and t-distributions.

\code{random}: Random number generation.

\code{sample}: Generate random samples.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{help}}.
}

\examples{
# list the information needed to access a specific topic
Help()
# short name
hlp()

# specific help message regarding summary statistics
Help("stats")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ help }



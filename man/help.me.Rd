\name{help.me}
\alias{help.me}

\title{Help System for Statistics by Topic that Suggests Related Functions}

\description{
R works by entering function names and arguments.  R provides extensive help for each available function based on a function's name, but these names are not apparent to someone who has not memorized them or has access to their definitions.  To alleviate this problem, this help system suggests and briefly explains various function calls regarding a requested topic for statistical analysis.
}

\usage{
help.me(topic=NULL)
}


\arguments{
  \item{topic}{Message reference, either null to specify a list of available topics or a specific argument to reference a specific help message.}
}


\details{
\code{help.me()} displays a list of help topics, listed below.

\code{help.me("topic")} generally displays the available functions relevant for the specified topic.

\code{help.me("help.to.pdf")} generates a pdf file of the full contents of all the help topics.

---

\code{data}: Create a csv data file from a worksheet application.

\code{read}: Read an external data file in csv format.

\code{write}: Write the contents of mydata to a data file in csv format.

\code{library}: Many libraries of functions developed by others can be added to R.

---

\code{prob}: Probabilities for Normal and t-distributions.

\code{random}: Random number generation.

\code{sample}: Generate random samples.

---

\code{histogram}: Histogram of a numeric variable.

\code{bar.chart}: Bar chart of a categorical variable.

\code{plot}: Run charts, scatterplots, function graphs.

---

\code{stats}: Summary statistics.

\code{one.sample}: Analysis of a single sample of data.

\code{two.samples}: The mean difference and related statistics.

\code{many.samples}: Compare means across two or more groups.

\code{props}: Compare proportions across two or more groups.

\code{power}: Power analysis for the t-test.

\code{cor}: Correlation analysis.

\code{reg}: Regression analysis.
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{help}}.
}

\examples{
# list the information needed to access a specific topic
help.me()

# specific help message regarding summary statistics
help.me("stats")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ help }

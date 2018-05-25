\name{Help}
\alias{Help}

\title{Help System for Statistics by Topic that Suggests Related Functions}

\description{

R works by entering function names and arguments.  R provides extensive help for each available function based on a function's name, but these names are not apparent to someone who has not memorized the names.  To alleviate this problem, this help system suggests and briefly explains various function calls regarding a requested topic for statistical analysis. The primary call is \code{Help()}, which displays the main help menu in a persistent graphics window, that is, which remains until explicitly closed by the user regardless of additional graphics analyses. Each specific Help window can be called with any of the function names, or their abbreviations or related expressions. The argument can be expressed in any combination of uppercase or lowercase letters.
}

\usage{
Help(topic=NULL, width=4.5, height=4.5)
}


\arguments{
  \item{topic}{Message reference, either null to specify a list of available topics or a specific argument to reference a specific help message.}
  \item{width}{Width of Help window.}
  \item{height}{Height of Help window.}
}

\details{
\code{Help()} displays a list of help topics, listed below.

\code{Help("topic")} generally displays the available functions relevant for the specified topic. If the name of the topic is a \code{lessR} statistical function, the abbreviated form of the name can also be invoked.

\code{Help("lessR")} calls up the link to the lessR manual and news, which includes current updates.

---

\code{style}: System level settings, such as a color theme for graphics.

\code{data}: Create a csv data file from a worksheet application.

\code{Read}: Read an external data file in csv format.

\code{Write}: Write the contents of mydata to a data file in csv format.

\code{library}: Many libraries of functions developed by others can be added to R.

\code{transform}: Edit and create new variables from existing variables.

---

\code{Histogram}: Histogram, box plot, dot plot and density curve..

\code{BarChart}: Bar chart or pie chart of a categorical variable.

\code{LineChart}: Run chart or time series chart.

\code{Plot}: Scatterplot for one or two variables, line chart.

---

\code{SummaryStats}: Summary statistics for one and two variables.

\code{one.sample}: Analysis of a single sample of data.

\code{ttest}: The mean difference and related statistics.

\code{ANOVA}: Compare means across two or more groups.

\code{power}: Power analysis for the t-test.

\code{Correlation}: Correlation analysis.

\code{Regression}: Regression analysis.

\code{Logit}: Logistic regression analysis.

\code{factor.analysis}: Confirmatory or exploratory factor analysis.

---

\code{prob}: Probabilities for Normal and t-distributions.

\code{random}: Random number generation.

\code{sample}: Generate random samples.

---

\code{lessR}: lessR manual and list of updates to current version
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{help}}.
}

\examples{
# list the information needed to access a specific topic
Help()

# specific help message regarding a histogram and related functions
Help(Histogram)

# other ways to call the same help message
#Help(histogram)  # case insensitive
#Help(hs)
#Help(hist)
#Help(hst)
#Help(boxplot)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ help }



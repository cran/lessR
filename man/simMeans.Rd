\name{simMeans}
\alias{simMeans}

\title{Pedagogical Simulation of Sample Means over Repeated Samples}

\description{
Show a sequence of sample means and data, all simulated from the same normal population. Useful for developing an intuition for developing an informal confidence interval, that is, specifying a likely range of values that contain the true population mean, but without a formal probability.
}

\usage{
simMeans(ns, n, mu=0, sigma=1, ylim.bound=NULL, 
         show.title=TRUE, show.data=TRUE, max.data=10, 
         col.grid="grey90", pause=FALSE,
         sort=NULL, set.mu=FALSE, digits.d=2,
         main=NULL, pdf.file=NULL, pdf.width=5, pdf.height=5)
}


\arguments{
  \item{ns}{Number of samples, that is, repetitions of the experiment.}
  \item{n}{Size of each sample.}
  \item{mu}{Population mean.}
  \item{sigma}{Population standard deviation.}
  \item{ylim.bound}{Specify the maximum deviation of the mean in either direction for the extent of the vertical axis.}
  \item{show.title}{Place a title on the graph that contains the parameter values.}
  \item{show.data}{Show the data values on the text output.}
  \item{max.data}{Maximum number of data values per sample on the text output.}
  \item{col.grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output sample by sample.}
  \item{sort}{Sort the output by the means in ascending order. By default is \code{TRUE} unless \code{se.mu} or \code{pause} is \code{TRUE}}
  \item{set.mu}{Have the program randomly set \code{mu} and \code{sigma}, usually to guess the correct value.}
  \item{digits.d}{Sort the output by the means in ascending order.}
  \item{main}{Title of graph.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
}


\details{
Simulate random normal data and display the resulting sample means, both as text output and graphic output.

If \code{pause=TRUE}, then the true population values are not revealed as the simulation progresses. These values are saved in the user's workspace and can be revealed by entering their names at the user prompt, \code{mu} and \code{sigma}. 
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 8 samples, each with a sample size of 10
# mu=0, sigma=1, that is, sample from the standard normal
simMeans(8, 10)

# 25 sample means with a sample size each of 100
# mu=100, sigma=15
# pause after each interval and show the data
simMeans(25, 100, mu=100, sigma=15, show.data=FALSE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ confidence interval }

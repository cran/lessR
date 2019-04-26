\name{simMeans}
\alias{simMeans}

\title{Pedagogical Simulation of Sample Means over Repeated Samples}

\description{
Show a sequence of sample means and data, all simulated from the same normal population. Useful for developing an intuition for developing an informal confidence interval, that is, specifying a likely range of values that contain the true population mean, but without a formal probability.
}

\usage{
simMeans(ns, n, mu=0, sigma=1, ylim_bound=NULL, 
         show_title=TRUE, show_data=TRUE, max_data=10, 
         grid="grey90", pause=FALSE,
         sort=NULL, set_mu=FALSE, digits_d=2,
         main=NULL, pdf_file=NULL, width=5, height=5, \ldots)
}


\arguments{
  \item{ns}{Number of samples, that is, repetitions of the experiment.}
  \item{n}{Size of each sample.}
  \item{mu}{Population mean.}
  \item{sigma}{Population standard deviation.}
  \item{ylim_bound}{Specify the maximum deviation of the mean in either direction for the extent of the vertical axis_}
  \item{show_title}{Place a title on the graph that contains the parameter values_}
  \item{show_data}{Show the data values on the text output.}
  \item{max_data}{Maximum number of data values per sample on the text output.}
  \item{grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output sample by sample.}
  \item{sort}{Sort the output by the means in ascending order. By default is \code{TRUE} unless \code{se.mu} or \code{pause} is \code{TRUE}}
  \item{set_mu}{Have the program randomly set \code{mu} and \code{sigma}, usually to guess the correct value.}
  \item{digits_d}{Sort the output by the means in ascending order.}
  \item{main}{Title of graph.}
  \item{pdf_file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values, possibly invalid.} 
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
simMeans(25, 100, mu=100, sigma=15, show_data=FALSE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ confidence interval }

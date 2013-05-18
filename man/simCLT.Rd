\name{simCLT}
\alias{simCLT}

\title{Pedagogical Simulation for the Central Limit Theorem}

\description{
Show the distribution of sample means and relevant summary statistics, such as the 95\% range of variation. Provide a plot of both the specified population and the corresponding distribution of sample means.
}

\usage{
simCLT(ns, n, p1=0, p2=1,
       type=c("normal", "uniform", "lognormal", "antinormal"),
       col.fill="lightsteelblue3", n.display=2, digits.d=3, 
       subtitle=TRUE, pop=TRUE, 
       main=NULL, pdf=FALSE, pdf.width=5, pdf.height=5, \ldots)
}


\arguments{
  \item{ns}{Number of samples, that is, repetitions of the experiment.}
  \item{n}{Size of each sample.}
  \item{p1}{First parameter value for the population distribution, the mean, minimum or meanlog for the normal, uniform and lognormal populations, respectively. Must be 0, the minimum, for the anti-normal distribution.}
  \item{p2}{Second parameter value for the population distribution, the standard deviation, maximum or sdlog for the normal, uniform and lognormal populations, respectively. Is the maximum for the anti-normal, usually left at the default value of 1.}
  \item{type}{The general population distribution.}
  \item{col.fill}{Fill color of the graphs.}
  \item{n.display}{Number of samples for which to display the sample mean and data values.}
  \item{digits.d}{Number of decimal digits to display on the output.}
  \item{subtitle}{If \code{TRUE}, then display the specific parameter values of the population or sample, depending on the graph.}
  \item{pop}{If \code{TRUE}, then display the graph of the population from which the data are sampled.}
  \item{main}{Title of graph.}
  \item{pdf}{Indicator as to if the graphic files should be saved as pdf files
       instead of directed to the standard graphics windows.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for R function \code{\link{lm}} which provides the core computations.} 
}


\details{
Provide a plot of both the specified population and the corresponding distribution of sample means. Include descriptive statistics including the 95\% range of sampling variation in raw units and standard errors for comparison to the normal distribution.  Also provide a few samples of the data and corresponding means.

Four different populations are provided: normal, uniform, lognormal for a skewed distribution, and what is called the anti-normal, the combining of two side-by-side triangular distributions so that most of the values are in the extremes and fewer values are close to the middle.

For the lognormal distribution, increase the skew by increasing the value of \code{p2}, which is the population standard deviation.

The anti-normal distribution requires the \code{triangle} package. No population mean and standard deviation are provided for the anti-normal distribution, so the 95\% range of sampling variable of the sample mean in terms of standard errors is not provided.  ** Not activated until the triangle package is updated. **

If the two plots, of the population and sample distributions respectively, are written to pdf files, according to \code{pdf=TRUE}, they are named SimPopulation.pdf and SimSample.pdf.  Their names and the directory to which they are written are provided as part the console output.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# plot of the standardized normal 
#  and corresponding sampling distribution with 10000 samples
#  each of size 2
simCLT(ns=1000, n=2)

# plot of the uniform dist from 0 to 4
#  and corresponding sampling distribution with 10000 samples
#  each of size 2
simCLT(ns=1000, n=2, p1=0, p2=4, type="uniform", bin.width=0.01)

# save the population and sample distributions to pdf files
simCLT(100, 10, pdf=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ central limit theorem }

\name{sim.CImean}
\alias{sim.CImean}

\title{Pedagogical Simulation for the Confidence Interval of the Mean}

\description{
Show a sequence of confidence intervals, all calculated from repeated samples of simulated data from the same normal population, and show which intervals contain the true population mean.
}

\usage{
sim.CImean(ns, n, mu=0, sigma=1, cl=0.95, 
         ylim.bound=NULL, show.data=FALSE, show.title=TRUE, 
         miss.only=FALSE, col.hit="gray40", col.miss="red",
         col.grid="grey90", pause=FALSE)
}


\arguments{
  \item{ns}{Number of samples, that is, repetitions of the experiment.}
  \item{n}{Size of each sample.}
  \item{mu}{Population mean.}
  \item{sigma}{Population standard deviation.}
  \item{cl}{Confidence level.}
  \item{ylim.bound}{Specify the maximum deviation of the mean in either direction for the extent of the vertical axis.}
  \item{show.data}{Plot the data for each sample as well as the confidence interval.}
  \item{show.title}{Place a title on the graph that contains the parameter values.}
  \item{miss.only}{For the text output, only display information for samples that missed the mean.}
  \item{col.hit}{Color of the confidence intervals that contains the mean.}
  \item{col.miss}{Color of the confidence intervals that miss the mean.}
  \item{col.grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output confidence interval by confidence interval.}
}


\details{
Simulate random normal data and display the resulting confidence intervals.}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 25 confidence intervals with a sample size each of 100
# mu=0, sigma=1, that is, sample from the standard normal
sim.CImean(25, 100)

# 25 confidence intervals with a sample size each of 100
# mu=100, sigma=15
# pause after each interval and show the data
sim.CImean(25, 100, mu=100, sigma=15, show.data=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ confidence interval }

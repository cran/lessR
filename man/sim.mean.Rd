\name{sim.mean}
\alias{sim.mean}

\title{Pedagogical Simulation of Sample Means}

\description{
Show a sequence of sample means and data, all simulated from the same normal population. Useful for developing an intuituion for developing an informal confidence interval, that is, specifying a likely range of values that contain the true population mean, but without a formal probability.
}

\usage{
sim.mean(nrep, n, mu=0, sigma=1, ylim.bound=NULL, 
         show.title=TRUE, show.pop=FALSE, show.data=TRUE, 
         col.grid="grey90", pdf.out=FALSE, pause=FALSE)
}


\arguments{
  \item{nrep}{Number of samples, that is, repetitions of the experiment.}
  \item{n}{Size of each sample.}
  \item{mu}{Population mean.}
  \item{sigma}{Population standar deviation.}
  \item{ylim.bound}{Specify the maximum deviation of the mean in either direction for the extent of the vertical axis.}
  \item{show.title}{Place a title on the graph that contains the parameter values.}
  \item{show.pop}{Show the population values.}
  \item{show.data}{Show the data values on the text output.}
  \item{col.grid}{Color of the grid lines.}
  \item{pdf.out}{Generate a pdf file with an appropriate title.}
  \item{pause}{Build the graph and the text output confidence interval by confidence interval.}
}


\details{
Simulate random normal data and display the resulting sample means, both as text output and graphic output.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 8 samples, each with a sample size of 10
# mu=0, sigma=1, that is, sample from the standard normal
sim.mean(8, 10)

# 25 sample means with a sample size each of 100
# mu=100, sigma=15
# pause after each interval and show the data
sim.mean(25, 100, mu=100, sigma=15, show.data=FALSE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ confidence interval }

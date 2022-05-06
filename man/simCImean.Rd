\name{simCImean}
\alias{simCImean}

\title{Pedagogical Simulation for the Confidence Interval of the Mean}

\description{
Show a sequence of confidence intervals, all calculated from repeated samples of simulated data from the same normal population, and show which intervals contain the true population mean.
}

\usage{
simCImean(ns, n, mu=0, sigma=1, cl=0.95, seed=NULL, 
         show_data=FALSE, show_title=TRUE, 
         miss_only=FALSE, color_hit="gray40", color_miss="red",
         grid="grey90", ylim_bound=NULL, pause=FALSE,
         main=NULL, pdf_file=NULL, width=5, height=5, \dots)
}


\arguments{
  \item{ns}{Number of samples, that is, repetitions of the experiment.}
  \item{n}{Size of each sample.}
  \item{mu}{Population mean.}
  \item{sigma}{Population standard deviation.}
  \item{cl}{Confidence level.}
  \item{seed}{Default seed is the R default. Enter a positive integer value
       to obtain a reproducible result, the same result for the same seed.}\cr

  \item{show_data}{Plot the data for each sample over the confidence interval.}
  \item{show_title}{Place a title on the graph that contains the parameter values_}
  \item{miss_only}{For the text output, only display information for samples
        that missed the mean.}
  \item{color_hit}{Color of the confidence intervals that contains the mean.}
  \item{color_miss}{Color of the confidence intervals that miss the mean.}
  \item{grid}{Color of the grid lines.}
  \item{ylim_bound}{Specify the maximum deviation of the mean in either direction
        for the extent of the vertical axis_}
  \item{pause}{Build the graph and the text output, pausing after each
        confidence interval.}
  \item{main}{Title of graph.}\cr

  \item{pdf_file}{Name of optional pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}\cr

  \item{\dots}{Other parameter values.} 
}


\details{
Simulate random normal data and display the resulting confidence intervals, with or without the data overlaid on each confidence interval. Highlight confidence intervals that miss the underlying population mean.}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 25 confidence intervals with a sample size each of 100
# mu=0, sigma=1, that is, sample from the standard normal
simCImean(25, 100)

# set the seed for a reproducible result with the same seed
simCImean(25, 100, seed=43)

# 25 confidence intervals with a sample size each of 100
# mu=100, sigma=15
# overlay the data over each confidence interval
simCImean(25, 100, mu=100, sigma=15, show_data=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ confidence interval }

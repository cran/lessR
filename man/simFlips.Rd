\name{simFlips}
\alias{simFlips}

\title{Pedagogical Binomial Simulation, Coin flips}

\description{
Simulate a sequence of coin flips.
}

\usage{
simFlips(n, prob=.5, seed=NULL,
         show_title=TRUE, show_flips=TRUE,
         grid="grey90", pause=FALSE,
         main=NULL, pdf_file=NULL, width=5, height=5, \dots)
}


\arguments{
  \item{n}{Size of each sample, that is, the number of trials or flips.}
  \item{prob}{Probability of a success on any one trial.}
  \item{seed}{Default seed is the R default. Enter a positive integer value
       to obtain a reproducible result, the same result for the same seed.}\cr

  \item{show_title}{Place a title on the graph that contains the parameter values_}
  \item{show_flips}{Plot the outcome of each flip.}
  \item{grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output, pausing after each
        confidence interval.}
  \item{main}{Title of graph.}\cr

  \item{pdf_file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}\cr

  \item{\dots}{Other parameter values.} 
}


\details{
Generate and plot successive values of a Head or a Tail using standard R \code{\link{rbinom}} function.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 10 flips of a fair coin
simFlips(10, .5)

# set the seed for a reproducible result with the same seed
simFlips(10, .5, seed=43)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binomial process }



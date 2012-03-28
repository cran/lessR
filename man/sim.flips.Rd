\name{sim.flips}
\alias{sim.flips}

\title{Pedagogical Binomail Simulation, Coin flips}

\description{
Simulate a sequence of coin flips.
}

\usage{
sim.flips(n, prob=.5, show.title=TRUE,
         show.flips=TRUE, col.grid="grey90", pause=FALSE)
}


\arguments{
  \item{n}{Size of each sample, that is, the number of trials or flips.}
  \item{prob}{Probability of a success on any one trial.}
  \item{show.title}{Place a title on the graph that contains the parameter values.}
  \item{show.flips}{Plot the outcome of each flips.}
  \item{col.grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output confidence interval by confidence interval.}
}


\details{
Generate and plot successive values of a Head or a Tail using standard R \code{\link{rbinom}} function.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 10 flips of a fair coin
sim.flips(10, .5)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binomial process }

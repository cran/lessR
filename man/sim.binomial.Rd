\name{sim.binomial}
\alias{sim.binomial}

\title{Pedagogical Binomail Simulation, Coin Flips}

\description{
Simulate a sequence of coin flips.
}

\usage{
sim.binomial(n, prob=.5, show.title=TRUE, pdf.out=FALSE, 
             show.flips=TRUE, col.grid="grey90", pause=FALSE)
}


\arguments{
  \item{n}{Size of each sample.}
  \item{prob}{Probability of a success on any one trial.}
  \item{show.title}{Place a title on the graph that contains the parameter values.}
  \item{pdf.out}{Generate a pdf file with an appropriate title.}
  \item{show.flips}{Plot the outcome of each flip.}
  \item{col.grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output confidence interval by confidence interval.}
}


\details{
Generate and plot successive values of a Head or a Tail using standard R \code{\link{rbinom}} function.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 10 flips of a fair coin
sim.binomial(10, .5)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binomial process }

\name{simFlips}
\alias{simFlips}

\title{Pedagogical Binomial Simulation, Coin flips}

\description{
Simulate a sequence of coin flips.
}

\usage{
simFlips(n, prob=.5, show.title=TRUE,
         show.flips=TRUE, color.grid="grey90", pause=FALSE,
         main=NULL, pdf.file=NULL, pdf.width=5, pdf.height=5, \ldots)
}


\arguments{
  \item{n}{Size of each sample, that is, the number of trials or flips.}
  \item{prob}{Probability of a success on any one trial.}
  \item{show.title}{Place a title on the graph that contains the parameter values.}
  \item{show.flips}{Plot the outcome of each flips.}
  \item{color.grid}{Color of the grid lines.}
  \item{pause}{Build the graph and the text output confidence interval by confidence interval.}
  \item{main}{Title of graph.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values, possibly invalid.} 
}


\details{
Generate and plot successive values of a Head or a Tail using standard R \code{\link{rbinom}} function.
}


\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\examples{
# 10 flips of a fair coin
simFlips(10, .5)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binomial process }



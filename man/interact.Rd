\name{interact}
\alias{interact}

\title{Run Shiny Apps}

\description{Interactive histogram.
}

\usage{
interact(app)
}


\arguments{
  \item{app}{Name of the shiny app to run, enclosed in quotes.}
}


\details{
Valid names are BarChart, Histogram, ScatterPlot, Trellis. If missing, then the valid names are listed.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
# Commented out as the analyses are interactive
#interact()
#interact("BarChart")
}


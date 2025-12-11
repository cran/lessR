\name{interact}
\alias{interact}

\title{Run Interactive Shiny Data Visualizations}

\description{Interactive data visualizations. Choose your data, choose your variables, and set the parameters as desired.
}

\usage{
interact(app)
}


\arguments{
  \item{app}{Name of the shiny app to run, enclosed in quotes.}
}


\details{
Valid names are \code{"BarChart"}, \code{"Histogram"}, \code{"ScatterPlot"}. If missing, then the valid names are listed.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
# Commented out as the analyses are interactive
#interact()
#interact("BarChart")
}


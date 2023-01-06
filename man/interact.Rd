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
Valid names are \code{"BarChart"}, \code{"PieChart"}, \code{"Histogram"}, \code{"ScatterPlot"}, \code{"Trellis"}. If missing, then the valid names are listed. Valid abbreviations, respectively, are \code{"bc"}, \code{"pc"}, \code{"hs"}, and \code{"Plot"}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
# Commented out as the analyses are interactive
#interact()
#interact("BarChart")
}


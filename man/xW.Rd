\name{xW}
\alias{xW}

\title{Text Processing: Wrap Words to Create New Lines From a Specified Line}

\description{Split a larger line into multiple lines by wrapping words with inserted line feeds. Primarily for internal use in text processing of \code{knitr} output. Not usually referenced by the user.
}

\usage{
xW(x, w=90, indent=5) 
}


\arguments{
  \item{x}{The character string to split into separate lines.}
  \item{w}{Maximum width of each line.}
  \item{indent}{Amount of spaces to indent lines after the first line.}
}

\details{
Input is a sentence. Output is the sentence word wrapped into multiple lines, each line up to the maximum width.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
xW("The quick brown fox jumped over the lazy dog's back.", w=30)
}

\name{corScree}
\alias{corScree}
\alias{scree}

\title{Eigenvalue Plot of a Correlation Matrix}

\description{
Abbreviation: \code{scree}

Plots the successive eigenvalues of an input correlation matrix. Also plots the successive differences of the eigenvalues. The purpose is usually to help determine the number of factors that explain the correlations in a correlation matrix. So usually a prelude to an exploratory factor analysis, such as provided by the \code{lessR} function \code{\link{corEFA}}. This program relies upon the standard R exploratory factor analysis \code{\link{factanal}}, which requires the specified number of factors as an input to the analysis.
}

\usage{
corScree(x=mycor, 
          main=NULL, pdf=FALSE, width=5, height=5, \ldots)

scree(\ldots)
}

\arguments{
  \item{x}{Correlation matrix.}
  \item{main}{Graph title, which is blank by default.}
  \item{pdf}{Indicator as to if the graphic files should be saved as pdf files
       instead of directed to the standard graphics windows.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\dots}{Parameter values_}
}


\details{
Interpretation of the scree plot to assist in the assessment of the number of factors that account for the structure of a correlation matrix depends primarily on the analysis of the differences between the successive eigenvalues_ The differences begin to diminish where the "scree" begins, analogous to the debris that falls off of a hill top.  Accordingly both the scree plot itself, the plot of the successive eigenvalues, and the plot of the differences of the successive eigenvalues are presented.

PDF OUTPUT\cr
Because of the customized graphic windowing system that maintains a unique graphic window for the Help function, the standard graphic output functions such as \code{\link{pdf}} do not work with the \code{lessR} graphics functions.  Instead, to obtain pdf output, use the \code{pdf_file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

If the two plots, of the population and sample distributions respectively, are written to pdf files, according to \code{pdf=TRUE}, they are named Scree.pdf and ScreeDiff.pdf. Their names and the directory to which they are written are provided as part the console output.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapters 9 and 10, NY: Routledge.
}

\seealso{
\code{\link{Correlation}}.
}

\examples{
# input correlation matrix of perfect two-factor model
# Factor Pattern for each Factor: 0.8, 0.6, 0.4
# Factor-Factor correlation: 0.3
mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("V1", "V2", "V3", "V4", "V5", "V6")
rownames(mycor) <- colnames(mycor) 

# obtain the scree plots
corScree()

# abbreviated form
scree()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ scree }

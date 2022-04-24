\name{dataAnova_2way}
\docType{data}
\alias{dataAnova_2way}
\title{Data for a Two-Way Balanced Factorial Design}

\description{Laboratory rats were randomly and equally divided into groups, and then given one of three dosages of an arousal inducing drug: 0, 5, and 10 milligrams. Following the dosage, each rat completed either an easy or a hard maze to obtain a food reward. The response (dependent) variable is the Time in seconds to complete the maze.}

\format{A data table with 48 rows of data and 3 columns: Difficulty, Dosage, and Time.}

\references{
Gerbing, D. W. (later in 2022). R Data Analysis without Programming, 2nd Edition, Chapter 7, NY: Routledge.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871. 
}

\author{
David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})
}

\examples{
d <- Read("Anova_2way")
ANOVA(Time ~ Dosage * Difficulty)
}

\source{author}
\keyword{datasets}

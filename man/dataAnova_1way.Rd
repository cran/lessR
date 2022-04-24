\name{dataAnova_1way}
\docType{data}
\alias{dataAnova_1way}
\title{Data for a One-Way ANOVA}

\description{To study the impact of arousal on the ability to complete a task, 24 laboratory rats were randomly and equally divided into three groups of eight. Each rat was administered one of three dosages of an arousal inducing drug: 0, 5, and 10 milligrams. Following the dosage, each rat completed a maze to obtain a food reward. The response (dependent) variable is the Time in seconds to complete the maze.}

\format{A data table with 24 rows of data and 2 columns, with variables Dosage and Time.}

\references{
Gerbing, D. W. (later in 2022). R Data Analysis without Programming, 2nd Edition, Chapter 7, NY: Routledge.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871. 
}

\author{
David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})
}

\examples{
d <- Read("Anova_1way")
ANOVA(Time ~ Dosage)
}

\source{author}
\keyword{datasets}

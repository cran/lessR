\name{dataAnova_rb}
\docType{data}
\alias{dataAnova_rb}
\title{Data for a Randomized Block ANOVA}

\description{Seven people, with differing amounts of muscle strength, took one of four different pre-workout supplements and then did a bench press of 125 lbs as many times as possible. Each person did four workouts at four different times, with a different supplement before each workout. The experimenter randomized the presentation order of the supplements to each person to avoid any artifacts from presentation order.}


\format{A data table with 7 rows of data and 5 columns: Person, and sup1 through sup4 for the four supplements.}

\references{
Gerbing, D. W. (later in 2022). R Data Analysis without Programming, 2nd Edition, Chapter 7, NY: Routledge.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871. 
}

\author{
David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})
}

\examples{
d <- Read("Anova_rb")
d <- reshape_long(d, sup1:sup4, group="Supplement", response="Reps")
ANOVA(Reps ~ Supplement + Person)
}

\source{author}
\keyword{datasets}

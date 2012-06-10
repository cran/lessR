\name{ANOVA}
\alias{ANOVA}
\alias{av}
\alias{av.brief}

\title{Analysis of Variance}

\description{
Abbreviation: \code{av}, \code{av.brief}

Analysis of variance with graphics.

Can also be called from the more general \code{\link{model}} function.
}

\usage{
ANOVA(my.formula, dframe=mydata, 
         brief=FALSE, digits.d=4, \ldots)

av(my.formula, \ldots)

av.brief(my.formula, brief=TRUE, \ldots) 
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example, for a response variable named Y and two predictor variables, X1 and 
       X2, specify the corresponding linear model as Y ~ X1 + X2.}
  \item{dframe}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{brief}{Extent of displayed results.}
  \item{digits.d}{For the Basic Analysis, it provides the number of decimal digits.
        For the rest of the output, it is a suggestion only.}
  \item{\dots}{Other parameter values for R function \code{\link{lm}} which provides the core computations.} 
}


\details{
One-way ANOVA with Tukey HSD and plot based on the R functions \code{\link{aov}}, \code{\link{TukeyHSD}}, and provides summary statistics for each level. Two-factor ANOVA also provides an interaction plot of the means with \code{\link{interaction.plot}} as well as a table of means and other summary statistics.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{aov}}, \code{\link{TukeyHSD}}, \code{\link{interaction.plot}}
}

\examples{
# create simulated data, no population mean difference
# X has two values only, Y is numeric
# put into a data frame
n <- 12
X <- sample(c("Group1","Group2"), size=n, replace=TRUE)
Y <- rnorm(n=n, mean=50, sd=10)
mydata <- data.frame(X,Y)

# analyze data with formula version
# variable names and levels of X are automatically obtained from data
# although data frame not attached, reference variable names directly
ANOVA(Y ~ X)
# short name
av(Y ~ X)

# variable of interest is in a data frame which is not the default mydata
# access the data frame in the lessR dat.twogroup data set
# although data not attached, access the variables directly by their name
data(dat.twogroup)
ANOVA(ShipTime ~ Supplier, dframe=dat.twogroup)

# two-factor ANOVA with replications and interaction
ANOVA(breaks ~ wool * tension, dframe=warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }



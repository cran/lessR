\name{Model}
\alias{Model}
\alias{model}
\alias{model.brief}


\title{Regression Analysis, ANOVA or t-test}

\description{
Abbreviation: \code{model}, \code{model.brief}

Automatically selects and then provides an analysis of a linear model: OLS regression, ANOVA, or a t-test depending on the proprieties of the data. Comprehensive regression analysis with graphics from a single, simple function call with many default settings, each of which can be re-specified.  By default the data exists as a data frame with the default name of \code{mydata}, such as data read by the \code{lessR} \code{rad} function.  Specify the model in the function call according to an R \code{\link{formula}}, that is, the response variable followed by a tilde, followed by the list of predictor variables, each pair separated by a plus sign.  
}

\usage{
Model(my.formula, dframe=mydata, brief=FALSE, \ldots)

model.brief(my.formula, brief=TRUE, \ldots) 

model(my.formula, \ldots) 
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example, for a response variable named Y and two predictor variables, X1 and 
       X2, specify the corresponding linear model as Y ~ X1 + X2.}
  \item{dframe}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{brief}{Reduced output.}
  \item{\dots}{Other parameter values for R functions such as \code{\link{lm}} which provide the core computations.} 
}



\details{
OVERVIEW\cr
The purpose of \code{Model} is to combine many standard R function calls into one, as well as provide ancillary analyses such as as graphics, organizing output into tables and sorting to assist interpretation of the output, all from a single function. Currently the supported models are OLS regression, ANOVA and the t-test.  For more details of each of these methods, see the \code{lessR} functions \code{\link{Regression}},  \code{\link{ANOVA}} and  \code{\link{ttest}}, respectively, which, in turn are based on many standard R functions.

All invocations of the \code{model} function are based on the standard R \code{\link{formula}}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{formula}}, \code{\link{lm}}, \code{\link{summary.lm}}, \code{\link{anova}}, \code{\link{confint}}, \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, \code{\link{cooks.distance}}
}

\examples{
# Generate random data, place in data frame mydata
X1 <- rnorm(20)
X2 <- rnorm(20)
Y <- .7*X1 + .2*X2 + .6*rnorm(20)
#  instead, if read data with the rad function
#   then the result is the data frame called mydata 
mydata <- data.frame(X1, X2, Y)
rm(Y); rm(X1); rm(X2)

# One-predictor regression
# Provide all default analyses including scatterplot etc.
Model(Y ~ X1)
# alternate form
model(Y ~ X1)

# Multiple regression model
# Provide all default analyses, including interactive 3d scatterplot
Model(Y ~ X1 + X2)

# t-test
Model(breaks ~ wool, dframe=warpbreaks)

# ANOVA analysis
# from another data frame other than the default \code{mydata}
# breaks is numerical, wool and tension are categorical
Model(breaks ~ wool + tension, dframe=warpbreaks)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }



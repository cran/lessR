\name{reg}
\alias{reg}

\title{Regression Analysis}

\description{
Automatically provides a comprehensive regression analysis from a single, simple function call with many default settings.  By default the data should exist as a dataframe called \code{mydata}, such as provided by the \code{\link{rad}} function included in this package for reading and processing data in preparation for analysis.  

The default analysis provides the model's parameter estimates and corresponding hypothesis tests and confidence intervals, goodness of fit indices, the ANOVA table, correlation matrix of the model's variables, collinearity analysis of the predictor variables, adjusted R-squared for the corresponding models defined by each possible subset of the predictor variables, and, for each observation in the model, analysis of residuals and influence as well as the confidence and prediction intervals.  By default the residual analysis lists the data and fitted value for each observation as well as the residual, Studentized residual and Cook's distance, with the first 25 observations listed and sorted by Cook's distance. The output for the confidence and prediction intervals also provides the data and fitted value for each observations, as well as the lower and upper bounds for each of the two intervals.  The observations are sorted by the lower bound of each prediction interval.  Also, for models with a single predictor variable, a scatterplot of the data is produced, along with the regression line and corresponding confidence and prediction intervals. For multiple regression models, a scatterplot matrix of the variables in the model is produced.    

Overriding the default settings can turn off features and reduce the number of provided analyses.
}

\usage{
reg(my.formula, dframe=mydata, graph=TRUE, cor=TRUE,
         res.rows=NULL, res.sort=c("cooks","rstudent","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         subsets=TRUE, collinear=TRUE, sig.digits=4, show.R=FALSE)
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example, for a response variable named Y and two predictor variables, X1 and X2, specify the corresponding linear model as Y ~ X1 + X2.}
  \item{dframe}{Default is \code{mydata}, the name of the data frame that contains the data.
       The default name is consistent with the name given by the \code{\link{rad}} function for reading the data, also available in this package.}
  \item{graph}{Default is \code{TRUE}. If there is one predictor variable in the model, a 
       scatterplot with regression line is produced.  If prediction intervals are requested, both the confidence and prediction intervals are added to the graph.}
  \item{cor}{Default is \code{TRUE}, which prints a correlation matrix of the model variables.}
  \item{res.rows}{Default is 25, which lists the first 25 rows of data sorted by the
       specified sort criterion.  To turn this option off, specify a value of 0.  To see the output for all observations, specify a value of \code{"all"}.}
  \item{res.sort}{Default is \code{"cooks"}, for specifying Cook's distance as the sort
       criterion for the display of the rows of data and associated residuals. Other values are \code{"rstudent"} for Studentized residuals, and \code{"off"} to not provide the analysis.}
  \item{pred}{Default is \code{TRUE}, which, produces confidence and prediction intervals 
       for each row of data.}
  \item{pred.all}{Default is \code{FALSE}, which, produces prediction intervals only for the first,
       middle and last five rows of data.}
  \item{pred.sort}{Default is \code{"predint"}, which sorts the rows of data and associated
       intervals by the lower bound of each prediction interval.  Turn off this sort by specifying a value of \code{"off"}.}
  \item{subsets}{Default is \code{TRUE}, for producing an analysis from the \code{leaps} package
       for the adjusted R-squared of all possible models from the set of predictor variables.}
  \item{collinear}{Default is \code{TRUE}, for producing a collinearity analysis from the \code{car}
       package.}
  \item{sig.digits}{Provides the same functionality as the standard \code{\link{options}}
       function regarding the digits option. The distinction is that this value applies selectively to portions of the output, 
       the different type of residuals and the confidence and prediction intervals.}
  \item{show.R}{Display the R instructions that yielded the lessR output, albeit without
        the additional lessR formatting.}
}


\details{
Regression analysis automatically provides a variety of regression analyses. The basic analysis successively invokes three standard R functions \code{\link{lm}}, \code{\link{summary}} and \code{\link{confint}}. The residual analysis invokes \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, and \code{\link{cooks.distance}}.  The option for prediction intervals calls the standard R function \code{\link{predict}}, once with the argument interval="confidence" and once with interval="prediction". If there is only one predictor variable in the model, a scatterplot of the data with regression line is produced, along with the plotted confidence and prediction intervals. 

The output for the residual analysis displays by default just the first 25 observations with the largest values of Cook's distance, sorted by this criterion.  The output of the prediction intervals is re-organized so that each row's computed fitted value and prediction interval are listed adjacent to the corresponding values of the predictor variables and response variable. Each row of information, the data and corresponding intervals, is by default sorted by the lower bound of the prediction interval.

The \code{\link{options}} function is called to turn off the stars for different significance levels (show.signif.stars=FALSE) and to turn off scientific notation for the output (scipen=30).

The purpose of \code{reg} is to combine these function calls into one, and provide ancillary analyses such as sorting where appropriate to assist in interpretation, and the analysis of the adjusted R-squared for the models defined by all possible subsets of the predictor variables.
}

\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{formula}}, \code{\link{lm}}, \code{\link{summary.lm}}, \code{\link{anova}}, \code{\link{confint}}, \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, \code{\link{cooks.distance}}
}

\examples{
# Generate random data, place in dataframe mydata
X1 <- rnorm(20)
X2 <- rnorm(20)
Y <- .7*X1 + .2*X2 + .6*rnorm(20)
mydata <- data.frame(X1, X2, Y)

# Call reg for a one-predictor regression
# Provide all default analyses including scatterplot etc.
reg(Y ~ X1)

# Call reg according to a multiple regression model
# Provide the full range of default analyses
reg(Y ~ X1 + X2)

# Call reg and modify the default settings as specified
reg(Y ~ X1 + X2, res.row=8, res.sort="rstudent", sig.digits=8, pred=FALSE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }

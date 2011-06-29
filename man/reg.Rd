\name{reg}
\alias{reg}

\title{Regression Analysis}

\description{
Automatically provides a comprehensive regression analysis from a single, simple function call with many default settings.  By default the data should exist as a dataframe called \code{mydata}, or explicitly specified with the \code{dframe} option.  The name \code{mydata} is by default provided by the \code{\link{rad}} function included in this package for reading and displaying information about the data in preparation for analysis. If all the variables in the model are not in the same dataframe, the analysis will not be complete.

The default analysis provides the model's parameter estimates and corresponding hypothesis tests and confidence intervals, goodness of fit indices, the ANOVA table, correlation matrix of the model's variables, collinearity analysis of the predictor variables, adjusted R-squared for the corresponding models defined by each possible subset of the predictor variables, and, for each observation in the model, analysis of residuals and influence as well as the confidence and prediction intervals.  By default the residual analysis lists the data and fitted value for each observation as well as the residual, Studentized residual and Cook's distance, with the first 25 observations listed and sorted by Cook's distance. The output for the confidence and prediction intervals also provides the data and fitted value for each observations, as well as the lower and upper bounds for each of the two intervals.  The observations are sorted by the lower bound of each prediction interval.  The default analysis for the prediction intervals is for the values of the predictor variables in the data, but additional values can also be specified for the calculation of the prediction intervals.

Three default graphs are also provided. A histogram is provided with superimposed normal and general density plots from the \code{\link{color.density}} function included in this package. A scatterplot of the residuals with the fitted values is also provided from the \code{\link{color.plot}} function included in this package.  For models with a single predictor variable, a scatterplot of the data is produced, along with the regression line and corresponding confidence and prediction intervals. For multiple regression models, a scatterplot matrix of the variables with the lowess best-fit lines in the model is produced.    

Overriding the default settings can turn off features and reduce the number of provided analyses.
}

\usage{
reg(my.formula, dframe=mydata, sig.digits=4,
         res.rows=NULL, res.sort=c("cooks","rstudent","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         cor=TRUE, subsets=TRUE, collinear=TRUE, relations=TRUE,
         cook.cut=1, results=c("full", "brief"), scatter.cor=FALSE,
         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL, show.R=FALSE)
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example, for a response variable named Y and two predictor variables, X1 and X2, specify the corresponding linear model as Y ~ X1 + X2.}
  \item{dframe}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{sig.digits}{Provides the same functionality as the standard \code{\link{options}}
       function regarding the digits option. The distinction is that this value applies selectively to portions of the output, the different type of residuals and the confidence and prediction intervals.}
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
  \item{cor}{Default is \code{TRUE}, which prints a correlation matrix of the model variables.}
  \item{subsets}{Default is \code{TRUE}, for producing an analysis from the \code{leaps} package
       for the adjusted R-squared of all possible models from the set of predictor variables.}
  \item{collinear}{Default is \code{TRUE}, for producing a collinearity analysis from the \code{car}
       package.}
  \item{relations}{Default is \code{TRUE}, which indicates to perform all three analyses of relations among
        the variables: correlations, collinearity and predictor variable subsets.}
  \item{cook.cut}{Cutoff value of Cook's Distance at which observations with a larger value are flagged in red and labeled in the resulting scatterplot of Residuals and Fitted Values.}
  \item{results}{Verbosity of displayed results.  Default is \code{"full"}.}
  \item{scatter.cor}{Display the correlation coefficients in the upper triangle of the
        scatterplot matrix. By default is \code{FALSE}.}
  \item{X1.new}{Values of the first listed predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X2.new}{Values of the second listed predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X3.new}{Values of the third listed predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X4.new}{Values of the fourth listed predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X5.new}{Values of the fifth listed predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{show.R}{Display the R instructions that yielded the lessR output, albeit without
        the additional lessR formatting.}
}


\details{
The basic analysis successively invokes several standard R linear model functions beginning with \code{\link{lm}}. The output of the analysis of \code{\link{lm}} is stored in the object \code{lm.out}, which is available for further analysis in the R environment when the \code{reg} function has completed. Usually, however, \code{reg} automatically provides the analyses from the standard R functions, \code{\link{summary}}, \code{\link{confint}} and  \code{\link{anova}}. The residual analysis invokes \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, and \code{\link{cooks.distance}}.  The option for prediction intervals calls the standard R function \code{\link{predict}}, once with the argument interval="confidence" and once with interval="prediction". Thomas Lumley's \code{leaps} package contains the \code{leaps} function that provides the analysis of the fit of all possible model subsets. 
The purpose of \code{reg} is to combine these function calls into one, as well as provide ancillary analyses such as as graphics and of sorting to assist in interpretation, and the analysis of the adjusted R-squared for the models defined by all possible subsets of the predictor variables.

For graphics, if there is only one predictor variable in the model, a scatterplot of the data with regression line is produced, along with the plotted confidence and prediction intervals, otherwise the scatterplot matrix of all the variables in the model is generated.  Also generated are the histogram of the residuals with superimposed general density curve and the plot of the residuals against the fitted values of the model.  For the fitted values plot, the point corresponding to the largest value of Cook's distance is plotted in red with the corresponding value of Cook's distance specified in the subtitle of the plot.

The output for the residual analysis displays by default just the first 25 observations with the largest values of Cook's distance, sorted by this criterion.  The output of the prediction intervals is re-organized so that each row's computed fitted value and prediction interval are listed adjacent to the corresponding values of the predictor variables and response variable. Each row of information, the data and corresponding intervals, is by default sorted by the lower bound of the prediction interval.  If, by providing values for the options \code{X1.new}, \code{X2.new} and so forth, to provide new data values for the computation of the corresponding fitted values and prediction intervals, then all combinations of the values for each predictor variable are analyzed.

The analysis of the models defined by each subset of the predictor variables is computed by the \code{leaps} function, written by Thomas Lumley, from the \code{leaps} package.

The \code{\link{options}} function is called to turn off the stars for different significance levels (show.signif.stars=FALSE) and to turn off scientific notation for the output (scipen=30).
}

\references{
Lumley, T., \code{leaps} function from the \code{leaps} package.
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

# One-predictor regression
# Provide all default analyses including scatterplot etc.
reg(Y ~ X1)

# Multiple regression model
# Provide the full range of default analyses
reg(Y ~ X1 + X2)
# Provide only the brief analysis
reg(Y ~ X1 + X2, results="brief")

# Modify the default settings as specified
reg(Y ~ X1 + X2, res.row=8, res.sort="rstudent", sig.digits=8, pred=FALSE)

# Specify values of the predictor variables for calculating forecasted
#  values and the corresponding prediction interval
# Note in this analysis it is just a coincidence that the variables are
#  named X1 and X2, which always begin the names X1.new and X2.new
reg(Y ~ X1 + X2, X1.new=seq(.4,.8,.2), X2.new=c(.5,.7))

# Scatterplot matrix with correlation coefficients in upper triangle
# Specify an input dataframe other than mydata
# help(mtcars) to see description of the data, included with R
reg(mpg ~ cyl + disp + hp + drat + wt + gear, scatter.cor=TRUE, dframe=mtcars)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }

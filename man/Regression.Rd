\name{Regression}
\alias{Regression}
\alias{reg}
\alias{reg.brief}
\alias{reg.explain}

\title{Regression Analysis}

\description{
Abbreviation: \code{reg}, \code{reg.brief}, \code{reg.explain}

Automatically provides a comprehensive regression analysis with graphics from a single, simple function call with many default settings, each of which can be re-specified.  By default the data exists as a data frame with the default name of \code{mydata}, such as data read by the \code{lessR} \code{rad} function.  Specify the model in the function call according to an R \code{\link{formula}}, that is, the response variable followed by a tilde, followed by the list of predictor variables, each pair separated by a plus sign.  

Default output includes the inferential analysis of the estimated coefficients and model, correlation matrix, sorted residuals and Cook's Distance, and sorted prediction intervals for existing data or new data.  For multiple regression models, also included is an analysis of the fit of all possible model subsets and an analysis of collinearity. The default output also includes three graphs beginning with a histogram of the residuals with superimposed normal and general density curves.  The second graph is a scatterplot of the fitted values with the residuals and the corresponding \code{\link{lowess}} curve. The point corresponding to the largest value of Cook's Distance is labeled accordingly. Also provided, for a model with one predictor variable, is a scatterplot of the data with regression line and confidence and prediction intervals.  For multiple regression models the graph is the scatterplot matrix of the model variables with the \code{\link{lowess}} curve displayed for each constituent scatterplot.  If the model has exactly two predictor variables, a 3D scatterplot about the regression plane is optionally produced.

Can also be called from the more general \code{\link{model}} function.

The resulting scatterplot or scatterplot matrix, when written to a pdf file according to \code{pdf=TRUE}, is named RegScatterplot.pdf or RegScatterMatrix.pdf. If residuals are reported, then the two additional pdf files are named RegResiduals.pdf and RegResidFitted.pdf. Their names and the directory to which they are written are povided as part the console output.
}

\usage{
Regression(my.formula, dframe=mydata, digits.d=4, text.width=120, 
         brief=FALSE, explain=FALSE, show.R=FALSE,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=c("predint", "off"),
         subsets=TRUE, cooks.cut=1, 

         scatter.coef=FALSE, scatter.3d=FALSE,

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL,

         pdf=FALSE, pdf.width=5, pdf.height=5, \ldots)

reg(my.formula, \ldots) 

reg.brief(my.formula, brief=TRUE, \ldots) 

reg.explain(my.formula, explain=TRUE, \ldots) 
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example, for a response variable named Y and two predictor variables, X1 and 
       X2, specify the corresponding linear model as Y ~ X1 + X2.}
  \item{dframe}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{digits.d}{For the Basic Analysis, it provides the number of decimal digits.
        For the rest of the output, it is a suggestion only.}
  \item{text.width}{Width of the text output at the console.}
  \item{brief}{Extent of displayed results.}
  \item{explain}{Off by default, but if set to \code{TRUE} then brief labels of different
        sections of output are replaced by more complete explanations of the output.}
  \item{show.R}{Display the R instructions that yielded the \code{lessR} output, albeit without
        the additional formatting of the results such as combining output of different 
        functions into a table.}
  \item{colors}{Sets the color palette for the resulting graphics.}
  \item{res.rows}{Default is 25, which lists the first 25 rows of data sorted by the
       specified sort criterion.  To turn this option off, specify a value of 0.  To see 
       the output for all observations, specify a value of \code{"all"}.}
  \item{res.sort}{Default is \code{"cooks"}, for specifying Cook's distance as the sort
       criterion for the display of the rows of data and associated residuals. Other values 
       are \code{"rstudent"} for Studentized residuals, and \code{"off"} to not provide the
       analysis.}
  \item{pred}{Default is \code{TRUE}, which, produces confidence and prediction intervals 
       for each row of data.}
  \item{pred.all}{Default is \code{FALSE}, which produces prediction intervals only for the
       first, middle and last five rows of data.}
  \item{pred.sort}{Default is \code{"predint"}, which sorts the rows of data and associated
       intervals by the lower bound of each prediction interval.  Turn off this sort by
       specifying a value of \code{"off"}.}
  \item{subsets}{Default is to produce the analysis of the fit based on adjusted R-squared 
       for all possible model subsets from the \code{leaps} package. Set to \code{FALSE} to
       turn off.}
  \item{cooks.cut}{Cutoff value of Cook's Distance at which observations with a larger value 
       are flagged in red and labeled in the resulting scatterplot of Residuals and Fitted 
       Values.  Default value is 1.0.}
  \item{scatter.coef}{Display the correlation coefficients in the upper triangle of the
        scatterplot matrix.}
  \item{scatter.3d}{A 3D scatterplot with best fitting regression plane, which applies only 
       to models with exactly two predictor variables. For two-predictor variable models, turn 
       off by setting to \code{FALSE}.}
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
  \item{pdf}{If \code{TRUE}, then graphics are written to pdf files.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for R function \code{\link{lm}} which provides the core computations.} 
}



\details{
OVERVIEW\cr
The purpose of \code{Regression} is to combine the following function calls into one, as well as provide ancillary analyses such as as graphics, organizing output into tables and sorting to assist interpretation of the output.  

The basic analysis successively invokes several standard R functions beginning with the standard R function for estimation of a linear model, \code{\link{lm}}. The output of the analysis of \code{\link{lm}} is stored in the object \code{lm.out}, available for further analysis in the R environment upon completion of the \code{reg} function by any R function that uses the output of \code{\link{lm}} as input. By default \code{reg} automatically provides the analyses from the standard R functions, \code{\link{summary}}, \code{\link{confint}} and \code{\link{anova}}, with some of the standard output modified and enhanced. The correlation matrix of the model variables is obtained with \code{\link{cor}} function. The residual analysis invokes \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, and \code{\link{cooks.distance}} functions.  The option for prediction intervals calls the standard R function \code{\link{predict}}, once with the argument \code{interval="confidence"} and once with \code{interval="prediction"}.  The \code{lessR} \code{\link{den}} function provides the histogram and density plots for the residuals and the \code{\link{plt}} function provides the scatter plots of the residuals with the fitted values and of the data for the one-predictor model. The \code{pairs} function provides the scatterplot matrix of all the variables in the model. Thomas Lumley's \code{leaps} package contains the \code{leaps} function that provides the analysis of the fit of all possible model subsets. The \code{car} package provides Henric Nilsson and John Fox's \code{vif} function for the computation of the variance inflation factors for the collinearity analysis. The \code{scatter3d} function from Fox and Weisberg's \code{car} package provides the interactive 3d scatterplot for models with exactly two predictor variables.`

The default analysis provides the model's parameter estimates and corresponding hypothesis tests and confidence intervals, goodness of fit indices, the ANOVA table, correlation matrix of the model's variables, analysis of residuals and influence as well as the confidence and prediction intervals for each observation in the model. Also provided, for multiple regression models, collinearity analysis of the predictor variables and adjusted R-squared for the corresponding models defined by each possible subset of the predictor variables. Because the results of the initial call to the linear model function, \code{\link{lm}}, are available after \code{reg} has completed in the R object \code{lm.out}.

DATA FRAME\cr
The name \code{mydata} is by default provided by the \code{\link{Read}} function included in this package for reading and displaying information about the data in preparation for analysis. If all the variables in the model are not in the same data frame, the analysis will not be complete.  The data frame does not need to be attached, just specified by name with the \code{dframe} option if the name is not the default \code{mydata}.

GRAPHICS\cr
At least three default graphs are provided, and a fourth graph is provided for models with two predictor variables. By default the graphs are written to separate graphics windows (which may overlap each other completely, in which case move the top graphics windows). Or, the \code{graphics.save} option may be invoked to save the graphs to a single pdf file called \code{regOut.pdf}. The directory to which the file is written is displayed on the console text output.

1. A histogram of the residuals includes the superimposed normal and general density plots from the \code{\link{den}} function included in this \code{lessR} package.  The overlapping density plots, which both overlap the histogram, are filled with semi-transparent colors to enhance readability.  

2. A scatterplot of the residuals with the fitted values is also provided from the \code{\link{plt}} function included in this package. The point corresponding to the largest value of Cook's distance, regardless of its size, is plotted in red and labeled and the corresponding value of Cook's distance specified in the subtitle of the plot. Also by default all points with a Cook's distance value larger than 1.0 are plotted in red, a value that can be specified to any arbitrary value with the \code{cooks.cut} option.  This scatterplot also includes the \code{\link{lowess}} curve.

3. For models with a single predictor variable, a scatterplot of the data is produced, which also includes the regression line and corresponding confidence and prediction intervals. As with the density histogram plot of the residuals and the scatterplot of the fitted values and residuals, the scatterplot includes a colored background with grid lines. For multiple regression models, a scatterplot matrix of the variables in the model with the \code{\link{lowess}} best-fit line of each constituent scatterplot is produced. If the \code{scatter.coef} option is invoked, each scatterplot in the upper-diagonal of the correlation matrix is replaced with its correlation coefficient.

4. A fourth graph is provided for a model with exactly two predictor variables, which is an interactive three dimensional scatterplot projected into two dimensions about the regression plane. This graph is generated by the \code{scatter3d} function in the \code{car} package. In turn, the code in the \code{car} package depends on code in the \code{rgl} package. To turn off this option for two predictor variable models, set the \code{scatter.3d} option to \code{FALSE}.

RESIDUAL ANALYSIS\cr
By default the residual analysis lists the data and fitted value for each observation as well as the residual, Studentized residual, Cook's distance and dffits, with the first 20 observations listed and sorted by Cook's distance.  The \code{res.sort} option provides for sorting by the Studentized residuals or not sorting at all.  The \code{res.rows} option provides for listing these rows of data and computed statistics statistics for any specified number of observations (rows).  To turn off the analysis of residuals, specify \code{res.rows=0}.

PREDICTION INTERVALS\cr
The output for the confidence and prediction intervals includes a table with the data and fitted value for each observation, as well as the lower and upper bounds for the confidence interval and the prediction interval.  The observations are sorted by the lower bound of each prediction interval.  If there are more than 50 observations then the information for only the first five, the middle five and the last five observations is displayed. To turn off the analysis of prediction intervals, specify \code{pred=FALSE}, which also removes the corresponding intervals from the scatterplot produced with a model with exactly one predictor variable, yielding just the scatterplot and the regression line.

The data for the default analysis of the prediction intervals is for the values of the predictor variables for each observation, that is, for each row of the data. New values of the predictor variables can be specified for the calculation of the prediction intervals by providing values for the options \code{X1.new} for the values of the first listed predictor variable in the model, \code{X2.new} for the second listed predictor variable, and so forth for up to five predictor variables. To provide these values, use functions such as \code{\link{seq}} for specifying a sequence of values and \code{\link{c}} for specifying a vector of values. For multiple regression models, all combinations of the specified new values for all of the predictor variables are analyzed.

RELATIONS AMONG THE VARIABLES\cr
By default the correlation matrix of all the variables in the model is displayed, and, for multiple regression models, collinearity analysis is provided with the \code{vif} function from the Fox and Weisberg (2011) \code{car} package as well as the adjusted R squared of each possible model from an analysis of all possible subsets of the predictor variables.  This all subsets analysis requires the \code{leaps} function from the \code{leaps} package. These contributed packages are automatically loaded if available.  If not available, an appropriate warning is provided to the user with instructions to install the corresponding package with the \code{\link{install.packages}} function, and the analysis continues without the output that would have been provided by invoking that package.  To turn off the all possible sets option, set \code{subsets=FALSE}.

OUTPUT\cr
The text output is organized to provide the most relevant information while at the same time minimizing the total amount of output, particularly for analyses with large numbers of observations (rows of data), the display of which is by default restricted to only the most interesting or representative observations in the analyses of the residuals and predicted values. Additional economy can be obtained by invoking the \code{brief=TRUE} option, or run \code{reg.brief}, which limits the analysis to just the basic analysis of the estimated coefficients and fit. An explanation of each section of the output can be obtained by setting the \code{explain} option to \code{TRUE}, or run \code{reg.explain}.  Much of the underlying relevant R code run by \code{reg} is obtained by setting the \code{show.R} option to \code{TRUE}.

INVOKED R OPTIONS\cr
The \code{\link{options}} function is called to turn off the stars for different significance levels (show.signif.stars=FALSE), to turn off scientific notation for the output (scipen=30), and to set the width of the text output at the console to 120 characters. The later option can be re-specified with the \code{text.width} option.  After \code{reg} is finished with a normal termination, the options are re-set to their values before the \code{reg} function began executing.

COLOR THEME\cr
A color theme for all the colors can be chosen for a specific plot with the \code{colors} option. Or, the color theme can be changed for all subsequent graphical analysis with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in a data frame called \code{mylabels}, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.}

\references{
Fox, J., & Weisberg, S. (2011). An R companion to applied regression (Second ed.). Thousand Oaks CA: Sage.
Lumley, T., \code{leaps} function from the \code{leaps} package.\cr
Nilsson, H. and Fox, J., \code{vif} function from the \code{car} package.\cr
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
Regression(Y ~ X1)
# short name
reg(Y ~ X1)
# Provide only the brief analysis
reg.brief(Y ~ X1)
# Provide an explanation for each section of output
reg.explain(Y ~ X1)
# Provide a brief analysis with explanation 
reg.brief(Y ~ X1, explain=TRUE)

# Modify the default settings as specified
Regression(Y ~ X1, res.row=8, res.sort="rstudent", digits.d=8, pred=FALSE)

# Multiple regression model
# Provide all default analyses
Regression(Y ~ X1 + X2)
# Save the three plots as pdf files 4 inches square
Regression(Y ~ X1, pdf=TRUE, pdf.width=4, pdf.height=4)

# Specify new values of the predictor variables to calculate
#  forecasted values and the corresponding prediction intervals
# Specify an input data frame other than mydata, see help(mtcars)
Regression(mpg ~ hp + wt + disp, dframe=mtcars,
  X1.new=seq(50,350,50), X2.new=c(2,3), X3.new=c(100,300))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }



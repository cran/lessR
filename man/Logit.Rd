\name{Logit}
\alias{Logit}
\alias{lgt}

\title{Logit Analysis}

\description{
Abbreviation: \code{lgt}

Based directly on the standard R \code{\link{glm}} function with \code{family="binomial"}, automatically provides a logit regression analysis with graphics from a single, simple function call with many default settings, each of which can be re-specified.  By default the data exists as a data frame with the default name of \code{mydata}, such as data read by the \code{lessR} \code{Read} function.  Specify the model in the function call according to an R \code{\link{formula}}, that is, the response variable followed by a tilde, followed by the list of predictor variables, each pair separated by a plus sign.  

Default output includes the inferential analysis of the estimated coefficients and model, sorted residuals and Cook's Distance, and sorted fitted values for existing data or new data.  The default output also includes two or three graphs beginning with a histogram of the residuals with superimposed normal and general density curves.  The second graph is a scatterplot of the fitted values with the residuals and the corresponding \code{\link{lowess}} curve. The point corresponding to the largest value of Cook's Distance is labeled accordingly. Also provided, for a model with one predictor variable, is a scatterplot of the data with regression line and confidence and prediction intervals.

Can also be called from the more general \code{\link{model}} function.
        
The resulting scatterplot, when written to a pdf file according to \code{pdf=TRUE}, is named RegScatterplot.pdf. If residuals are reported, then the two additional pdf files are named RegResiduals.pdf and RegResidFitted.pdf. Their names and the directory to which they are written are povided as part the console output.
}

\usage{
Logit(my.formula, dframe=mydata, digits.d=4, text.width=120, 
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred=TRUE, pred.all=FALSE, pred.sort=TRUE, cooks.cut=1, 

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL,

         pdf=FALSE, pdf.width=5, pdf.height=5, \ldots)

lgt(my.formula, \ldots) 
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
  \item{pred.sort}{Default is \code{TRUE}, which sorts the rows of data and associated
       intervals by the lower bound of each fitted value.}
  \item{cooks.cut}{Cutoff value of Cook's Distance at which observations with a larger value 
       are flagged in red and labeled in the resulting scatterplot of Residuals and Fitted 
       Values.  Default value is 1.0.}
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
  \item{\dots}{Other parameter values for R function \code{\link{glm}} which provides the core computations.} 
}



\details{
OVERVIEW\cr
The purpose of \code{Logit} is to combine the following function calls into one, as well as provide ancillary analyses such as as graphics, organizing output into tables and sorting to assist interpretation of the output.  

The basic analysis successively invokes several standard R functions beginning with the standard R function for estimation of the logit model, \code{\link{glm}} with \code{family="binomial"}. The output of the analysis is stored in the object \code{lm.out}, available for further analysis in the R environment upon completion of the \code{Logit} function. By default \code{reg} automatically provides the analyses from the standard R functions, \code{\link{summary}}, \code{\link{confint}} and \code{\link{anova}}, with some of the standard output modified and enhanced. The correlation matrix of the model variables is obtained with \code{\link{cor}} function. The residual analysis invokes \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, and \code{\link{cooks.distance}} functions.  The option for prediction intervals calls the standard generic R function \code{\link{predict}}.  The \code{lessR} \code{\link{den}} function provides the histogram and density plots for the residuals and the \code{\link{ScatterPlot}} function provides the scatter plots of the residuals with the fitted values and of the data for the one-predictor model. 

The default analysis provides the model's parameter estimates and corresponding hypothesis tests and confidence intervals, goodness of fit indices, the ANOVA table, analysis of residuals and influence as well as the fitted value and standard error for each observation in the model. The response variable must be binary with only numeric values of 0 and 1. See the examples of how obtain exclusive 0 and 1 values from character data. 

DATA FRAME\cr
The name \code{mydata} is by default provided by the \code{\link{Read}} function included in this package for reading and displaying information about the data in preparation for analysis. If all the variables in the model are not in the same data frame, the analysis will not be complete.  The data frame does not need to be attached, just specified by name with the \code{dframe} option if the name is not the default \code{mydata}.

GRAPHICS\cr
Two or three default graphs are provided. By default the graphs are written to separate graphics windows (which may overlap each other completely, in which case move the top graphics windows). Or, the \code{graphics.save} option may be invoked to save the graphs to a single pdf file called \code{regOut.pdf}. The directory to which the file is written is displayed on the console text output.

1. A histogram of the residuals includes the superimposed normal and general density plots from the \code{\link{den}} function included in this \code{lessR} package.  The overlapping density plots, which both overlap the histogram, are filled with semi-transparent colors to enhance readability.  

2. A scatterplot of the residuals with the fitted values is also provided from the \code{\link{ScatterPlot}} function included in this package. The point corresponding to the largest value of Cook's distance, regardless of its size, is plotted in red and labeled and the corresponding value of Cook's distance specified in the subtitle of the plot. Also by default all points with a Cook's distance value larger than 1.0 are plotted in red, a value that can be specified to any arbitrary value with the \code{cooks.cut} option.  This scatterplot also includes the \code{\link{lowess}} curve.

3. For models with a single predictor variable, a scatterplot of the data is produced, which also includes the fitted values. As with the density histogram plot of the residuals and the scatterplot of the fitted values and residuals, the scatterplot includes a colored background with grid lines. 

RESIDUAL ANALYSIS\cr
By default the residual analysis lists the data and fitted value for each observation as well as the residual, Studentized residual, Cook's distance and dffits, with the first 20 observations listed and sorted by Cook's distance. The residual displayed is the actual difference between fitted and observed, that is, with the setting in the \code{\link{residuals}} of \code{type="response"}.  The \code{res.sort} option provides for sorting by the Studentized residuals or not sorting at all.  The \code{res.rows} option provides for listing these rows of data and computed statistics statistics for any specified number of observations (rows).  To turn off the analysis of residuals, specify \code{res.rows=0}.

INVOKED R OPTIONS\cr
The \code{\link{options}} function is called to turn off the stars for different significance levels (show.signif.stars=FALSE), to turn off scientific notation for the output (scipen=30), and to set the width of the text output at the console to 120 characters. The later option can be re-specified with the \code{text.width} option.  After \code{reg} is finished with a normal termination, the options are re-set to their values before the \code{reg} function began executing.

COLOR THEME\cr
A color theme for all the colors can be chosen for a specific plot with the \code{colors} option. Or, the color theme can be changed for all subsequent graphical analysis with the \code{lessR} function \code{\link{set}}. The default color theme is \code{blue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in a data frame called \code{mylabels}, obtained from the \code{\link{Read}} function.  If this labels data frame exists, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{formula}}, \code{\link{glm}}, \code{\link{summary.glm}}, \code{\link{anova}}, \code{\link{confint}}, \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, \code{\link{cooks.distance}}
}

\examples{
# Generate random data, place in data frame mydata
n <- 20
X1 <- rnorm(n)
X2 <- rnorm(n)
Y <- .7*X1 + .2*X2 + .6*rnorm(n)
# Y is binary, 0 or 1
Y <- cut(Y, breaks=2, labels=FALSE)
#  instead, if read data with the rad function
#   then the result is the data frame called mydata 
mydata <- data.frame(X1, X2, Y)
rm(Y); rm(X1); rm(X2)
Recode(Y, old=c(1,2), new=c(0,1))

# One-predictor logistic regression
# Provide all default analyses including scatterplot etc.
Logit(Y ~ X1)
# short name
lgt(Y ~ X1)

# Modify the default settings as specified
Logit(Y ~ X1, res.row=8, res.sort="rstudent", digits.d=8, pred=FALSE)

# Multiple logistic regression model
# Provide all default analyses
Logit(Y ~ X1 + X2)
# Save the three plots as pdf files 4 inches square, gray scale
Logit(Y ~ X1, pdf=TRUE, pdf.width=4, pdf.height=4, colors="gray")

# Specify new values of the predictor variables to calculate
#  forecasted values
# Specify an input data frame other than mydata, see help(mtcars)
Logit(am ~ hp + wt + disp, dframe=mtcars,
  X1.new=seq(50,350,50), X2.new=c(2,3), X3.new=c(100,300))

# obtain numeric 0,1 values from character data
data(datEmployee)
# Gender has values of "M" and "F"
# convert to 0 and 1, but the variable Gender now is non-numeric
Recode(Gender, old=c("M", "F"), new=c(0,1), dframe=datEmployee)
# so create a new variable from the "0" and "1" values that has
#  true numeric values with R function as.numeric
Transform(Gender.num=as.numeric(Gender), dframe=datEmployee)
# proceed with the logit regression
Logit(Gender.num ~ Years, dframe=datEmployee)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ logit }



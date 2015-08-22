\name{Regression}
\alias{Regression}
\alias{reg}
\alias{reg.brief}

\title{Regression Analysis}

\description{
Abbreviation: \code{reg}, \code{reg.brief}

Provides a regression analysis with extensive output, including graphics, from a single, simple function call with many default settings, each of which can be re-specified. The computations are obtained from the \code{R} function \code{\link{lm}} and related \code{R} regression functions. The outputs of these functions are re-arranged and collated.

By default the data exists as a data frame with the default name of \code{mydata}, or specify explicitly with the \code{data} option.  Specify the model in the function call as an R \code{\link{formula}}, that is, for a basic model, the response variable followed by a tilde, followed by the list of predictor variables, each pair separated by a plus sign, such as \code{reg(Y ~ X1 + X2)}.  

Output is generated into distinct segments by topic, organized and displayed in sequence by default. When the output is assigned to an object, such as \code{r} in \code{r <- reg(Y ~ X)}, the full or partial output can be accessed for later analysis and/or viewing. A primary such analysis is with \code{knitr} for dynamic report generation, run from R directly or from within \code{RStudio}. The input instructions to \code{knitr} are written comments and interpretation with embedded \code{R} code, called R~Markdown. Doing a \code{knitr} analysis is to "knit" these comments and subsequent output together so that the \code{R} output is embedded in the resulting document, either html, pdf or Word, by default with explanation and interpretation. Generate a complete \code{knitr} set of instructions ready to knit, an R~Markdown file (\code{.Rmd}) from the \code{knitr.file} option. Simply specify the option with a file name, run the Regression function to create the file, and then open the newly created \code{.Rmd} file in \code{RStudio} and click the \code{knit} button to create a formatted document that consists of the statistical results and interpretative comments. See the sections \code{arguments}, \code{value} and \code{examples} for more information. 
}

\usage{
Regression(my.formula, data=mydata, digits.d=NULL, standardize=FALSE,

         knitr.file=NULL, 
         results=getOption("results"), explain=getOption("explain"),
         interpret=getOption("interpret"), document=getOption("document"), 
         code=getOption("code"), 

         text.width=120, brief=getOption("brief"), show.R=FALSE,

         res.rows=NULL, res.sort=c("cooks","rstudent","dffits","off"), 
         pred.rows=NULL, pred.sort=c("predint", "off"),
         subsets=NULL, cooks.cut=1, 

         scatter.coef=TRUE, scatter.3D=FALSE, graphics=TRUE,

         X1.new=NULL, X2.new=NULL, X3.new=NULL, X4.new=NULL, 
         X5.new=NULL, X6.new=NULL,

         pdf=FALSE, pdf.width=5, pdf.height=5, refs=FALSE, 
         fun.call=NULL, \ldots)

reg(\ldots) 

reg.brief(\ldots, brief=TRUE) 
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.  For
       example, for a response variable named Y and two predictor variables, X1 and 
       X2, specify the corresponding linear model as Y ~ X1 + X2.}
  \item{data}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{digits.d}{For the Basic Analysis, it provides the number of decimal digits,
        set by default to at least 2 or the largest number of digits in the values 
        of the response variable plus 1.}
  \item{standardize}{Standardize each of the variables in the regression model before
        conducting the analysis.}
  \item{knitr.file}{File name for the file of knitr instructions to be written, if specified.
        The file type is .Rmd, which automatically opens in RStudio, but it is a simple
        text file that can be edited with any text editor, including RStudio.}
  \item{results}{By default TRUE. If set to \code{FALSE} the \code{knitr} results
        are not provided, relying upon the interpretations.  Can set globally with
        set(results=FALSE).}
  \item{explain}{By default TRUE. If set to \code{FALSE} the \code{knitr} explanations
        of the results are not provided. Can set globally with set(explain=FALSE).}
  \item{interpret}{By default TRUE. If set to \code{FALSE} the \code{knitr} interpretations
        of the results are not provided.  Can set globally with set(interpret=FALSE).}
  \item{document}{By default TRUE If set to \code{FALSE} the \code{knitr} code documentation
        is not provided.  Can set globally with set(document=FALSE).}
  \item{code}{By default TRUE. If set to \code{FALSE} the \code{knitr} interpretations
        of the results are not provided. Can set globally with set(code=FALSE).}
  \item{text.width}{Width of the text output at the console.}
  \item{brief}{If set to \code{TRUE}, reduced text output. Can change system default
        with \code{\link{set}} function.}
  \item{show.R}{Display the R instructions that yielded the \code{lessR} output, albeit without
        the additional formatting of the results such as combining output of different 
        functions into a table.}
  \item{res.rows}{Default is 20, which lists the first 20 rows of data sorted by the
       specified sort criterion.  To disable residuals, specify a value of 0.  To see 
       the output for all observations, specify a value of \code{"all"}.}
  \item{res.sort}{Default is \code{"cooks"}, for specifying Cook's distance as the sort
       criterion for the display of the rows of data and associated residuals. Other values 
       are \code{"rstudent"} for externally Studentized residuals, \code{"dffits"} for dffits
       and \code{"off"} to not sort the rows of data.}
  \item{pred.rows}{Default is 4, which lists prediction intervals only for the
       first, middle and last 4 rows of data, unless there are 25 or less
       rows of data when all rows are displayed.  To disable prediction intervals,
       specify a value of 0.  To see the output for rows of data, specify a value of
       \code{"all"}.}
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
  \item{scatter.3D}{A 3D scatterplot with best fitting regression plane, which applies only 
       to models with exactly two predictor variables. To obtain, set to \code{TRUE}.}
  \item{graphics}{Produce graphics. Default is \code{TRUE}.  In \code{knitr} can be useful
       to set to \code{FALSE} so that \code{\link{regPlot}} can be used to place
       the graphics within the output file.}
  \item{X1.new}{Values of the first listed numeric predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X2.new}{Values of the second listed numeric predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X3.new}{Values of the third listed numeric predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X4.new}{Values of the fourth listed numeric predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X5.new}{Values of the fifth listed numeric predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{X6.new}{Values of the sixth listed numeric predictor variable for which forecasted values
        and corresponding prediction intervals are calculated.}
  \item{pdf}{If \code{TRUE}, then graphics are written to pdf files.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{refs}{If \code{TRUE}, then list the references for R and the packages used from
        which functions were used to generate the output.}
  \item{fun.call}{Function call. Used internally with \code{knitr} to pass the 
        function call when obtained from the abbreviated function call \code{reg}.
        Not usually invoked by the user.}
  \item{\dots}{Other parameter values for R function \code{\link{lm}} which provides the
        core computations.} 
}


\details{
OVERVIEW\cr
The purpose of \code{Regression} is to combine the following function calls into one, as well as provide ancillary analyses such as as graphics, organizing output into tables and sorting to assist interpretation of the output, as well as run through \code{knitr}, such as with \code{RStudio}.  

The basic analysis successively invokes several standard R functions beginning with the standard R function for estimation of a linear model, \code{\link{lm}}. The output of the analysis of \code{\link{lm}} is stored in the object \code{lm.out}, available for further analysis in the R environment upon completion of the \code{Regression} function. By default \code{reg} automatically provides the analyses from the standard R functions, \code{\link{summary}}, \code{\link{confint}} and \code{\link{anova}}, with some of the standard output modified and enhanced. The correlation matrix of the model variables is obtained with \code{\link{cor}} function. The residual analysis invokes \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, and \code{\link{cooks.distance}} functions.  The option for prediction intervals calls the standard R function \code{\link{predict}}, once with the argument \code{interval="confidence"} and once with \code{interval="prediction"}. The \code{lessR} \code{\link{Density}} function provides the histogram and density plots for the residuals and the \code{\link{ScatterPlot}} function provides the scatter plots of the residuals with the fitted values and of the data for the one-predictor model. The \code{pairs} function provides the scatterplot matrix of all the variables in the model. Thomas Lumley's \code{leaps} package contains the \code{leaps} function that provides the analysis of the fit of all possible model subsets. The \code{car} package provides Henric Nilsson and John Fox's \code{vif} function for the computation of the variance inflation factors for the collinearity analysis. The \code{scatter3d} function from Fox and Weisberg's \code{car} package provides the interactive 3d scatterplot for models with exactly two predictor variables.

INPUT DATA FRAME\cr
The name \code{mydata} is by default provided by the \code{\link{Read}} function included in this package for reading and displaying information about the data in preparation for analysis. If all the variables in the model are not in the same data frame, the analysis will not complete.  The data frame does not need to be attached, just specified by name with the \code{data} option if the name is not the default \code{mydata}.

TEXT OUTPUT\cr
The output is produced in pieces by topic (see \code{values} below), automatically collated by default in the final output. But the pieces are available for later reference if the output of the function is directed toward an object, such as \code{r} in \code{r <- reg(Y ~ X)}. This is especially useful if the pieces are accessed within \code{knitr} or individual pieces are displayed at the console.

The text output is organized to provide the most relevant information while at the same time minimizing the total amount of output, particularly for analyses with large numbers of observations (rows of data), the display of which is by default restricted to only the most interesting or representative observations in the analyses of the residuals and predicted values. Additional economy can be obtained by invoking the \code{brief=TRUE} option, or run \code{reg.brief}, which limits the analysis to just the basic analysis of the estimated coefficients and fit. 

knitr\cr
A file ready for input into \code{knitr} can be obtained by specifying a value for \code{knitr.file}. For the specified file name, the directory to which the file is written is displayed on the console text output, and the file type \code{.Rmd} is automatically appended to the specified name if it is not included in the specification. Process with the \code{knitr} button in \code{RStudio}, or with the \code{knit} function from the \code{knitr} package and the \code{render} function from the \code{rmarkdown} package. 

The output from \code{knitr.file} is conceptually partitioned into five parts:  results, explanations of the results, interpretations of the results, documentation o the code, and the code itself. By default all available output is genreated but the flags \code{results}, \code{explain}, \code{interpret}, \code{document}, \code{code} can be set to \code{FALSE} to reduce the output. The options can be specified in a specific function all or set globally, such as with \code{options(explain=FALSE)}. Turning off all five flags leaves just the outline of the potential output and a bare minimum of results.

Both any existing variable labels and variable units are included in the output to the R~Markdown file. Any varible units set as a dollar, are set as USD dollars and cents in the output, displayed with a \$. 

The default analysis provides as text output to the console the model's parameter estimates and corresponding hypothesis tests and confidence intervals, goodness of fit indices, the ANOVA table, correlation matrix of the model's variables, analysis of residuals and influence as well as the confidence and prediction intervals for each observation in the model. Also provided, for multiple regression models, collinearity analysis of the predictor variables and adjusted R-squared for the corresponding models defined by each possible subset of the predictor variables. 

DECIMAL DIGITS\cr
The number of decimal digits displayed on the output is, by default, the maximum number of decimal digits for all the data values of the response variable. Or, this value can be explicitly specified with the \code{digits.d} parameter.

GRAPHICS OUTPUT\cr
Three default graphs are provided. When running \code{R} by itself, by default the graphs are written to separate graphics windows (which may overlap each other completely, in which case move the top graphics windows). Or, the \code{pdf} option may be invoked to save the graphs to a single pdf file called \code{regOut.pdf}. Within \code{RStudio} the graphs are successively written to the \code{Plots} window. Within \code{knitr} from \code{RStudio} the graphics will all appear by default at the beginning of the output. Or set to \code{graphics=FALSE}, and generate them individually with the accompanying function \code{\link{regPlot}} at the desired location within the file.

1. A histogram of the residuals includes the superimposed normal and general density plots from the \code{\link{Density}} function included in this \code{lessR} package.  The overlapping density plots, which both overlap the histogram, are filled with semi-transparent colors to enhance readability.  

2. A scatterplot of the residuals with the fitted values is also provided from the \code{\link{ScatterPlot}} function included in this package. The point corresponding to the largest value of Cook's distance, regardless of its size, is plotted in red and labeled and the corresponding value of Cook's distance specified in the subtitle of the plot. Also by default all points with a Cook's distance value larger than 1.0 are plotted in red, a value that can be specified to any arbitrary value with the \code{cooks.cut} option.  This scatterplot also includes the \code{\link{lowess}} curve.

3. For models with a single predictor variable, a scatterplot of the data is produced, which also includes the regression line and corresponding confidence and prediction intervals. As with the density histogram plot of the residuals and the scatterplot of the fitted values and residuals, the scatterplot includes a colored background with grid lines. For multiple regression models, a scatterplot matrix of the variables in the model with the \code{\link{lowess}} best-fit line of each constituent scatterplot is produced. If the \code{scatter.coef} option is invoked, each scatterplot in the upper-diagonal of the correlation matrix is replaced with its correlation coefficient.

RESIDUAL ANALYSIS\cr
By default the residual analysis lists the data and fitted value for each observation as well as the residual, Studentized residual, Cook's distance and dffits, with the first 20 observations listed and sorted by Cook's distance.  The \code{res.sort} option provides for sorting by the Studentized residuals or not sorting at all.  The \code{res.rows} option provides for listing these rows of data and computed statistics statistics for any specified number of observations (rows).  To turn off the analysis of residuals, specify \code{res.rows=0}.

PREDICTION INTERVALS\cr
The output for the confidence and prediction intervals includes a table with the data and fitted value for each observation, the lower and upper bounds for the confidence interval and the prediction interval, and the wide of the prediction interval.  The observations are sorted by the lower bound of each prediction interval.  If there are 25 or more observations then the information for only the first three, the middle three and the last three observations is displayed. To turn off the analysis of prediction intervals, specify \code{pred.rows=0}, which also removes the corresponding intervals from the scatterplot produced with a model with exactly one predictor variable, yielding just the scatterplot and the regression line.

The data for the default analysis of the prediction intervals is for the values of the predictor variables for each observation, that is, for each row of the data. New values of the predictor variables can be specified for the calculation of the prediction intervals by providing values for the options \code{X1.new} for the values of the first listed predictor variable in the model, \code{X2.new} for the second listed predictor variable, and so forth for up to five predictor variables, and all predictor variables are numeric. To provide these values, use functions such as \code{\link{seq}} for specifying a sequence of values and \code{\link{c}} for specifying a vector of values. For multiple regression models, all combinations of the specified new values for all of the predictor variables are analyzed.

RELATIONS AMONG THE VARIABLES\cr
By default the correlation matrix of all the variables in the model is displayed, and, for multiple regression models, collinearity analysis is provided with the \code{vif} function from the Fox and Weisberg (2011) \code{car} package. Also provided are the first 50 models with the largest R squared adjusted from each possible model from an analysis of all possible subsets of the predictor variables.  This all subsets analysis requires the \code{leaps} function from the \code{leaps} package. These contributed packages are automatically loaded if available.  To turn off the all possible sets option, set \code{subsets=FALSE}.


INVOKED R OPTIONS\cr
The \code{\link{options}} function is called to turn off the stars for different significance levels (show.signif.stars=FALSE), to turn off scientific notation for the output (scipen=30), and to set the width of the text output at the console to 120 characters. The later option can be re-specified with the \code{text.width} option.  After \code{Regression} is finished with a normal termination, the options are re-set to their values before the \code{Regression} function began executing.

COLOR THEME\cr
A color theme for all the colors can be chosen for a specific plot with the \code{colors} option. Or, the color theme can be changed for all subsequent graphical analysis with the \code{lessR} function \code{\link{set}}. The default color theme is \code{dodgerblue}, but a gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{set}}. 

VARIABLE LABELS\cr
If variable labels exist, then the corresponding variable label is by default listed as the label for the horizontal axis and on the text output. For more information, see \code{\link{Read}}.
}

\value{
The output can optionally be returned and saved into an \code{R} object, otherwise it simply appears at the console. The components of this object are redesigned in \code{lessR} version 3.3 into (a) pieces of text that form the readable output and (b) a variety of statistics. The readable output are character strings such as tables amenable for viewing and interpretation. The statistics are numerical values amenable for further analysis, such as to be referenced in a subsequent \code{knitr} document. The motivation of these three types of output is to facilitate \code{knitr} documents, as the name of each piece, preceded by the name of the saved object followed by a \$, can be inserted into the \code{knitr} document (see \code{examples}).

TEXT OUTPUT\cr
\code{out_background}: variables in the model, rows of data and retained\cr
\code{out_estimates}: estimated coefficients, hypothesis tests and confidence intervals\cr
\code{out_fit}: fit indices\cr
\code{out_anova}: Analysis of variance\cr
\code{out_cor}: correlations among all variables in the model\cr
\code{out_collinear}: collinearity analysis\cr
\code{out_subsets}: R squared adjusted for all (or many) possible subsets\cr
\code{out_residuals}: residuals\cr
\code{out_predict}: analysis of residuals and influence\cr
\code{out_cite}: list of packages from other developers used in the analysis\cr
\code{out_ref}: references if selected on the \code{Regression} function call\cr
\code{out_knitr.file}: lists the name and location of the \code{knitr} instructions\cr
\code{out_plots}: list of plots generated if more than one\cr

Separated from the rest of the text output are the major headings, which can then be deleted from custom collations of the output.
\code{out_title_bck}: BACKGROUND\cr
\code{out_title_basic}: BASIC ANALYSIS\cr
\code{out_title_rel}: RELATIONS AMONG THE VARIABLES\cr
\code{out_title_res}: ANALYSIS OF RESIDUALS AND INFLUENCE\cr
\code{out_title_pred}: FORECASTING ERROR\cr

STATISTICS\cr
\code{fun.call}: function call that generated the analysis\cr
\code{formula}: model formula that specifies the model\cr
\code{n.vars}: number of variables in the model\cr
\code{n.obs}: number of rows of data submitted for analysis\cr
\code{n.keep}: number of rows of data retained in the analysis\cr
\code{coefficients}: estimated regression coefficients\cr
\code{sterrs}: standard errors of the estimated coefficients\cr
\code{tvalues}: t-values of the estimated coefficients for null of 0\cr
\code{pvalues}: p-values from the t-tests of the estimated coefficients\cr
\code{cilb}: lower bound of 95\% confidence interval of estimate\cr
\code{ciub}: upper bound of 95\% confidence interval of estimate \cr
\code{anova_model}: model df, ss, ms, F-value and p-value\cr
\code{anova_residual}: residual df, ss and ms\cr
\code{anova_total}: total df, ss and ms\cr
\code{se}: standard deviation of the residuals\cr
\code{resid_range}: 95\% range of normally distributed fitted residuals\cr
\code{Rsq}: R-squared\cr
\code{Rsqadj}: adjusted R-squared\cr
\code{PRESS}: PRESS sum of squares\cr
\code{RsqPRESS}: PRESS R-squared\cr
\code{cor}: correlation matrix of all variables in the model\cr
\code{tolerances}: tolerance of each predictor variable for collinearity analysis\cr
\code{VIF}: variance inflation factor for each predictor variable\cr
\code{resid.max}: five largest values of the residuals on which the ouput is sorted\cr
\code{pred_min_max}: Rows with the smallest and largest prediction intervals\cr
\code{residuals}: residuals\cr
\code{fitted.values}: fitted values\cr
\code{cooks.distance}: Cook's distance\cr
\code{model}: data retained for the analysis\cr
\code{terms}: terms specified for the analysis\cr

Although not typically needed for analysis, if the regression output is assigned to an object named, for example, \code{r}, then the complete contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(r)}. Invoking the \code{\link{class}} function on the saved object reveals a class of {out_all}. The class of each of the text pieces of output is {out_piece}.
}

\references{
Fox, J., & Weisberg, S. (2011). An R companion to applied regression (Second ed.). Thousand Oaks CA: Sage.

Lumley, T., \code{leaps} function from the \code{leaps} package.\cr
Nilsson, H. and Fox, J., \code{vif} function from the \code{car} package.\cr

Gerbing, D. W. (2013). R Data Analysis without Programming, Chapters 9 and 10, NY: Routledge.

Xie, Y. (2013). Dynamic Documents with R and knitr, Chapman & Hall/CRC The R Series.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{formula}}, \code{\link{lm}}, \code{\link{summary.lm}}, \code{\link{anova}}, \code{\link{confint}}, \code{\link{fitted}}, \code{\link{resid}}, \code{\link{rstudent}}, \code{\link{cooks.distance}}, \code{\link{Nest}}, \code{\link{regPlot}}
}

\examples{
# Read internal data set
mydata <- rd("Reading", format="lessR", quiet=TRUE)
# do not need all this data, so take only 40% to reduce CPU time
mydata <- Subset(random=.4)

# One-predictor regression
# Provide all default analyses including scatterplot etc.
Regression(Reading ~ Verbal)
# short name of function call
reg(Reading ~ Verbal)
# Provide only the brief analysis on the standardized variables
reg.brief(Reading ~ Verbal, standardize=TRUE)

# Access the pieces of output, here in an object named \code{r}
r <- reg(Reading ~ Verbal + Absent + Income)
# Display all output at the console in the standard sequence
r
# list the names of all the saved components
names(r)
# Display just the estimated coefficients and their inferential analysis
r$out_estimates

# Generate knitr instructions with the option: knitr.file
# Output file here will be read.Rmd, a simple text file that can
#   be edited with any text editor including RStudio from which it
#   can be knit to generate dynamic output to a Word document,
#   pdf file or html file
reg(Reading ~ Verbal + Absent, knitr.file="read")
# knitr.file with no explanations of the results, but does
#  include intepretations of the results and documentation of the code
reg(Reading ~ Verbal + Absent, knitr.file="read2", explain=FALSE)

# Modify the default settings as specified
Regression(Reading ~ Verbal, res.row=8, res.sort="rstudent", pred.rows=0, digits.d=4)

# Multiple regression model
# Provide all default analyses
Regression(Reading ~ Verbal + Absent + Income)
# Save the three plots as pdf files 4 inches square
Regression(Reading ~ Verbal, pdf=TRUE, pdf.width=4, pdf.height=4)

# Compare nested models
# Reduced model:  Reading ~ Verbal
# Full model:  Reading ~ Verbal + Income + Absent
Nest(Reading, Verbal, c(Income, Absent))

# Specify new values of the predictor variables to calculate
#  forecasted values and the corresponding prediction intervals
# Specify an input data frame other than mydata, see help(mtcars)
Regression(mpg ~ hp + wt + disp, data=mtcars, 
  X1.new=seq(50,350,50), X2.new=c(2,3), X3.new=c(100,300))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }


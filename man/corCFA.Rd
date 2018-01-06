\name{corCFA}
\alias{corCFA}
\alias{cfa}
\alias{scales}

\title{Confirmatory Factor Analysis of a Multiple Indicator Measurement Model}

\description{
Abbreviation: \code{cfa}

A multiple indicator measurement model partitions a set of indicators, such as items on a survey, into mutually exclusive groups with one common factor per group of indicators. From the input correlation matrix of the indicator variables, this procedure uses iterated centroid estimation to estimate the coefficients of the model, the factor pattern and factor-factor correlations, as well as the correlations of each factor with each indicator. The analysis is an adaptation and extension of John Hunter's program PACKAGE (Hunter and Cohen, 1969).

Corresponding scale reliabilities are provided, as well as the residuals, the difference between the indicator correlations and those predicted by the model.  To visualize the relationships, a heat map of the re-ordered correlation matrix is also provided, with indicator communalities in the diagonal. To understand the meaning of each factor, the corresponding indicator content is displayed for each factor if the indicators have been read as variable labels. Also provides the code to obtain the maximum likelihood solution of the corresponding multiple indicator measurement model (MIMM) with the \code{cfa} function from the \code{lavaan} package.

The \code{scales} is a wrapper that retains 1's in the diagonal of the indicator correlation matrix, so provides scale reliabilities and observed indicator-scale and scale-scale correlations.

Output is generated into distinct pieces by topic, organized and displayed in sequence by default. When the output is assigned to an object, such as \code{f} in \code{f <- cfa(Fac =~ X1 + X2 + X3)}, the full or partial output can be accessed for later analysis and/or viewing. A primary such analysis is with \code{knitr} for dynamic report generation, run from, for example, \code{RStudio}. The input instructions written to the R~Markdown file are written comments and interpretation with embedded \code{R} code. Doing a \code{knitr} analysis is to "knit" these comments and subsequent output together so that the \code{R} output is embedded in the resulting document, either html, pdf or Word, by default with explanation and interpretation. Generate a complete R~Markdown set of instructions ready to knit from the \code{Rmd} option. Simply specify the option and create the file and then open in \code{RStudio} and click the \code{knit} button to create a formatted document that consists of the statistical results and interpretative comments. See the following sections \code{arguments}, \code{value} and \code{examples} for more information. 
}

\usage{
corCFA(mimm=NULL, x=mycor, data=mydata, fac.names=NULL, 

         Rmd=NULL, explain=getOption("explain"),
         interpret=getOption("interpret"), results=getOption("results"),

         labels=c("include", "exclude", "only"),

         min.cor=.10, min.res=.05, iter=50, grid=TRUE, 

         resid=TRUE, item.cor=TRUE, sort=TRUE,

         main=NULL, heat.map=TRUE, bottom=3, right=3, 

         pdf.file=NULL, width=5, height=5,

         F1=NULL, F2=NULL, F3=NULL, F4=NULL, F5=NULL,
         F6=NULL, F7=NULL, F8=NULL, F9=NULL, F10=NULL,
         F11=NULL, F12=NULL,

         fun.call=NULL)

cfa(\ldots)

scales(\ldots, iter=0, resid=FALSE, item.cor=FALSE, sort=FALSE, heat.map=FALSE)
}

\arguments{
  \item{mimm}{Multiple indicator measurement model, a character string with the
        specification of each factor on a separate line: the factor
        name, an equals sign, and the indicators separated by plus signs. 
        Each indicator is assigned to only one factor.}
  \item{x}{Correlation matrix to be analyzed.}
  \item{data}{Data frame of the original data to be checked for any
       variable labels, usually indicator (item) content. This is not to
       calculate correlations, which is separately provided for by the
       \code{lessR} function \code{\link{Correlation}}.}
  \item{fac.names}{Optional factor names for the original, non-lavaan model specification.}
  \item{Rmd}{File name for the file of R Markdown instructions to be written, if specified.
        The file type is .Rmd, which automatically opens in RStudio, but it is a simple
        text file that can be edited with any text editor, including RStudio.}
  \item{explain}{If set to \code{FALSE} the explanations of the results are not
        provided in the R~Markdown file. Set globally with options(explain=FALSE).}
  \item{interpret}{If set to \code{FALSE} the interpretations
        of the results are not provided in the R~Markdown file. Set globally with
        options(interpret=FALSE).}
  \item{results}{If set to \code{FALSE} the results
        are not provided in the R~Markdown file, relying upon the interpretations.
        Set globally with options(results=FALSE).}
  \item{labels}{If "include" or "exclude" then variable labels are displayed
       (if available) or not, organized by the items within each factor.
       If "only" then no data analysis performed, only the display of the
       labels by factor.}
  \item{min.cor}{Minimum correlation to display. To display all, set to 0.}
  \item{min.res}{Minimum residual to display. To display all, set to 0.}
  \item{iter}{Number of iterations for communality estimates.}
  \item{grid}{If \code{TRUE}, then separate items in different factors
        by a grid of horizontal and vertical lines in the output 
        correlation matrix.}
  \item{resid}{If \code{TRUE}, then calculate and print the residuals.}
  \item{item.cor}{If \code{TRUE}, display the indicator correlations.}
  \item{sort}{If \code{TRUE}, re-order the output correlation matrix so that
        indicators within each factor are sorted by their factor loadings on
        their own factor.}
  \item{main}{Graph title of heat map. Set to \code{main=""} to turn off.}
  \item{heat.map}{If \code{TRUE}, display a heat map of the indicator
        correlations with indicator communalities in the diagonal.}
  \item{bottom}{Number of lines of bottom margin of heat map.}
  \item{right}{Number of lines of right margin of heat map.}
  \item{pdf.file}{Name of the pdf file to which graphics are redirected.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{F1}{Variables that define Factor 1.}
  \item{F2}{Variables that define Factor 2.}
  \item{F3}{Variables that define Factor 3.}
  \item{F4}{Variables that define Factor 4.}
  \item{F5}{Variables that define Factor 5.}
  \item{F6}{Variables that define Factor 6.}
  \item{F7}{Variables that define Factor 7.}
  \item{F8}{Variables that define Factor 8.}
  \item{F9}{Variables that define Factor 9.}
  \item{F10}{Variables that define Factor 10.}
  \item{F11}{Variables that define Factor 11.}
  \item{F12}{Variables that define Factor 12.}
  \item{fun.call}{Function call. Used internally with \code{knitr} to pass the 
        function call when obtained from the abbreviated function call \code{cfa}.
        Not usually invoked by the user.}
  \item{\dots}{Parameter values.}
}

\details{
OVERVIEW\cr
A multiple indicator measurement model defines one or more latent variables, called factors, in terms of mutually exclusive sets of indicator variables, such as items from a questionnaire or survey.  That is, each factor is defined by a unique set or group of indicators, and each indicator only contributes to the definition of one factor. Two sets of parameters are estimated by the model, the factor pattern coefficients, the lambda's, and the factor-factor correlations, the phi's. Also estimated here are the correlations of each indicator with the other factors. 

INPUT\cr
Unless \code{labels="only"}, the analysis requires the correlation matrix of the indicators and the specification of the groups of indicators, each of which defines a factor in the multiple indicator measurement model.  The default name for the indicator correlation matrix is \code{mycor}, which is also the default name of the matrix produced by the \code{lessR} function \code{\link{Correlation}} that computes the correlations from the data, as well as the name of the matrix read by the \code{lessR} function \code{\link{corRead}} that reads the already computed correlation matrix from an external file.

For versions of \code{lessR} after 3.3, the correlation matrix computed by \code{\link{Correlation}} is now a list element called \code{cors} within the returned list. For example, \code{mycor$cors} from \code{mycor <- cr(mydata)}. The function \code{corCFA} automatically finds this correlation matrix from just entering the entire list name of the returned list, \code{mycor}, or the specific location, \code{mycor$cors}, or as a stand-alone numerical matrix as done in versions of \code{lessR} previous to 3.3. 

The data frame from which the correlation matrix was computed is required only if any associated variable labels are listed, organized by the items within each factor. By default, \code{labels="include"}, these labels are listed as part of the analysis if they are available.

Define the constituent variables, the indicators, of each factor with a listing of each variable by its name in the correlation matrix. Each of the up to 12 factors is named F1, F2, etc. If the specified variables of a factor are in consecutive order in the input correlation matrix, the list can be specified by listing the first variable, a colon, and then the last variable.  To specify multiple variables, a single variable or a list, separate each by a comma, then invoke the R combine or \code{\link{c}} function, preceded by the factor's name and an equals sign. For example, if the first factor is defined by variables in the input correlation matrix from m02 through m05, and the variable Anxiety, then define the factor in the \code{corCFA} function call according to \code{F1=c(m02:m05,Anxiety)}. 

OUTPUT\cr
The result of the analysis is the correlation matrix of the indicator variables and resulting factors, plus the reliability analysis of the observed total scores or scale that corresponds to each factor.  Each scale is defined as an unweighted composite. The corresponding code to analyze the model with the \code{cfa} function from the \code{lavaan} package is also provided with the default maximum likelihood estimation procedure. The comparable \code{lavaan} solution appears in the column that represents the fully standardized solution, factors and indicators, \code{Std.all}, the last column of the solution output. If the \code{lavaan} library is loaded, then explicitly refer to the \code{lessR} function \code{cfa} with \code{lessR::cfa} and the corresponding \code{lavaan} function with \code{lavaan::cfa}.  

VARIABLE LABELS\cr
To display the indicator content, first read the indicators as variable labels with the \code{lessR} function \code{\link{Read}}.  If this labels data frame exists, then the corresponding variable labels, such as the actual items on a survey, are listed by factor. For more information, see \code{\link{Read}}.

HEAT MAP\cr
To help visualize the overall pattering of the correlations, the corresponding heat map of the item correlation matrix with communalities is produced when \code{heat.map=TRUE}, the default. As is true of the output correlation matrix, the correlations illustrated in the heat map are also sorted by their ordering within each factor. The corresponding color scheme is dictated by the system setting, according to the \code{lessR} function \code{\link{style}}.  The default color scheme is \code{blue}.

ESTIMATION PROCEDURE\cr
The estimation procedure is centroid factor analysis, which defines each factor, parallel to the definition of each scale score, as the unweighted composite of the corresponding items for that scale.  The latent variables are obtained by replacing the 1's in the diagonal of the indicator variable correlation matrix with communality estimates.  These estimates are obtained by iterating the solution to the specified number of iterations according to \code{iter}, which defaults to 50.

A communality is the percentage of the item's correlation attributable to, in this situation of a multiple indicator measurement model, its one underlying factor.  As such, the communality is comparable to the item correlations for items within the same factor, which are also due only to the influence of the one common, underlying factor. A value of 0 for \code{iter} implies that the 1's remain in the observed variable correlation matrix, which then means that there are no latent factors defined.  Instead the resulting correlation matrix is of the observed scale scores and the component items.
}

\value{

TEXT OUTPUT\cr
\code{out_labels}: variables in the model\cr
\code{out_reliability}: reliability analysis with alpha and omega\cr
\code{out_indicators}: solution in terms of the analysis of each indicator\cr
\code{out_solution}: full solution\cr
\code{out_residuals}: residuals\cr
\code{out_res_stats}: stats for residuals\cr
\code{out_lavaan}: lavaan model specification\cr

Separated from the rest of the text output are the major headings, which can then be deleted from custom collations of the output.
\code{out_title_scales}: scales\cr
\code{out_title_rel}: reliability analysis\cr
\code{out_title_solution}: solution\cr
\code{out_title_residuals}: residual analysis\cr
\code{out_title_lavaan}: lavaan specification\cr

STATISTICS\cr
Returns a list of six components.\cr
1. \code{ff.cor}: matrix of the factor correlations\cr
2. \code{if.cor}: matrix of the indicator-factor correlations that includes the estimated pattern coefficients of the model that link a factor to its indicators\cr
3. \code{diag.cor}: the indicator communalities\cr
4. \code{alpha}: coefficient alpha for each set of indicators\cr
5. \code{omega}: if a factor analysis with communality estimates (\code{iter} > 0), contains coefficient omega for each set of indicators\cr
6. \code{pred}: matrix of correlations predicted by the model and its estimates 
7. \code{resid}: matrix of raw indicator residuals defined as the observed correlation minus that predicted by the model and its estimates 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 11, NY: Routledge.

Gerbing, D. W., & Hamilton, J. G. (1994). The surprising viability of a simple alternate estimation procedure for the construction of large-scale structural equation measurement models. Structural Equation Modeling: A Multidisciplinary Journal, 1, 103-115.

Hunter, J. E., Gerbing, D. W., & Boster, F. J. (1982). Machiavellian beliefs and personality: The construct invalidity of the Machiavellian dimension. Journal of Personality and Social Psychology, 43, 1293-1305.

Hunter, J. & Cohen, J. (1969). PACKAGE: A system of computer routines for the analysis of correlational data. Educational and Psychological Measurement, 1969, 29, 697-700.

Yves Rosseel (2012). lavaan: An R Package for Structural Equation
Modeling. Journal of Statistical Software, 48(2), 1-36. URL
http://www.jstatsoft.org/v48/i02/.
}

\seealso{
\code{\link{Correlation}}.
}

\examples{
# perfect input correlation matrix for two-factor model
# Population Factor Pattern of the 3 items for each respective
#   Factor: 0.8, 0.6, 0.4
# Population Factor-Factor correlation: 0.3
mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("X1", "X2", "X3", "X4", "X5", "X6")
rownames(mycor) <- colnames(mycor)

# the confirmatory factor analysis
# first three variables with first factor, last three with second
# default correlation matrix is mycor
MeasModel <- 
" 
   First =~ X1 + X2 + X3 
   Second =~ X4 + X5 + X6
"
c <- cfa(MeasModel)

# access the solution directly by saving to an object called fit
cfa(MeasModel)
fit <- cfa(MeasModel)
fit
# get the pattern coefficients from the communalities
lambda <- sqrt(fit$diag.cor)
lambda

# alternative specification described in Gerbing(2014),
#   retained to be consistent with that description
# can specify the items with a colon and with commas
# abbreviated form of function name: cfa
cfa(F1=c(X4,X5,X6), F2=X1:X3)

# component analysis, show observed scale correlations
scales(F1=X1:X3, F2=X4:X6)

# produce a gray scale heat map of the item correlations
#   with communalities in the diagonal
# all subsequent graphics are in gray scale until changed
style("gray")
corCFA(F1=X1:X3, F2=X4:X6)

# access the lessR data set called datMach4, with variable labels
mydata <- Read("Mach4", format="lessR", quiet=TRUE)
# calculate the correlations and store in mycor
mycor <- cr(m01:m20)
# specify measurement model in Lavaan notation
MeasModel <- 
" 
   Deceit =~ m07 + m06 + m10 + m09 
   Trust =~ m12 + m05 + m13 + m01 
   Cynicism =~ m11 + m16 + m04 
   Flattery =~ m15 + m02 
"
# confirmatory factor analysis of 4-factor solution of Mach IV scale
# Hunter, Gerbing and Boster (1982)
# generate R Markdown instructions with the option: Rmd
# Output file will be m4.Rmd, a simple text file that can
#   be edited with any text editor including RStudio, from which it
#   can be knit to generate dynamic output such as to a Word document
c <- cfa(MeasModel, Rmd="m4")
# view all the output
c
# view just the scale reliabilities
c$out_reliability

# analysis of item content only
cfa(MeasModel, data=mydata, labels="only")


# bad fitting model to illustrate indicator diagnostics
mycor <- corReflect(vars=c(m20))
MeasModel <- 
" 
   F1 =~ m06 + m09 + m19
   F2 =~ m07
   F3 =~ m04 + m11 + m16 
   F4 =~ m15 + m12 + m20 + m18 
"
cfa(MeasModel)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ factor analysis }






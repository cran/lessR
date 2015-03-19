\name{corCFA}
\alias{corCFA}
\alias{cfa}
\alias{scales}

\title{Confirmatory Factor Analysis of a Multiple Indicator Measurement Model}

\description{
Abbreviation: \code{cfa}

A multiple indicator measurement model partitions a set of indicators, such as items on a survey, into mutually exclusive groups with one common factor per group of indicators. From the input correlation matrix of the indicator variables, this procedure uses iterated centroid estimation to estimate the coefficients of the model, the factor pattern and factor-factor correlations, as well as the correlations of each factor with each indicator. The analysis is provided by Fortran code, adapted from John Hunter's (1969) program PACKAGE (Hunter and Cohen, 1969).

Corresponding scale reliabilities are provided, as well as the residuals, the difference between the indicator correlations and those predicted by the model.  To visualize the relationships, a heat map of the re-ordered correlation matrix is also provided, with indicator communalities in the diagonal. To understand the meaning of each factor, the corresponding indicator content is displayed for each factor if the indicators have been read as variable labels. Also provides the code to obtain the maximum likelihood solution of the corresponding multiple indicator measurement model (MIMM) with the \code{cfa} function from the \code{lavaan} package.

The \code{scales} is a wrapper, which retains 1's in the diagonal of the indicator correlation matrix, so provides scale reliabilities and observed indicator-scale and scale-scale correlations.
}

\usage{
corCFA(x=mycor, data=mydata,

         labels=c("include", "exclude", "only"),

         iter=50, resid=TRUE, item.cor=TRUE, sort=TRUE,

         main=NULL, heat.map=TRUE, bottom=3, right=3, 

         pdf.file=NULL, pdf.width=5, pdf.height=5,

         F1=NULL, F2=NULL, F3=NULL, F4=NULL, F5=NULL,
         F6=NULL, F7=NULL, F8=NULL, F9=NULL, F10=NULL,
         F11=NULL, F12=NULL)

cfa(\ldots)

scales(\ldots, iter=0, resid=FALSE, item.cor=FALSE, sort=FALSE, heat.map=FALSE)
}

\arguments{
  \item{x}{Correlation matrix to be analyzed.}
  \item{data}{Data frame of the original data to be checked for any
       variable labels, usually indicator content. This is not to calculate
       correlations, which is separately provided for by the \code{lessR}
       function \code{\link{Correlation}}.}
  \item{labels}{If "include" or "exclude" then variable labels are displayed
       or not, organized by the items within each factor. If "only" then no
       data analysis performed, only the display of the labels by factor.}
  \item{iter}{Number of iterations for communality estimates.}
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
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{F1}{Variables, listed by ordinal position, that define Factor 1.}
  \item{F2}{Variables, listed by ordinal position, that define Factor 2.}
  \item{F3}{Variables, listed by ordinal position, that define Factor 3.}
  \item{F4}{Variables, listed by ordinal position, that define Factor 4.}
  \item{F5}{Variables, listed by ordinal position, that define Factor 5.}
  \item{F6}{Variables, listed by ordinal position, that define Factor 6.}
  \item{F7}{Variables, listed by ordinal position, that define Factor 7.}
  \item{F8}{Variables, listed by ordinal position, that define Factor 8.}
  \item{F9}{Variables, listed by ordinal position, that define Factor 9.}
  \item{F10}{Variables, listed by ordinal position, that define Factor 10.}
  \item{F11}{Variables, listed by ordinal position, that define Factor 11.}
  \item{F12}{Variables, listed by ordinal position, that define Factor 12.}
  \item{\dots}{Parameter values.}
}

\details{
OVERVIEW\cr
A multiple indicator measurement model defines one or more latent variables, called factors, in terms of mutually exclusive sets of indicator variables, such as items from a questionnaire or survey.  That is, each factor is defined by a unique set or group of indicators, and each indicator only contributes to the definition of one factor. Two sets of parameters are estimated by the model, the factor pattern coefficients, the lambda's, and the factor-factor correlations, the phi's. Also estimated here are the correlations of each indicator with the other factors. 

INPUT\cr
Unless \code{labels="only")}, the analysis requires the correlation matrix of the indicators and the specification of the groups of indicators, each of which defines a factor in the multiple indicator measurement model.  The default name for the indicator correlation matrix is \code{mycor}, which is also the default name of the matrix produced by the \code{lessR} function \code{\link{Correlation}} that computes the correlations from the data, as well as the name of the matrix read by the \code{lessR} function \code{\link{corRead}} that reads the already computed correlation matrix from an external file.

The data frame from which the correlation matrix was computed is requried only if any associated variable labels are listed, organized by the items within each factor. By default, \code{labels="include"}, these labels are listed as part of the analysis if they are available.

Define the constituent variables, the indicators, of each factor with a listing of each variable by its name in the correlation matrix. Each of the up to 12 factors is named F1, F2, etc. If the specified variables of a factor are in consecutive order in the input correlation matrix, the list can be specified by listing the first variable, a colon, and then the last variable.  To specify multiple variables, a single variable or a list, separate each by a comma, then invoke the R combine or \code{\link{c}} function, preceded by the factor's name and an equals sign. For example, if the first factor is defined by variables in the input correlation matrix from m02 through m05, and the variable Anxiety, then define the factor in the \code{corCFA} function call according to \code{F1=c(m02:m05,Anxiety)}. 

OUTPUT\cr
The result of the analysis is the correlation matrix of the indicator variables and resulting factors, plus the reliability analysis of the observed total scores or scale that corresponds to each factor.  Each scale is defined as an unweighted composite. The corresponding code to analyze the model with the \code{cfa} function from the \code{lavaan} package is also provided with the default maximum likelihood estimation procedure. The comparable \code{lavaan} solution appears in the column that represents the fully standardized solution, factors and indicators, \code{Std.all}, the last column of the solution output. 

VARIABLE LABELS\cr
To display the indicator content, first read the indicators as variable labels with the \code{lessR} function \code{\link{Read}}.  If this labels data frame exists, then the corresponding variable labels, such as the actual items on a survey, are listed by factor. For more information, see \code{\link{Read}}.

HEAT MAP\cr
To help visualize the overall pattering of the correlations, the corresponding heat map of the item correlation matrix with communalities is produced when \code{heat.map=TRUE}, the default. As is true of the output correlation matrix, the correlations illustrated in the heat map are also sorted by their ordering within each factor. The corresponding color scheme is dictated by the system setting, according to the \code{lessR} function \code{\link{set}}.  The default color scheme is \code{blue}.

ESTIMATION PROCEDURE\cr
The estimation procedure is centroid factor analysis, which defines each factor, parallel to the definition of each scale score, as the unweighted composite of the corresponding items for that scale.  The latent variables are obtained by replacing the 1's in the diagonal of the indicator variable correlation matrix with communality estimates.  These estimates are obtained by iterating the solution to the specified number of iterations according to \code{iter}, which defaults to 50.

A communality is the percentage of the item's correlation attributable to, in this situation of a multiple indicator measurement model, its one underlying factor.  As such, the communality is comparable to the item correlations for items within the same factor, which are also due only to the influence of the one common, underlying factor. A value of 0 for \code{iter} implies that the 1's remain in the observed variable correlation matrix, which then means that there are no latent factors defined.  Instead the resulting correlation matrix is of the observed scale scores and the component items.
}

\value{
Returns a list of six components.\cr
1. \code{ff.cor}: matrix of the factor correlations\cr
2. \code{if.cor}: matrix of the indicator-factor correlations that includes the estimated pattern coefficients of the model that link a factor to its indicators\cr
3. \code{diag.cor}: the indicator communalities\cr
4. \code{alpha}: coefficient alpha for each set of indicators\cr
5. \code{omega}: if a factor analysis with communality estimates (\code{iter} > 0), contains coefficient omega for each set of indicators\cr
6. \code{resid}: matrix of raw indicator residuals defined as the observed correlation minus that predicted by the model and its estimates 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Gerbing, D. W. (2013). R Data Analysis without Programming, Chapter 11, NY: Routledge.

Gerbing, D. W., & Hamilton, J. G. (1994). The surprising viability of a simple alternate estimation procedure for the construction of large-scale structural equation measurement models. Structural Equation Modeling: A Multidisciplinary Journal, 1, 103-115.

Hunter, J. E., Gerbing, D. W., & Boster, F. J. (1982). Machiavellian beliefs and personality: The construct invalidity of the Machiavellian dimension. Journal of Personality and Social Psychology, 43, 1293-1305.

Hunter, J. and Cohen, J. (1969). PACKAGE: A system of computer routines for the analysis of correlational data. Educational and Psychological Measurement, 1969, 29, 697-700.

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
# two ways to specify the items, with a colon and with commas
corCFA(F1=X1:X3, F2=c(X4,X5,X6))

# abbreviated form, one factor definition per line
cfa(
  F1=X1:X3,
  F2=X4:X6
)

# component analysis, show observed scale correlations
scales(F1=X1:X3, F2=X4:X6)

# produce a gray scale heat map of the item correlations
#   with communalities in the diagonal
# all subsequent graphics are in gray scale until changed
set(colors="gray")
corCFA(F1=X1:X3, F2=X4:X6)

# access the solution directly by saving to an object called fit
fit <- corCFA(F1=X1:X3, F2=c(X4,X5,X6))
fit
# get the pattern coefficients from the communalties
lambda <- sqrt(fit$diag.cor)
lambda

# access the lessR data set called datMach4, with variable labels
mydata <- Read("Mach4", format="lessR", quiet=TRUE)
# reverse score the reflected variables
mydata <- Recode(c(m03,m04,m06,m07,m09,m10,m11,m14,m16,m17,m19),
                 old=0:5, new=5:0, quiet=TRUE)
# calculate the correlations and store in mycor
mycor <- cr(m01:m20)
# confirmatory factor analysis of 4-factor solution of Mach IV scale
# Hunter, Gerbing and Boster (1982)
corCFA(F1=c(m06,m07,m09,m10), F2=c(m15,m02),
       F3=c(m04,m11,m16), F4=c(m01,m05,m12,m13))

# bad fitting model to illustrate indicator diagnostics
mycor <- corReflect(vars=c(m20))
corCFA(F1=c(m06,m09,m19), F2=c(m07),
       F3=c(m04,m11,m16), F4=c(m20,m18,m12,m15))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ factor analysis }


\name{corEFA}
\alias{corEFA}
\alias{efa}

\title{Exploratory Factor Analysis and Multiple Indicator Measurement Model}

\description{
Abbreviation: \code{efa}

A maximum likelihood exploratory factor analysis of an input correlation matrix, provided by the standard R exploratory factor analysis \code{\link{factanal}}, which requires the specified number of factors as an input to the analysis. Then constructs the code to run the corresponding multiple indicator measurement model (MIMM) suggested by the exploratory factor analysis loadings in terms of both the \code{lessR} \code{\link{corCFA}} and the \code{cfa} function from the \code{lavaan} package.
}

\usage{
corEFA(R=mycor, n_factors, rotate=c("promax", "varimax", "none"), 
          min_loading=.2, sort=TRUE, Rmd=NULL, \dots)

efa(\dots)
}

\arguments{
  \item{R}{Correlation matrix.}
  \item{n_factors}{Number of factors.}
  \item{rotate}{Rotation method, if any. Choices are promax (obique) or
       varimax (orthogonal.}
  \item{min_loading}{Minimum loading to include in suggested factor for
        confirmatory analysis and for the display of the loadings for the
        exploratory analysis. To ignore, set to 0.}
  \item{sort}{Sort the input variables by their highest factor loadings (but only
        first just list those items with loadings larger than 0.5).}
  \item{Rmd}{File name for the file of R markdown to be written,
        if specified. The file type is .Rmd, which automatically opens in RStudio,
        but it is a simple text file that can be edited with any text editor,
        including RStudio.}
  \item{\dots}{Parameter values_}
}

\details{
Only the loadings from the exploratory factor analysis are provided, with either an oblique (promax), by default, or an orthogonal (varimax) rotation.  If more information is desired, run \code{\link{factanal}} directly.

Also provides the associated multiple indicator measurement model suggested by the exploratory factor analysis. Each MIMM factor is defined by the items that have the highest loading on the corresponding exploratory factor.

For versions of \code{lessR} after 3.3, the correlation matrix computed by \code{\link{Correlation}} is now a list element called \code{R} within the returned list. For example, \code{mycor$R} from \code{mycor <- cr(d)}. The function \code{corEFA} automatically finds this correlation matrix from just entering the entire list name of the returned list, \code{mycor}, or the specific location, \code{mycor$R}, or as a stand-alone numerical matrix as done in versions of \code{lessR} previous to 3.3. 
}

\value{
The output can optionally be returned and saved into an \code{R} object, otherwise it simply appears at the console. The components of this object are redesigned in \code{lessR} version 3.3 into three different types: pieces of text that form the readable output, a variety of statistics, and R markdown instructions. The readable output are character strings such as tables amenable for viewing and interpretation. The statistics are numerical values amenable for further analysis, such as to be referenced in a subsequent R markdown document. The R~Markdown input is available for entry direct into \code{knitr}, such as in RStudio. The motivation of these three types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object followed by a \$, can be inserted into the R markdown document (see \code{examples}).

READABLE OUTPUT\cr
\code{out_title}: Variables in the model, rows of data and retained\cr
\code{out_loadings}: Estimated coefficients, hypothesis tests and confidence intervals\cr
\code{out_sum_squares}: Fit indices\cr
\code{out_cfa_title}: Analysis of variance\cr
\code{out_ice}: Correlations among all variables in the model\cr
\code{out_lavaan}: Collinearity analysis\cr
\code{out_deleted}: R squared adjusted for all (or many) possible subsets\cr

STATISTICS\cr
\code{Rmd}: Instructions to run through \code{knitr}, such as copy and paste, to obtain output in the form of a web file, pdf document or Word document. Can also obtain these instructions with the \code{Rmd} option, which writes them directly to the specified text file. Obtain a less detailed \code{Rmd} file by setting \code{explain=FALSE}. 

Although not typically needed for analysis, if the output is assigned to an object named, for example, \code{fa}, then the complete contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(fa)}. Invoking the \code{\link{class}} function on the saved object reveals a class of {out_all}. The class of each of the text pieces of output is {out}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\references{
Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 11, NY: Routledge.

Yves Rosseel (2012). lavaan: An R Package for Structural Equation
Modeling. Journal of Statistical Software, 48(2), 1-36. URL
http://www.jstatsoft.org/v48/i02/.
}

\seealso{
\code{\link{Correlation}}.
}

\examples{
# input correlation matrix of perfect two-factor model
# Factor Pattern for each Factor: 0.8, 0.6, 0.4
# Factor-Factor correlation: 0.3
mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("X1", "X2", "X3", "X4", "X5", "X6")
rownames(mycor) <- colnames(mycor) 

# default factor analysis of default correlation matrix mycor
#   with two factors extracted
corEFA(n_factors=2)

# abbreviated form
# use all items to construct the MIMM, regardless of their loadings
#  and show all loadings
# show the initial factor extraction
efa(n_factors=2, min_loading=0, show_initial=TRUE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }
\keyword{ factor analysis }



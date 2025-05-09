\name{Correlation}
\alias{Correlation}
\alias{cr}
\alias{cr_brief}

\title{Correlation Analysis}

\description{
Abbreviation: \code{cr}, \code{cr_brief}

For two variables, yields the correlation coefficient with hypothesis test and confidence interval. For a data frame or subset of variables from a data frame, yields the correlation matrix. The default computed coefficient(s) are the standard Pearson's product-moment correlation, with Spearman and Kendall coefficients available. For the default missing data technique of pairwise deletion, an analysis of missing data for each computed correlation coefficient is provided. For a correlation matrix a statistical summary of the missing data across all cells is provided.
}

\usage{
Correlation(x, y, data=d,
         miss=c("pairwise", "listwise", "everything"),
         show=c("cor", "missing"),
         fill_low=NULL, fill_hi=NULL,
         brief=FALSE, digits_d=NULL, heat_map=TRUE,
         main=NULL, bottom=3, right=3, quiet=getOption("quiet"),
         pdf_file=NULL, width=5, height=5, \dots)

cr_brief(\dots, brief=TRUE) 

cr(\dots) 
}


\arguments{
  \item{x}{First variable, or list of variables for a correlation matrix.}
  \item{y}{Second variable or not specified if the first argument is a list.}
  \item{data}{Optional data frame that contains the variables of interest,
       default is \code{d}.}
  \item{miss}{Basis for deleting missing data values.}
  \item{show}{Default is to compute and show correlations, or specify to 
       compute and show missing data by setting to \code{"missing"}.}
  \item{fill_low}{Starting color for a custom sequential palette.}
  \item{fill_hi}{Ending color for a custom sequential palette.}
  \item{brief}{Pertains to a single correlation coefficient analysis. If 
       \code{FALSE}, then the sample covariance and number of non-missing
       and missing observations are displayed.}
  \item{digits_d}{Specifies the number of decimal digits to display in the output.}
  \item{heat_map}{If \code{TRUE}, generate a heat map.}
  \item{main}{Graph title of heat map. Set to \code{main=""} to turn off.}
  \item{bottom}{Number of lines of bottom margin of heat map.}
  \item{right}{Number of lines of right margin of heat map.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
        with \code{\link{style}} function.}
  \item{pdf_file}{Indicate to direct pdf graphics to the specified name of
        the pdf file.}
  \item{width}{Width of the pdf file in inches.}
  \item{height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for internally called functions, which
       include \code{method="spearman"} and \code{method="kendall"} and also
       \code{alternative="less"} and \code{alternative="more"}.}
}


\details{
When two variables are specified, both \code{x} and y, the output is the correlation coefficient with hypothesis test, for a null hypothesis of 0, and confidence interval. Also displays the sample covariance. Based on R functions \code{\link{cor}}, \code{\link{cor.test}}, \code{\link{cov}}.

In place of two variables \code{x} and \code{y}, \code{x} can be a complete data frame, either specified with the name of a data frame, or blank to rely upon the default data frame \code{d}. Or, \code{x} can be a list of variables from the input data frame. In these situations \code{y} is missing. Any non-numeric variables in the data frame or specified variable list are automatically deleted from the analysis. 

When \code{heat_map=TRUE}, generate a heat map to standard graphics windows. Set \code{pdf=TRUE} to generate these graphics but have them directed to their respective pdf files.

For treating missing data, the default is \code{pairwise}, which means that an observation is deleted only for the computation of a specific correlation coefficient if one or both variables are missing the value for the relevant variable(s).  For \code{listwise} deletion, the entire observation is deleted from the analysis if any of its data values are missing.  For the more extreme \code{everything} option, any missing data values for a variable result in all correlations for that variable reported as missing.
}

\value{
From versions of \code{lessR} of 3.3 and earlier, if a correlation matrix is computed, the matrix is returned. Now more values are returned, so the matrix is embedded in a list of returned elements.

READABLE OUTPUT\cr

single coefficient\cr
\code{out_background}: Variables in the model, any variable labels\cr
\code{out_describe}: Estimated coefficients\cr
\code{out_inference}: Hypothesis test and confidence interval estimated coefficient\cr

matrix\cr
\code{out_cor}: Correlations or \code{out_missing}: Missing values analysis\cr


STATISTICS\cr

single coefficient\cr
\code{r}: Model formula that specifies the model\cr
\code{tvalue}: t-statistic of estimated value of null hypothesis of no relationship\cr
\code{df}: Degrees of freedom of hypothesis test 
\code{pvalue}: Number of rows of data submitted for analysis\cr
\code{lb}: Lower bound of confidence interval\cr
\code{ub}: Upper bound of confidence interval\cr

matrix\cr
\code{R}: Correlations\cr
}

\references{
Gerbing, D. W. (2023). \emph{R Data Analysis without Programming: Explanation and Interpretation}, 2nd edition, Chapter 10, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{cor.test}}, \code{\link{cov}}.
}

\examples{
# data
n <- 12
f <- sample(c("Group1","Group2"), size=n, replace=TRUE)
x1 <- round(rnorm(n=n, mean=50, sd=10), 2)
x2 <- round(rnorm(n=n, mean=50, sd=10), 2)
x3 <- round(rnorm(n=n, mean=50, sd=10), 2)
x4 <- round(rnorm(n=n, mean=50, sd=10), 2)
d <- data.frame(f,x1, x2, x3, x4)
rm(f); rm(x1); rm(x2); rm(x3); rm(x4)

# correlation and covariance
Correlation(x1, x2)
# short name
cr(x1, x2)
# brief form of output
cr_brief(x1, x2)

# Spearman rank correlation, one-sided test
Correlation(x1, x2, method="spearman", alternative="less")

# correlation matrix of the numerical variables in R
R <- Correlation()

# correlation matrix of Kendall's tau coefficients
R <- cr(method="kendall")

# analysis with data not from data frame R
data(attitude)
R <- Correlation(rating, learning, data=attitude)

# analysis of entire data frame that is not R
data(attitude)
R <- Correlation(attitude)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ write }
\keyword{ csv }



\name{ANOVA}
\alias{ANOVA}
\alias{av}
\alias{av.brief}

\title{Analysis of Variance}

\description{
Abbreviation: \code{av}, \code{av.brief}

Analysis of variance from the R \code{\link{aov}} function plus graphics and effect sizes. Permitted designs are one-way between groups, two-way between groups and randomized blocks with one treatment factor with one observation for each treatment and block combination. 
}

\usage{
ANOVA(my.formula, data=mydata, brief=getOption("brief"), digits.d=NULL, 
         rb.points=TRUE, res.rows=NULL, res.sort=c("zresid", "fitted", "off"),
         pdf=FALSE, pdf.width=5, pdf.height=5, \ldots)

av(\ldots)

av.brief(\ldots, brief=TRUE) 
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.}
  \item{data}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{brief}{If set to \code{TRUE}, reduced text output. Can change system default
       with \code{\link{set}} function.}
  \item{digits.d}{For the Basic Analysis, it provides the number of decimal digits.
        For the rest of the output, it is a suggestion only.}
  \item{rb.points}{For a randomized block design, a plot of the fitted value
        for each cell is obtained as well as the individual data values. Set to
        \code{FALSE} to suppress the data values.}
  \item{res.rows}{Default is 20, which lists the first 20 rows of data and residuals
       sorted by the specified sort criterion.  To disable residuals, specify a 
       value of 0. To see the residuals output for all observations, specify a 
       value of \code{"all"}.}
  \item{res.sort}{Default is \code{"zresid"}, for specifying standardized residuals
       as the sort criterion for the display of the rows of data and associated
       residuals. Other values are \code{"fitted"} for the fitted values and
      \code{"off"} to not sort the rows of data.}
  \item{pdf}{Indicator as to if the graphic files should be saved as pdf files
       instead of directed to the standard graphics windows.}
  \item{pdf.width}{Width of the pdf file in inches.}
  \item{pdf.height}{Height of the pdf file in inches.}
  \item{\dots}{Other parameter values for R function \code{\link{lm}} which provides the core computations.} 
}


\details{
OVERVIEW\cr
The one-way ANOVA with Tukey HSD and plot based on the R functions \code{\link{aov}}, \code{\link{TukeyHSD}}, and provides summary statistics for each level. Two-factor ANOVA also provides an interaction plot of the means with \code{\link{interaction.plot}} as well as a table of means and other summary statistics. The two-factor analysis can be between groups or a randomized blocked design. Residuals are displayed by default. Tukey HSD comparisons and residuals are not displayed if \code{brief=TRUE}.

MODEL SPECIFICATION\cr
In the following specifications, Y is the response variable, X is a treatment variable and Blocks is the blocking variable. The distinction between the one-way randomized blocks and the two-way between groups models is not the variable names, but rather the delimiter between the variable names. Use \code{*} to indicate a two-way crossed between groups design and \code{+} for a randomized blocks design.\cr
one-way between groups: \code{ANOVA(Y ~ X)}\cr
one-way randomized blocks: \code{ANOVA(Y ~ X + Blocks)}\cr
two-way between groups: \code{ANOVA(Y ~ X1 * X2)}\cr
For more complex designs, use the standard R function \code{\link{aov}} upon which \code{ANOVA} depends.

BALANCED DESIGN\cr
The design for the two-factor analyses must be balanced. A check is performed and processing ceases if not balanced. For unbalanced designs, consider the function  \code{lmer} in the \code{lme4} package.

DECIMAL DIGITS\cr
The number of decimal digits displayed on the output is, by default, the maximum number of decimal digits for all the data values of the response variable. Or, this value can be explicitly specified with the \code{digits.d} parameter.
}

\value{
Following the standard \code{R} function \code{\link{aov}}, invisibly returns an object of \code{\link{class}} c("aov", "lm").
}

\references{
Gerbing, D. W. (2013). R Data Analysis without Programming, Chapter 7, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{aov}}, \code{\link{TukeyHSD}}, \code{\link{interaction.plot}}
}

\examples{
# create simulated data, no population mean difference
# X has two values only, Y is numeric
# put into a data frame
n <- 12
X <- sample(c("Group1","Group2","Group3"), size=n, replace=TRUE)
Y <- round(rnorm(n=n, mean=50, sd=10), 2)
mydata <- data.frame(X,Y)

# analyze data with formula version
# variable names and levels of X are automatically obtained from data
# although data frame not attached, reference variable names directly
ANOVA(Y ~ X)
# short name
av(Y ~ X)

# variable of interest is in a data frame which is not the default mydata
# access the data frame in the lessR dat.Learn data set
# although data not attached, access the variables directly by their name
data(dataLearn)
ANOVA(Score ~ StudyType, data=dataLearn)

# two-factor between-groups ANOVA with replications and interaction
ANOVA(breaks ~ wool * tension, data=warpbreaks)

# randomized blocks design with the second term the blocking factor
ANOVA(breaks ~ wool + tension, data=warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }





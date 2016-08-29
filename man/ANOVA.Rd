\name{ANOVA}
\alias{ANOVA}
\alias{av}
\alias{av.brief}

\title{Analysis of Variance}

\description{
Abbreviation: \code{av}, \code{av.brief}

Analysis of variance from the R \code{\link{aov}} function plus graphics and effect sizes. Permitted designs are one-way between groups, two-way between groups and randomized blocks with one treatment factor with one observation for each treatment and block combination. 

Output is generated into distinct segments by topic, organized and displayed in sequence by default. When the output is assigned to an object, such as \code{a} in \code{a <- reg(Y ~ X)}, the full or partial output can be accessed for later analysis and/or viewing. A primary such analysis is with \code{knitr} for dynamic report generation, run from R directly or from within \code{RStudio}. The input instructions to \code{knitr} are written comments and interpretation with embedded \code{R} code, called R~Markdown. Doing a \code{knitr} analysis is to "knit" these comments and subsequent output together so that the \code{R} output is embedded in the resulting document, either html, pdf or Word, by default with explanation and interpretation. Generate a complete, though preliminary at this time, R Markdown document from the \code{Rmd} option ready to knit. Simply specify the option with a file name, run the ANOVA function to create the file. Then open the newly created \code{.Rmd} file in \code{RStudio} and click the \code{knit} button to create a formatted document that consists of the statistical results and interpretative comments. See the sections \code{arguments}, \code{value} and \code{examples} for more information. 
}

\usage{
ANOVA(my.formula, data=mydata, brief=getOption("brief"), digits.d=NULL, 
         Rmd=NULL, graphics=TRUE,
         rb.points=TRUE, res.rows=NULL, res.sort=c("zresid", "fitted", "off"),
         pdf=FALSE, pdf.width=5, pdf.height=5, fun.call, \ldots)

av(\ldots)

av.brief(\ldots, brief=TRUE) 
}


\arguments{
  \item{my.formula}{Standard R \code{\link{formula}} for specifying a model.}
  \item{data}{The default name of the data frame that contains the data for analysis 
       is \code{mydata}, otherwise explicitly specify.}
  \item{brief}{If set to \code{TRUE}, reduced text output with no Tukey multiple 
       comparison of means and no residuals. Can change system default
       with \code{\link{theme}} function.}
  \item{digits.d}{For the Basic Analysis, it provides the number of decimal digits.
        For the rest of the output, it is a suggestion only.}
  \item{Rmd}{File name for the file of R Markdown instructions to be written,
        if specified. The file type is .Rmd, which automatically opens in RStudio,
        but it is a simple text file that can be edited with any text editor,
        including RStudio.}
  \item{graphics}{Produce graphics. Default is \code{TRUE}.  In \code{Rmd} can
        be useful to set to \code{FALSE} so that \code{\link{regPlot}} can be used
        to place the graphics within the output file.}
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
  \item{fun.call}{Function call. Used with \code{Rmd} to pass the function call when
        obtained from the abbreviated function call \code{av}.}
  \item{\dots}{Other parameter values for R function \code{\link{lm}} which provides the core computations.} 
}


\details{
OVERVIEW\cr
The one-way ANOVA with Tukey HSD and corresponding plot is based on the R functions \code{\link{aov}}, \code{\link{TukeyHSD}}, and provides summary statistics for each level. Two-factor ANOVA also provides an interaction plot of the means with \code{\link{interaction.plot}} as well as a table of means and other summary statistics. The two-factor analysis can be between groups or a randomized blocked design. Residuals are displayed by default. Tukey HSD comparisons and residuals are not displayed if \code{brief=TRUE}.

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
The output can optionally be returned and saved into an \code{R} object, otherwise it simply appears at the console. The components of this object are redesigned in \code{lessR} version 3.3.5 into (a) pieces of text that form the readable output and (b) a variety of statistics. The readable output are character strings such as tables amenable for viewing and interpretation. The statistics are numerical values amenable for further analysis, such as to be referenced in a subsequent R Markdown document. The motivation of these two types of output is to facilitate R markdown documents, as the name of each piece, preceded by the name of the saved object followed by a $, can be inserted into the R markdown document (see \code{examples}).

TEXT OUTPUT\cr
\code{out_background}: variables in the model, rows of data and retained\cr
1-predictor: \code{out_descriptive}: decriptive stats\cr
2-predictors: \code{out_cell.n}: cell sample size\cr
2-predictors: \code{out_cell.means}: cell means\cr
2-predictors: \code{out_cell.marginals}: marginal means\cr
2-predictors: \code{out_cell.gm}: grand mean\cr
2-predictors: \code{out_cell.sd}: cell standard deviations\cr
\code{out_anova}: analysis of variance summary table\cr
\code{out_effects}: effect sizes\cr
\code{out_hsd}: Tukey's honestly significant different analysis\cr
\code{out_res}: residuals\cr
\code{out_plots}: list of plots generated if more than one\cr

Separated from the rest of the text output are the major headings, which can then be deleted from custom collations of the output.
\code{out_title_bck}: BACKGROUND\cr
\code{out_title_des}: DESCRIPTIVE STATISTICS\cr
\code{out_title_basic}: BASIC ANALYSIS\cr
\code{out_title_res}: RESIDUALS\cr

STATISTICS\cr
\code{call}: function call that generated the analysis\cr
\code{formula}: model formula that specifies the model\cr
\code{n.vars}: number of variables in the model\cr
\code{n.obs}: number of rows of data submitted for analysis\cr
\code{n.keep}: number of rows of data retained in the analysis\cr
\code{residuals}: residuals\cr
\code{fitted}: fitted values\cr

Although not typically needed for analysis, if the output is assigned to an object named, for example, \code{a}, then the complete contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(a)}. Invoking the \code{\link{class}} function on the saved object reveals a class of {out_all}. The class of each of the text pieces of output is {out_piece}.
}


\references{
Gerbing, D. W. (2013). R Data Analysis without Programming, Chapter 7, NY: Routledge.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{aov}}, \code{\link{TukeyHSD}}, \code{\link{interaction.plot}}
}

\examples{


# access the PlantGrowth data frame
ANOVA(weight ~ group, data=PlantGrowth)
#brief version
av.brief(weight ~ group, data=PlantGrowth)

# variables of interest in a data frame that is not the default mydata
# two-factor between-groups ANOVA with replications and interaction
# warpbreaks is a data set provided with R
ANOVA(breaks ~ wool * tension, data=warpbreaks)

# randomized blocks design with the second term the blocking factor
# use short name
av(breaks ~ wool + tension, data=warpbreaks)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }





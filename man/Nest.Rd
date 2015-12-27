\name{Nest}
\alias{Nest}
\alias{nt}

\title{Nest the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{nt}

A nested model has a subset of predictor variables from the corresponding full model. Compare a nested linear model with a full model to evaluate the effectiveness of the predictor variables deleted from the full model to define the nested model. 
}

\usage{
Nest(y, nested.model, full.model, method=c("ls", "logit"),
         data=mydata, digits.d=NULL)

nt(\ldots)
}

\arguments{
  \item{y}{Response variable.}
  \item{nested.model}{Predictor variables in the nested model.}
  \item{full.model}{Predictor variables in either the full model, or
        just those that added to the reduced model to derive the
        full model.}
  \item{method}{Do a least squares analysis, \code{ls}, the default, or 
        set to \code{logit}.}
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{mydata} by default.}
  \item{digits.d}{Number of decimal digits, set by default to at least 2
        or the largest number of digits in the values of the response variable
        plus 1.}
  \item{\dots}{The specified arguments.}
}

\details{
Use the standard \code{R} function \code{\link{anova}} function to compare a nested model with a corresponding full model. By default, compare models estimated with ordinary least squares from the \code{R} function \code{\link{lm}}, or compare models estimated with logistic regression from the \code{R} function \code{\link{glm}} with \code{family="binomial"}. For the logistic analysis, the \code{\link{anova}} analysis is with \code{test="Chisq"}.

To insure that the same data are analyzed for both models, the fit for the full model is first obtained. Then the data frame that is returned by this analysis is input into the analysis for the nested model. This guarantees that any cases with missing data values missing for the full analysis will have been deleted for the nested analysis.  Otherwise rows of data could be retained for the nested analysis that were dropped for the full analysis because of missing data values for the deleted predictor variables. This method also guarantees that cases are not deleted because data was missing on variables not included in full analysis. 
}

\value{
The output can optionally be returned and saved into an \code{R} object, otherwise it simply appears at the console. The components of this object are redesigned in \code{lessR} version 3.3 into (a) pieces of text that form the readable output and (b) a variety of statistics. The readable output are character strings such as tables amenable for viewing and interpretation. The statistics are numerical values amenable for further analysis, such as to be referenced in a subsequent \code{knitr} document. The motivation of these three types of output is to facilitate \code{knitr} documents, as the name of each piece, preceded by the name of the saved object followed by a \$, can be inserted into the \code{knitr} document (see \code{examples}).

TEXT OUTPUT\cr
\code{out_models}: The specification of the two models compared\cr
\code{out_anova}: Analysis of variance or, for logit, analysis of deviance\cr

STATISTICS\cr
\code{fun.call}: Function call that generated the analysis\cr
\code{anova_tested}: Term that is tested\cr
\code{anova_residual}: Residual df, and either ss and ms or deviance for logit\cr
\code{anova_total}: For logit, total df and deviance\cr

Although not typically needed for analysis, if the output is assigned to an object named, for example, \code{n}, then the complete contents of the object can be viewed directly with the \code{\link{unclass}} function, here as \code{unclass(n)}. Invoking the \code{\link{class}} function on the saved object reveals a class of {out_all}. The class of each of the text pieces of output is {out_piece}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{anova}}, \code{\link{lm}}, \code{\link{glm}}.
}

\examples{

mydata <- Read("Reading", format="lessR")

# compare least-squares models
# can specify all the variables in the full model
Nest(Reading, c(Absent), c(Verbal,Absent,Income))
# or, can specify just the additional variables in the full model
Nest(Reading, c(Absent), c(Verbal,Income))

# compare logistic models, save results into an object
# define the full model by adding just the variables
#  not found in the reduced model
mydata <- Read("BodyMeas", format="lessR")
n <- Nest(Gender, c(Weight, Hips, Hand, Shoe),
      c(Height, Waist, Chest), method="logit")
# view the results
n
# see the names of the available output components
names(n)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nested models }









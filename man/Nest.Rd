\name{Nest}
\alias{Nest}
\alias{nt}

\title{Nest the Values of an Integer or Factor Variable}

\description{
Abbreviation: \code{nt}

A nested model has a subset of predictor variables from the corresponding full model. Compare a nested linear model with a full model to evaluate the effectiveness of the predictor variables deleted from the full model to define the nested model. 
}

\usage{
Nest(y, nested.model, full.model, method=c("ls", "logit"), data=mydata)

nt(\ldots)
}

\arguments{
  \item{y}{Response variable.}
  \item{nested.model}{Predictor variables in the nested model.}
  \item{full.model}{Predictor variables in the full model.}
  \item{method}{Do a least squares analysis, the default, or a logit analysis.}
  \item{data}{The name of the data frame from which to create the subset, which
       is \code{mydata} by default.}
  \item{\dots}{The specified arguments.}
}

\details{
Use the standard \code{R} function \code{\link{anova}} function to compare a nested model with a corresponding full model. By default, compare models estimated with ordinary least squares from the \code{R} function \code{\link{lm}}, or compare models estimated with logistic regression from the \code{R} function \code{\link{glm}} with \code{family="binomial"}. For the logistic analysis, the \code{\link{anova}} analysis is with \code{test="Chisq"}.

To insure that the same data are analyzed for both models, the fit for the full model is first obtained. Then the data frame that is returned by this analysis is input into the analysis for the nested model. This guarantees that any cases with missing data values missing for the full analysis will have been deleted for the nested analysis.  Otherwise rows of data could be retained for the nested analysis that were dropped for the full analysis because of missing data values for the deleted predictor variables. This method also guarantees that cases are not deleted because data was missing on variables not included in full analysis. 
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{anova}}, \code{\link{lm}}, \code{\link{glm}}.
}

\examples{

mydata <- Read("Reading", format="lessR")


# compare least-squares models
mydata <- Read("Reading", format="lessR")
Nest(Reading, c(Absent), c(Verbal,Absent,Income))

# compare logistic models
mydata <- Read("BodyMeas", format="lessR")
Nest(Gender, c(Weight, Hips, Hand, Shoe),
      c(Height, Weight, Waist, Hips, Chest, Hand, Shoe), method="logit")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nested models }









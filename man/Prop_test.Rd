\name{Prop_test}
\alias{Prop_test}
\alias{prop}

\title{Analysis of Prop_test}

\description{
Abbreviation: \code{prop}

Analyze proportions, either of a single proportion against a fixed alternative, a set of proportions evaluated for equality, or a goodness-of-fit test for a single categorical variable or a test of independence for multiple variables.
}

\usage{
Prop_test(variable=NULL, success=NULL, by=NULL, data=d,
          n_succ=NULL, n_fail=NULL, n_tot=NULL, n_table=NULL,
          Yates=FALSE, pi=NULL, digits_d=3, \dots)

prop(\dots)
}

\arguments{
  \item{variable}{Numerical variable to analyze.}
  \item{success}{Value of \code{variable} considered a success.}
  \item{by}{Compare proportions over groups, the values of this
        categorical variable.}
  \item{data}{Data frame that contains the \code{variable} to analyze.}
  \item{n_succ}{Number of successes.}
  \item{n_fail}{Number of trials, either provide this or \code{n}.}
  \item{n_tot}{Number of trials, either provide this or \code{q}.}
  \item{n_table}{Path name of the file that contains a frequency table.}
  \item{Yates}{Set to \code{TRUE} to implement Yate's correction factor
        where applicable.}
  \item{pi}{Value of null hypothesized probability.}
  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{\dots}{Parameter values passed to \code{Prop_test}.} 
}


\details{
The analysis of proportions is of two primary types.

For one or more samples of data, focus on a single value of a categorical variable, traditionally called a success. Analyze the resulting proportion of occurrence for a single sample or compare proportions of occurrence of a success across distinct samples of data, what is called a test of homogeneity.

For a single sample, compare proportions from a contingency table. These tests are called a goodness-of-fit test for a single variable and a test of independence for multiple variables.

From standard base R functions, the lessR function \code{Prop_test()}, abbreviated \code{prop()}, provides for either type of the analysis for proportions. To use, enter either the original data from which the sample proportions are computed, or directly enter already computed sample frequencies from which the proportions are computed.

TEST OF HOMOGENEITY\cr
When analyzing the original data, an entered value for the parameter \code{success} for the categorical variable of interest, indicated by parameter \code{variable}, triggers the test of homogeneity. For a single proportion the analysis is the exact binomial test. If the proportions are entered directly, indicate the number of successes and the total number of trials with the \code{n_succ} and \code{n_tot} parameters, each as a single value for a single sample or as vectors of multiple values for multiple samples. 

TEST OF UNIFORM GOODNESS-OF-FIT\cr
To test for goodness-of-fit from the original data, just enter the name of the categorical variable. To test from the proportions, specify the proportions as a vector with the \code{n_tot} parameter.

TEST OF INDEPENDENCE\cr
Without a value for \code{success} or \code{n_succ} the analysis is of goodness-of-fit or independence. For the test of independence, to enter the joint frequency table directly, store the frequencies in a file accessible from your computer system. One possibility is to enter the numbers into a text file with file type `.csv` or `.txt`. Enter the numbers with a text editor, or with a word processor saving the file as a text file. With this file format, separate the adjacent values in each row with a comma, as indicated below. Or, enter the numbers into an MS Excel formatted file with file type `.xlsx`. Enter only the numeric frequencies, no labels. Use the parameter \code{n_table} to indicate the path name to the file, enclosed in quotes. Or, leave the quotes empty to browse for the joint frequency table. 

To conduct the test from the data, enter the names of the two categorical variables. The variable listed first is the parameter `variable`. The second listed variable is for the parameter `by`, the name of which must be included in the function call.

See the corresponding vignette for more detail and examples.\cr
Enter \code{browseVignettes("lessR")}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}


\seealso{
\code{\link{binom.test}}.
}

\examples{

# generate data
Classvalues <- c("Freshman", "Sophomore", "Junior", "Senior")
Goodvalues <- c("Nice", "OK", "Mean")
Class <- sample(Classvalues, size=250, replace=TRUE)
Goodness <- sample(Goodvalues, size=250, replace=TRUE)
d <- data.frame(Class, Goodness)

# Test a single proportion
Prop_test(variable=Goodness, success="Nice")

# Test multiple proportions, one each for each level of Plan
Prop_test(Goodness, "Nice", by=Class)

# Test of independence
Prop_test(Goodness, by=Class)

# Same example as for the base R binom.test
Prop_test(n_succ=682, n_fail=243, p=.75, digits_d=2)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ proportion }

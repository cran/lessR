\name{train_test}
\alias{train_test}

\title{Create Training and Testing Data}

\description{
Given a data frame, create a list of either two components, \code{train} and \code{test}, or four components, for training and testing data: \code{train_x}, \code{train_y}, \code{test_x}, and \code{test_y}.
}

\usage{
train_test(data, response=NULL, p_train=0.75, seed=NULL)
}

\arguments{
  \item{data}{Data frame that contains the variables.}
  \item{response}{Optional name of the response variable of the response values.}
  \item{p_train}{Percentage of the input data frame to be retained for training.}
  \item{seed}{Set to a usually odd value to reproduce results.}
}

\details{
From the input data frame create training and testing data frames. If the \code{response} is specified, create four component data frames with x and y variables separated. Otherwise create two component data frames, \code{train} and \code{test}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
d <- Read("Employee")

# create four component data frames: train_x, train_y, test_x, and test_y
out <- train_test(d, Salary)
names(out)

# create two component data frames: train and test
out <- train_test(d)
names(out)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ long-form }

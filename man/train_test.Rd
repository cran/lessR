\name{train_test}
\alias{train_test}

\title{Create Training and Testing Data}

\description{
Given a data frame, create a list of either two components, \code{train} and \code{test}, or four components, for training and testing data: \code{train_x}, \code{train_y}, \code{test_x}, and \code{test_y}.
}

\usage{
train_test(data, response=NULL, p_train=0.75, seed=NULL, matrix_out=FALSE)
}

\arguments{
  \item{data}{Data frame that contains the variables.}
  \item{response}{Optional name of the response variable of the response values.}
  \item{p_train}{Percentage of the input data frame to be retained for training.}
  \item{seed}{Set to a usually odd value to reproduce results.}
  \item{matrix_out}{If \code{TRUE} then output data structures as matrices
        instead of data frames.}
}

\details{
From the input data frame create training and testing data frames. If the \code{response} is specified, create four component data frames with x and y variables separated. Otherwise create two component data frames, \code{train} and \code{test}.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\examples{
d <- Read("Employee")

# create four component data frames that separate the response variable, y,
#   from predictor variables, X: train_x, train_y, test_x, and test_y
out <- train_test(d, response=Salary)
names(out)
# then can copy to regular data frames apart from the list output structure
X_train <- out$train_x
y_train <- out$train_y
X_test <- out$test_x
y_test <- out$test_y

# create two component data frames, train and test, which retain all
#   variables for the model in the same data frame
out <- train_test(d)
names(out)
# then can copy to regular data frames apart from the list output structure
d_train <- out$train
d_test <- out$test
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reshape }
\keyword{ wide-form }
\keyword{ long-form }

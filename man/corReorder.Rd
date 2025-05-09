\name{corReorder}
\alias{corReorder}
\alias{reord}

\title{Reorder Variables in a Correlation Matrix}

\description{
Abbreviation: \code{reord}

Re-arranges the order of the variables in the input correlation matrix.  If no variable list is specified then by default the variables are re-ordered according to hierarchical clustering. Or, re-order with the Hunter (1973) chain method in which the first variable is the variable with the largest sum of squared correlations of all the variables, then the next variable is that with the highest correlation with the first variable, and so forth. Or, re-order manually.
}

\usage{
corReorder(R=mycor, order=c("hclust", "chain", "manual", "as_is"),
          hclust_type = c("complete", "ward.D", "ward.D2", "single",
                          "average", "mcquitty", "median", "centroid"),
          dist_type=c("R", "dist"),
          n_clusters=NULL, vars=NULL, chain_first=0,
          heat_map=TRUE, dendrogram=TRUE, diagonal_new=TRUE,
          main=NULL, bottom=NULL, right=NULL, quiet=getOption("quiet"),
          pdf=FALSE, width=5, height=5, \ldots)

reord(\ldots)
}

\arguments{
  \item{R}{Correlation matrix.}
  \item{order}{Source of ordering (seriation): Default of hierarchical cluster
       analysis,
       Hunter(1973) chain method, manually specified with \code{vars}, or left
       \code{"as_is"}.}
  \item{hclust_type}{Type of hierarchical cluster analysis.}
  \item{dist_type}{Default is a correlation matrix of similarities, otherwise
       a distance matrix.}
  \item{n_clusters}{For a hierarchical cluster analysis, optionally specify
       the cluster
       membership for the specified number of clusters.}
  \item{vars}{List of the re-ordered variables, each variable listed by its ordinal
       position in the input correlation matrix. If this is set, then
       \code{order} set to \code{"manual"}.}
  \item{chain_first}{The first variable listed in the ordered matrix with
       the chain method.}
  \item{main}{Graph title. Set to \code{main=""} to turn off.}
  \item{heat_map}{If \code{TRUE}, display a heat map of the item correlations.}
  \item{dendrogram}{If \code{TRUE}, display a heat map of the item correlations
       for a hierarchical cluster analysis.}
  \item{diagonal_new}{If \code{TRUE}, replace diagonal for the heat map only
       with an average of the correlation of item on the diagonal with the two
       adjacent items.}
  \item{bottom}{Number of lines of bottom margin.}
  \item{right}{Number of lines of right margin.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
        with \code{\link{style}} function.}
  \item{pdf}{if \code{TRUE}, create possibly two PDF files: the hierarchical
       cluster analysis dendrogram if requested and the heat map of the reordered
       correlation matrix.}
  \item{width}{Width of a pdf file in inches.}
  \item{height}{Height of a pdf file in inches.}
  \item{\dots}{Parameter values_}
}

\details{
Reorder and/or delete variables in the input correlation matrix.  

Define the constituent variables, the items, with a listing of each variable by its name in the correlation matrix. If the specified variables are in consecutive order in the input correlation matrix, the list can be specified by listing the first variable, a colon, and then the last variable.  To specify multiple variables, a single variable or a list, separate each by a comma, then invoke the R combine or \code{\link{c}} function. For example, if the list of variables in the input correlation matrix is from m02 through m05, and the variable Anxiety, then define the list in the \code{corReorder} function call according to \code{vars=c(m02:m05,Anxiety)}. 

Or, define the ordering with a hierarchical cluster analysis from the base R function \code{hclust()}. The same default type of \code{"complete"} is provided, though this can be changed with the parameter \code{hclust_type} according to \code{\link{hclust}}. Default input is a correlation matrix, converted to a matrix of dissimilarities by subtracting each element from 1.

Or, use the Hunter (1973) chain method. Define the ordering of the variables according to the following algorithm. If no variable list is specified then the variables are re-ordered such that the first variable is that which has the largest sum of squared correlations of all the variables, then the variable that has the highest correlation with the first variable, and so forth.

In the absence of a variable list, the first variable in the re-ordered matrix can be specified with the \code{chain_first} option.

}


\references{
Hunter, J.E. (1973), Methods of reordering the correlation matrix to facilitate visual inspection and preliminary cluster analysis, Journal of Educational Measurement, 10, p51-61.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{Correlation}}, \code{\link{hclust}}.
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
colnames(mycor) <- c("V1", "V2", "V3", "V4", "V5", "V6")
rownames(mycor) <- colnames(mycor)

# leave only the 3 indicators of the second factor
#  in reverse order
#replace original mycor
mycor <- corReorder(vars=c(V6,V5,V4))

#  reorder according to results of a hierarchical cluster analysis
mynewcor <- corReorder()

#  get cluster membership for two clusters
# specify each parameter
mynewcor <- corReorder(mycor, order="hclust", n_clusters=2)

#  reorder with first variable with largest sums of squares
mynewcor <- corReorder(order="chain")

# reorder the variables according to the ordering algorithm
#  with the 4th variable listed first
# no heat map
mynewcor <- corReorder(chain_first=2, heat_map=FALSE)

mycor <- matrix(nrow=6, ncol=6, byrow=TRUE,
c(1.000,0.480,0.320,0.192,0.144,0.096,
  0.480,1.000,0.240,0.144,0.108,0.072,
  0.320,0.240,1.000,0.096,0.072,0.048,
  0.192,0.144,0.096,1.000,0.480,0.320,
  0.144,0.108,0.072,0.480,1.000,0.240,
  0.096,0.072,0.048,0.320,0.240,1.000))
colnames(mycor) <- c("V1", "V2", "V3", "V4", "V5", "V6")
rownames(mycor) <- colnames(mycor)

# can also re=order with index position of each variable
mycor <- corReorder(vars=c(4,5,6,1,2,3))

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation }





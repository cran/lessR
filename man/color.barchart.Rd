\name{color.barchart}
\alias{color.barchart}

\title{Barchart of One or Two Variables with Color}

\description{
Using the standard R functions \code{\link{barplot}} and, for two variables, \code{\link{legend}}, plots a bar chart of one or two variables with default colors, including background color and gridlines in addition to the bars. Unlike \code{\link{barplot}}, the variable(s) can be entered directly into the function call without first converting to a table.  If two variables are plotted, a legend is automatically provided.
}

\usage{
color.barchart(x, y=NULL, col.bars=NULL, border="black", col.bg="seashell", 
         col.grid="grey90", beside=FALSE, over.grid=FALSE, 
         legend.loc=NULL, legend.labels=NULL, vivid=FALSE, 
         random=FALSE, addtop=1, \ldots)
}

\arguments{
  \item{x}{Variable for which to construct the bar chart.}
  \item{y}{Option second variable.}
  \item{col.bars}{Specified bar colors.}
  \item{border}{Color of the border of the bars.}
  \item{col.bg}{Color of the plot background.}
  \item{col.grid}{Color of the grid lines.}
  \item{beside}{When plotting two variables, set to true for the levels fo the first 
        variable to be plotted as adjacent bars instead of stacked on each other.}
  \item{over.grid}{If true, plot the grid lines over the histogram.}
  \item{legend.loc}{When plotting two variables, location of the legend. Usually choose
        from among "topleft", "top", "topright", "right", or "center".}
  \item{legend.labels}{When plotting two variables, labels for the legend, which by default
        are the levels for the first variable.}
  \item{vivid}{Sets the intensity of the default color palette for the bars and the 
        background.  The default of \code{"FALSE"} sets more pastel colors.}
  \item{random}{Randomizes the order of the colors within the chosen color palette so
        that each run of the same function call generally yields different colors of the bars.}
  \item{addtop}{In the same scale as the vertical axis, puts more space at between the bars 
        and the top of the plot area, usually to accomodate the legend when plotting two 
        variables.}
  \item{\dots}{Other parameter values for graphics as defined processed 
      by \code{\link{hist}} and \code{\link{plot}}, including \code{xlim}, \code{ylim}, \code{lwd} and \code{cex.lab}, \code{col.main}, etc.}
}


\details{
Plot a bar chart with default colors for one or two variables, presumably with a relatively small number of values for each variable.  By default, colors are selected for the bars, background and gridlines, all of which can be overridden.

A frequency bar is produced for each level of the first and perhaps only variable. When two variables are plotted, the bar for each level of the first variable is replaced with a group of bars, one for each level of the second variable.

There are three ways to override the default colors.\cr
1. There are two color palettes, each with 7 colors.  The default palette provides lighter, more pastel colors.  The vivid palette, activated by the \code{vivid=TRUE} option, provides brighter colors with a brighter background (cornsilk1).\cr
2. The order of the colors within the chosen palette can be randomized, so that, for example, each of the seven colors in a palette has a 1/7 chance of appearing as the first color, the only color used in the plot of a single variable, and the color used for the first bar in each group for a plot of two variables.\cr
3. The desired colors can be specified with the \code{col.bg} option. When plotting one variable, include one color in the color list, the color used for all of the bars.  When plotting two variables, usually the list of colors includes the same number of elements as the number of levels for the second variable.  As always with R, if the list includes more than once color, the \code{c} function must be used to generate the list, as in \code{col.bars=c("coral3","seagreen3")}. When two variables are plotted, if there are fewer specified colors than the levels of the second variable, the remaining colors are selected from the  remaining colors in the specified palette.

The ordered colors in one of the two provided color palettes can be viewed by running the corresponding two lines of R code, first for the default colors and second for the vivid colors:\cr\cr
clr <- c("lightsteelblue", "moccasin", "salmon", "palegreen", "thistle", "turquoise", "yellow2")\cr
barplot(rep(1,7), names=clr, col=clr)\cr
\cr
clr <- c("sienna3", "dodgerblue3", "maroon3", "seagreen3", "turquoise3", "purple3", "yellow3")\cr
barplot(rep(1,7), names=clr, col=clr)\cr

When two variables are plotted, a legend is produced, with values for each level of the second variable.  By default, the location is top center. This position can be changed with the \code{legend.loc} option, which accepts any valid value consistent with the standard R \code{\link{legend}} function, used to generate the legend. Sometimes bars from the graph may intrude into the legend. One response is to re-run the analysis with the legend in a new location.  Another response is to invoke the \code{addtop} option to place more space between the top bar in the graph and the top of the graph.  
}


\author{David W. Gerbing (Portland State University; \email{davidg@sba.pdx.edu})}

\seealso{
\code{\link{barplot}}, \code{\link{legend}}.
}


\examples{
# ----------------------
# barchart from the data
# ----------------------

# generate some random data values for two variables
Pain <- sample(c("None", "Some", "Much", "Massive"), size=25, replace=TRUE)
Pain <- factor(Pain, levels=c("None", "Some", "Much", "Massive"))
Gender <- sample(c("Male", "Female"), size=25, replace=TRUE)
Gender <- factor(Gender)

# bar chart of the levels of Pain, all defaults except variable label
color.barchart(Pain, xlab="Pain Level")

# bar chart of Pain, Males and Females at each level of Pain
color.barchart(Pain, Gender, beside=TRUE, xlab="Pain Level")

# can enter the table counts directly, as in standard R barplot function
color.barchart(table(Pain), col.bars="sienna3", xlab="Pain Level")

# bar chart of Gender for each level of Pain, with clearance at the top
color.barchart(Gender, Pain, addtop=4)  

# many options
color.barchart(Pain, Gender, beside=TRUE, col.bars=c("coral3","seagreen3"), 
  legend.loc="topleft", legend.labels=c("The Girls", "The Boys"), 
  xlab="Pain Level", main="Gender and Pain Level", col.bg="cornsilk1")


# -----------------------------
# barchart directly from counts
# -----------------------------

# barchart of one variable
Employees <- c(206, 94, 382)
names(Employees) <- c("LA","Chicago","NY")
color.barchart(Employees, main="Employees in Each City", 
   xlab="Branch Offices", addtop=10, random=TRUE, vivid="hi")

# barchart of two variables with vivid colors
Pass <- c(19, 16, 23) 
Defective <- c(4, 2, 7) 
mymat <- rbind(Pass, Defective)
row.names(mymat) <- c("Pass","Defective")
colnames(mymat) <- c("Supplier1","Supplier2","Supplier3")
color.barchart(mymat, beside=TRUE, vivid="hi", random=TRUE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bar chart }
\keyword{ color }

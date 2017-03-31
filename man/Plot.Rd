\name{Plot}
\alias{Plot}
\alias{ScatterPlot}
\alias{sp}

\title{Plot One or Two Continuous and/or Categorical Variables}

\description{
Abbreviation: \code{sp}, \code{ScatterPlot}

To illustrate the values of a distribution, or of the relationship between two distributions, a scatterplot is a set of points plotted in an \emph{n}-dimensional coordinate system, in which the coordinates of each point are the values of the \emph{n} corresponding variables for a single observation (row of data). From the identical syntax, from any combination of continuous or categorical variables variables \code{x} and \code{y}, \code{Plot(x)} or \code{Plot(x,y)}, where \code{x} or \code{y} can be a vector, by default generates a family of related 1- or 2-variable scatterplots, as well as related statistical analyses. A categorical variable is either non-numeric, such as an R factor, or consists of a small number of equally spaced integer values. The maximum number of such values to define such an integer variable as categorical is set by the \code{n.cat} parameter, with a default value of 8. 

\code{Plot(x,y)}: x and y continuous yields traditional scatterplot of two continuous variables\cr
\code{Plot(x,y)}: x and y categorical, to solve the over-plot problem, a bubble (balloon) scatterplot, the size of each bubble based on the corresponding joint frequency as a replacement for the two dimensional bar chart\cr
\code{Plot(x,y)}: x (or y) categorical and the other variable continuous, a scatterplot with means at each level of the categorical variable\cr
\code{Plot(x,y)}: x (or y) categorical with unique (ID) values and the other variable continuous, a Cleveland dot plot\cr
\code{Plot(X,y)} or \code{Plot(x,Y)}: one vector variable defined by several continuous variables, paired with another single continuous variable, results in multiple scatterplots on the same graph\cr 
\code{Plot(x)}:  one continuous variable generates either a 1-dimensional scatterplot or a run chart with \code{run=TRUE}, or \code{x} can be an R time series variable for a time series chart\cr
\code{Plot(x)}: one categorical variable yields a 1-dimensional bubble plot to solve the over-plot problem for a more compact replacement of the traditional bar chart\cr
\code{Plot(X)}: one vector of continuous variables, with no \code{y}-variable, results in a scatterplot matrix\cr
\code{Plot(X)}: one vector of categorical \code{x}-variables, with no \code{y}-variable, generalizes to a matrix of 1-dimensional bubble plots, here called the bubble plot frequency matrix, to replace a series of bar charts\cr

Represent the influence of additional categorical variable with \code{by} or \code{by2} to generate Trellis plots conditioned on one or two variables from implicit calls to functions from Deepayan Sarkar's (2009) lattice package. Use \code{by.group} to group multiple variables on the same plot or on multiple panels. For a third variable, which is continuous, specify \code{size} for a bubble plot. By default, the \code{values} of analysis that generate the plotted points is \code{data}, or choose other values to plot, which are statistics computed from the data such as \code{count}.
}

\usage{
Plot(x, y=NULL, data=mydata,
         values=c("data", "count", "prop", "sum", "mean", "sd", "min",
                  "median", "max"),
         n.cat=getOption("n.cat"),

         by=NULL, by2=NULL, by.group=NULL,
         n.row=NULL, n.col=NULL, aspect="fill",

         fill=getOption("fill.pt"), stroke=getOption("stroke.pt"),
         bg=getOption("bg"), box=getOption("box"), 
         grid=getOption("grid"),
         segment=getOption("stroke.pt"),
         color=NULL, trans=NULL,

         cex.axis=0.76, axes="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         value.labels=NULL, label.max=20,
         rotate.x=0, rotate.y=0, offset=0.5,
         proportion=FALSE,

         size=NULL, size.cut=NULL, shape="circle", means=TRUE,
         sort.yx=FALSE, segments.y=FALSE, segments.x=FALSE,

         ID="row.name", ID.cut=NULL, ID.color="gray50", ID.size=0.75,

         radius=0.25, power=0.6, bubble.text="black",
         low.fill=NULL, hi.fill=NULL,

         smooth=FALSE, points.smooth=100, trans.smooth=0.25,
         bins.smooth=128,

         fit=FALSE, stroke.fit="gray20", lwd.fit=NULL, se.fit=0,

         ellipse=FALSE, stroke.ellipse=getOption("stroke.pt"),
         fill.ellipse=getOption("fill.ellipse"), lwd.ellipse=1,

         method="overplot", pt.reg="circle", pt.out="circle",
         out30="firebrick2", out15="firebrick4", new=TRUE,
         boxplot=FALSE,

         run=FALSE, lwd=2, area=FALSE, area.origin=0, 
         center.line=c("default", "mean", "median", "zero", "off"),
         show.runs=FALSE, stack=FALSE,

         bin.start=NULL, bin.width=NULL, bin.end=NULL,
         breaks="Sturges", cumul=FALSE,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,
         add.cex=1, add.lwd=1, add.lty="solid", 
         add.stroke="gray50", add.fill=getOption("fill.pt"),
         add.trans=NULL,

         digits.d=NULL, quiet=getOption("quiet"), do.plot=TRUE,
         width=NULL, height=NULL, pdf.file=NULL, 
         fun.call=NULL, \ldots)

ScatterPlot(\ldots)

sp(\ldots)
}

\arguments{
  \item{x}{By itself, or with \code{y}, by default a variable plotted by its
        values mapped to coordinates. The \bold{data} can be
        continuous, categorical or a time series. If \code{x} is sorted
        with equal intervals
        separating the values, or is a time series, then by default
        plots the points sequentially, joined by line segments.
        Can specify multiple \code{x}-variables or multiple \code{y}-variables
        as vectors, but not both. Can be in a data frame or defined
        in the global environment.}
  \item{y}{Variable with values to be mapped to coordinates of points in
        the plot on the vertical axis. Can be continuous or categorical.
        Can be in a data frame or defined in the global environment.} 
  \item{data}{Optional data frame that contains one or both of \code{x} and
        \code{y}. Default data frame is \code{mydata}.}
  \item{values}{The values as coordinates from which to plot the points,
        data values
        by default. If only \code{x} is specified,
        then the statistics \code{"count"} and \code{"prop"} can be specified
        for categories, or automatically binned values of a continuous variable.
        If there is a second variable, \code{y}, which is continuous, then
        for either a categorical \code{x} variable, or a continuous \code{x} variable
        with values binned into categories, then can apply \code{"mean"}, etc.}
  \item{n.cat}{Number of categories, specifies the largest number of
        unique, equally spaced integer values of variable for which
        the variable will be analyzed as categorical.
        Set to 0 to turn off, to force all such variables to be analyzed
        as continuous.}\cr

  \item{by}{A categorical variable called a conditioning variable that
        activates \bold{Trellis graphics}, from the lattice package, to provide
        a separate scatterplot (panel) of numeric primary variables \code{x}
        and \code{y} for each level of the variable.}
  \item{by2}{A second conditioning variable to generate Trellis
        plots jointly conditioned on both the \code{by} and \code{by2} variables,
        with \code{by2} as the row variable, which yields a scatterplot (panel)
        for each cross-classification of the levels of numeric \code{x} and
        \code{y} variables.}
  \item{by.group}{A categorical variable to provide a scatterplot for
        each level of the numeric primary variables \code{x} and
        \code{y} on the \emph{same} plot, which applies to the panels of
        a Trellis plot if \code{by} is specified.}
  \item{n.row}{Optional specification for the number of rows in the layout
        of a multi-panel display with Trellis graphics. Need not specify
        \code{n.col}.}
  \item{n.col}{Optional specification for the number of columns in the
        layout of a multi-panel display with
        Trellis graphics. Need not specify \code{n.row}. If set to 1, then
        the strip that labels each group locates to the left of each plot
        instead of the top.}
  \item{aspect}{Lattice parameter for the aspect ratio of the panels,
        defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to "bank" to 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr


  \item{fill}{The interior \bold{color} of the plotted points or bubbles.
       By default, is
       a partially transparent version of the border color, \code{stroke}. 
       If \code{y}-values are unique, as in a Cleveland dot plot, then no
       transparency by default as there can be no over-plotting. Remove with
       \code{fill="off"}. This
       and the following colors can also be changed globally, individually and as 
       a color theme, with the \code{lessR} \code{\link{global}} function.
       The \code{lessR} function \code{\link{showColors}} provides examples of all
       R named colors.}
  \item{stroke}{Border color of the plotted points, strokes,
       or, if there is a line and
       no points, color of the line. If there is a \code{by} variable,
       one value for each level of \code{by}. Remove with \code{stroke="off"}.}
  \item{bg}{Fill color of the plot area background. Remove with \code{bg="off"}.}
  \item{box}{Color of border around the plot background, the box, that encloses 
        the plot. Remove with \code{box="off"}.}
  \item{grid}{Color of the grid lines, a value of "on" restores the color from
        the current \code{global} if turned off by default as with a Cleveland dot
        plot. Remove with \code{grid="off"}.}
  \item{segment}{Color of connecting line segments when there are also plotted
        points, such as in a frequency
        polygon. Default color is \code{stroke}.}
  \item{color}{Simultaneously specifies both \code{stroke} and \code{fill}, and
       takes precedence over their individually specified values.}
  \item{trans}{Transparency level of plotted points from 0 (none) to 1 (complete).
        For plotting data values, fill transparency is 0.25 to allow for overlap
        of plotted points, otherwise set at 0 if overlap is not possible.}\cr

  \item{cex.axis}{Scale magnification factor of the values on the \bold{axes}.}
  \item{axes}{Color of the font used to label the axis values.}
  \item{xy.ticks}{Flag that indicates if tick marks and associated values on the 
        axes are to be displayed.}
  \item{xlab}{Label for \code{x}-axis. If \code{xlab} is not specified, then the label
       becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy.ticks} is \code{FALSE}, then no label is displayed.
       If no y variable is specified, then \code{xlab} is set to Index unless
       \code{xlab} has been specified.}
  \item{ylab}{Label for \code{y}-axis.  If \code{xlab} is not specified, then
       the label becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy.ticks} is \code{FALSE}, then no label displayed.}
  \item{main}{Label for the title of the graph.  If the corresponding variable
       labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below \code{xlab}.}
  \item{value.labels}{Labels for the \code{x}-axis on the graph to override 
        existing data values, including factor levels. If the variable is a 
        factor and \code{value.labels} is not specified (is \code{NULL}), then the
        value.labels are set to the factor levels with each space replaced by
        a new line character. If \code{x} and \code{y}-axes have the same scale,
        they also apply to the \code{y}-axis.}
  \item{label.max}{Maximum size of labels for the values of a categorical variable.
        Not a literal maximum as preserving unique values may require a larger number
        of characters than specified.}
  \item{rotate.x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate.y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}
  \item{proportion}{Specify proportions, relative frequencies, instead of counts.
        For a two variable bar chart, if \code{TRUE} then to facilitate group
        comparisons, displays the proportion of data values by fill variable within
        each group.}\cr
       
  \item{size}{When set to a constant, the scaling factor for \bold{standard points}
      (not bubbles) or a line, with default of 1.0 for points and 2.0 for a line.
       Set to 0 to not plot the points or lines. When set to a variable, activates a 
       bubble plot with the size of each bubble further determined
       by the value of \code{radius}.}
  \item{size.cut}{If \code{TRUE} (or \code{1}), then for a bubble plot, show the value
        of the sizing variable for a bubble in the center of selected bubbles,
        unless the bubble is too small.  If \code{FALSE}, no value is displayed.
        If a number greater than 1, then display the value only for the
        corresponding quantiles, such as just the max and min for a setting of 2,
        the default value when bubbles represent a size
        variable.  Color of the displayed text set by \code{bubble.text}.}
  \item{shape}{The plot character(s). The default value is a circle with both an
       stroke and filled area, specified with \code{stroke} and \code{fill}.
       Possible values are \code{circle}, \code{square}, \code{diamond},
       \code{triup} (triangle up), \code{tridown} (triangle down), all
       uppercase and lowercase letters, all digits, and most punctuation characters.
       The numbers 21 through 25 as defined by the R \code{\link{points}} function
       also apply. If plotting levels according to \code{by}, then list one shape for 
       each level to be plotted.}
  \item{means}{If the one variable is categorical the other variable continuous,
       then if \code{TRUE}, by default, plot means with the scatterplot. Also
       applies to a 1-D scatterplot.}
  \item{sort.yx}{Sort the values of \code{y} by the values of \code{x}, such as
        for a Cleveland dot plot, that is, a numeric \code{x}-variable paired
        with a categorical \code{y}-variable with unique values. If a \code{x}
        is a vector of two variables, sort by their difference.}
  \item{segments.y}{For one \code{x}-variable, draw a line segment from the
        \code{y}-axis to
        each plotted point, such as for the Cleveland dot plot. For two
        \code{x}-variables, the line segments connect the two points.}
  \item{segments.x}{Draw a line segment from the \code{x}-axis for each plotted point.}\cr

  \item{ID}{Name of variable to provide the \bold{labels for the plotted points},
       row names by default.}
  \item{ID.cut}{Proportion of plotted points to label, in order of their 
       Mahalanobis distance from the scatterplot center, so that more extreme
       points are labeled first. Defaults to 0 in 
       general for no labels, but for one or more ellipses, set to a value
       of 1 minus the data level of the largest ellipse, reduced by 0.04.}
  \item{ID.color}{Color of the text to display the labels, with a default
       of\code{"gray50"}.}
  \item{ID.size}{Size of the plotted labels, with a default of 0.75 according
       to the R parameter \code{cex}.}\cr

  \item{radius}{Scaling factor of the bubbles in a \bold{bubble plot}, which
        sets the radius of the largest displayed bubble in inches, with default of
        0.25 inches. Can activate by setting the value of \code{size} to
        a third variable, which
        sets the size of a bubble according to the size of the third variable.
        Or activate when the values of the variables are categorical, either a
        factor or an integer variable with the number of unique values less than
        \code{n.cat}, in which case the size of the bubbles represents
        frequency.} 
  \item{power}{Relative size of the scaling of the bubbles to each other.
        Value of 0.5 scales the bubbles so that the area of each bubble is the
        value of the corresponding sizing variable. Value of 1 scales so the
        radius of the bubble 
        is the value of the sizing variable, increasing the discrepancy of size
        between the variables. The default value is 0.6.}
  \item{bubble.text}{Color of the displayed text regarding the size of a bubble,
        either a tabulated frequency for categorical variables, or the value of a
        third variable according to \code{size}.}
  \item{low.fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient of the fill color
        beginning with this color.}
  \item{hi.fill}{For a categorical variables and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient of the fill color
        ending with this color.}\cr
  
  \item{smooth}{\bold{Smoothed density plot} for two numerical variables. By default,
        set to \code{TRUE} for 2500 or more rows of data.}
  \item{points.smooth}{Number of points superimposed on the density plot in the
        areas of
        the lowest density to help identify outliers, which controls how dark are the
        smoothed points.}
  \item{trans.smooth}{Exponent of the function that maps the density scale to the
        color scale.}
  \item{bins.smooth}{Number of bins in both directions for the density estimation.}\cr

  \item{fit}{The \bold{best fit line}.  Default value is \code{FALSE}, with
        options for \code{"loess"} and for least squares, indicated by
        \code{"ls"}. Or, if set to \code{TRUE}, then a loess line. Invoking
        any of the other fit parameters activates \code{fit}.}
  \item{stroke.fit}{Color of the best fitting line.}
  \item{lwd.fit}{Width of fit line. By default is 2 for Windows and 1.5 for Mac.}
  \item{se.fit}{Number of standard errors to plot around the fit. The default
       value of 0 turns off the standard error plot. Can be a vector to display
       multiple ranges.} 

  \item{ellipse}{If \code{TRUE}, enclose a scatterplot of only a single
        \code{x}-variable 
        and a single y-variable with the default .95 \bold{data ellipse}. Or can
        specify a single or vector of numeric values greater than 0 and less than 1,
        to plot one or more specified ellipses. The specified number of ellipses
        for each level of \code{by.group}. For Trellis graphics, only the
        maximum level applies and only one ellipse per panel.}
  \item{stroke.ellipse}{Color of the ellipses, the strokes of the filled ellipses.
        If specified, \code{ellipse} is set to \code{TRUE}.}
  \item{fill.ellipse}{If \code{TRUE}, fill color of the ellipses, with the
        default a highly
        transparent version of the fill color of the applicable color theme.
        If specified, \code{ellipse} is set to \code{TRUE}. Not applicable to
        Trellis graphics.}\cr
  \item{lwd.ellipse}{Line width of each ellipse.}

  \item{method}{Applies to a \bold{1-variable scatter plot} of a numerical variable,
        sometimes called a dot plot. Default is \code{"overplot"}, but can also set
        to \code{"jigger"} to scramble the points.}
  \item{pt.reg}{For 1-D scatterplot, type of regular (non-outlier) point. Default
        is 21, a circle with specified fill.}
  \item{pt.out}{For a 1-D scatterplot, type of point for outliers. Default is 19, a
        filled circle.}
  \item{out30}{For a 1-D scatterplot, color of outliers according to Tukey's
       definition based on the IQR.}
  \item{out15}{For a 1-D scatterplot, color of potential outliers.}
  \item{new}{If \code{FALSE}, then add the 1-D scatterplot to an existing graph.}
  \item{boxplot}{For a 1-D scatterplot, superimpose a box plot.}\cr
        
  \item{run}{If set to \code{TRUE}, generate a \bold{run chart}, i.e., line chart,
        in which
        points are plotted in the sequential order of occurrence in the data table.
        By default the points are connected by line
        segments to form a run chart. Set by default when the \code{x}-values
        are sorted with equal intervals or a single variable is a time series.}
  \item{lwd}{Width of the line segments. Set to zero to remove the line
        segments.}       
  \item{area}{Color of the fill area under a curve, the area between the curve
        and the axis. Can also be \code{TRUE}, which sets to the fill color
        for points, or a specific color can be specified. Default is \code{TRUE} if 
        multiple time series are plotted.}
  \item{area.origin}{Origin for the filled area under the time series line. Values
       less than this value are below the corresponding reference line, values
       larger are above the line.}
  \item{center.line}{Plots a dashed line through the middle of a run chart.  The two
        possible values for the line are \code{"mean"} and \code{"median"}.
        Provides a center line for the \code{"median"} by default when the values
        randomly vary about the mean. A value of \code{"zero"} specifies the center
        line should go through zero. Currently does not apply to Trellis plots.}
  \item{show.runs}{If \code{TRUE}, display the individual runs in the run analysis.
        Also sets \code{run} to \code{TRUE}.}
  \item{stack}{If \code{TRUE}, multiple time plots are stacked on each other, with
       \code{area} set to \code{TRUE} by default.}\cr

  \item{bin.start}{Optional specified starting value of the bins for a \bold{frequency
        polygon}, when \code{values} is set to \code{"count"} or \code{"prop"} 
        for a continuous \code{x} variable.}
  \item{bin.width}{Optional specified bin width value.}
  \item{bin.end}{Optional specified value that is within the last bin, so the
        actual endpoint of the last bin may be larger than the specified value.}
  \item{breaks}{The method for calculating the bins, or an explicit specification of
       the bins, such as with the standard R \code{\link{seq}} function or other
       options provided by the \code{\link{hist}} function.}
  \item{cumul}{Specify a cumulative frequency polygon.}\cr


  \item{add}{\bold{Draw one or more objects}, text or a geometric figures,
       on the plot.
       Possible values are any text to be written, or, to indicate a figure,
       \code{"text"}, \code{"rect"} (rectangle), \code{"line"}, \code{"arrow"},
       \code{"v.line"} (vertical line), and \code{"h.line"} (horizontal line).
       The value \code{"means"} is short-hand for vertical and horizontal lines
       at the respective means. Does not apply to Trellis graphics.}
  \item{x1}{First x coordinate to be considered for each object, can be
       \code{"mean.x"}. Not used for \code{"h.line"}.}
  \item{y1}{First y coordinate to be considered for each object, can be
       \code{"mean.y"}. Not used for\code{"v.line"}.}
  \item{x2}{Second x coordinate to be considered for each object, can be
       \code{"mean.x"}. Only used for \code{"rect"}, \code{"line"} and
       \code{arrow}.}
  \item{y2}{Second y coordinate to be considered for each object, can be
       \code{"mean.y"}.  Only used for \code{"rect"}, \code{"line"} and
       \code{arrow}.}
  \item{add.cex}{Text expansion factor, relative to 1. As with the following
   properties, can be a vector for multiple placement or objects.}
  \item{add.lwd}{Line width of added object.}
  \item{add.lty}{Line type of added object.}
  \item{add.stroke}{Color of borders and lines of added object.}
  \item{add.fill}{Interior fill color of added object.}
  \item{add.trans}{Transparency level of stroke or fill, which ever is
       applicable from 0 (opaque) to 1 (transparent).}\cr

  \item{digits.d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{global}} function.}
  \item{do.plot}{If \code{TRUE}, the default, then generate the plot.}
  \item{width}{Width of the plot window in inches, defaults to 5 except in RStudio
        to maintain an approximate square plotting area.}
  \item{height}{Height of the plot window in inches, defaults to 4.5 except for
        1-D scatterplots and when in RStudio.}
  \item{pdf.file}{Indicate to direct pdf graphics to the specified name of
        the pdf file.}
  \item{fun.call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{sp}.}\cr

  \item{\ldots}{Other parameter values for non-Trellis graphics as defined by and
      processed by standard R functions \code{\link{plot}} and \code{\link{par}},
      including\cr
      \code{xlim} and \code{ylim} for setting the range of the \code{x} and
        \code{y}-axes\cr
      \code{cex.main} for the size of the title\cr
      \code{col.main} for the color of the title\cr
      \code{cex} for the size of the axis value labels\cr
      \code{cex.lab} for the size of the axis labels\cr
      \code{col.lab} for the color of the axis labels\cr
      \code{lty} for line type, such as \code{"solid"}, \code{"dashed"},
      \code{"dotted"}, \code{"dotdash"}\cr
      \code{sub} and \code{col.sub} for a subtitle and its color\cr
      For one continuous variable, parameters from \code{\link{stripchart}}
  }
}


\details{
VARIABLES and TRELLIS PLOTS\cr
At a minimum there is one primary variable, \code{x}, which defines the coordinate system for plotting in terms of the \code{x}-axis, the horizontal axis. Most plots also specify a second primary variable, \code{y}, which defines the \code{y}-axis of the coordinate system. One of these primary variables may be a vector. The simplest plot is from the specification of only the primary variables, each as a single variable, which generates a single scatterplot of two variables, necessarily on a single plot, called a panel, defined by a single \code{x}-axis and usually a single \code{y}-axis.

For numeric primary variables, a single panel may also contain multiple scatterplots, of two types. The first type is formed from subsets of observations (rows of data) based on values of a categorical variable. Specify this plot with the \code{by.group} parameter, which identifies the grouping variable for which a scatterplot of the primary variables is generated at each of its levels. The points for each group are plotted with a different shape and/or color. By default, the colors vary, though to maintain the color scheme, if there are only two levels of the grouping variable, the points for one level are filled with the current theme color and the points for the second level are left with transparent interiors. (In versions of lessR prior to 3.5.6, before Trellis graphics were implemented, the \code{by} parameter had the meaning of the current \code{by.groups} parameter.)

Or, obtain multiple scatterplots on the same panel with multiple numeric \code{x}-variables, or multiple \code{y}-variables. To obtain this graph, specify one of the primary variables as a vector of multiple variables. 

Trellis graphics, from Deepayan Sarkar's (2009) \code{lattice} package, may be implemented in which multiple panels for one numeric \code{x}-variable and one numeric \code{y}-variable are displayed according to the levels of one or two categorical variables, called conditioning variables.  A variable specified with \code{by} is a conditioning variable that results in a Trellis plot, the scatterplot of \code{x} and \code{y} produced at \emph{each} level of the \code{by} variable. Inclusion of a second conditioning variable, \code{by2}, results in a separate scatterplot panel for \emph{each} combination of cross-classified values of both \code{by} and \code{by2}. A grouping variable according to \code{by.group} may also be specified, which is then applied to each panel.

The panel dimensions and the overall size of the Trellis plot can be controlled with the following parameters: \code{width} and \code{height} for the physical dimensions of the plot window, \code{n.row} and \code{n.col} for the number of rows and columns of panels, and \code{aspect} for the ratio of the height to the width of each panel. The plot window is the standard graphics window that displays on the screen, or it can be specified as a pdf file with the \code{pdf.file} parameter. 

CATEGORICAL VARIABLES\cr
At the conceptual level, there are continuous variables and categorical variables. Categorical variables have relatively few unique data values. However, categorical variables can be defined with non-numeric values, but also with numeric values, such as responses to a five-point Likert scale from Strongly Disagree to Strongly Agree, with responses coded 1 to 5. The three \code{by}-variables -- \code{by}, \code{by2} and \code{by.group} -- only apply to graphs created with numeric numeric \code{x} and \code{y} variables, continuous or categorical.

The standard and most general way to define a categorical variable is as an R factor, illustrated in the examples for the \code{\link{Transform}} function. \code{lessR} also provides the option of defining an integer variable with equally spaced values as categorical based on the value of \code{n.cat}, which can be set locally or globally with the \code{\link{global}} function. For example, for a variable with data values from 5-point Likert scale, a value of \code{n.cat} of 5 will define the define the variable as categorical. The default value is 8. To explicitly analyze the values as numerical, set \code{n.cat} to a value lower than 6, usually 0. Can also annotate a graph of the values of an integer categorical variable with \code{value.labels} option.  

A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 26 possible paired values to plot, so most of the plotted points overlap with others.  In this situation, that is, when a single variable or two variables with Likert response scales are specified, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values. A sunflower plot can be requested in lieu of the bubble plot by setting the \code{shape} to \code{"sunflower"}.

DATA\cr
The default input data frame is \code{mydata}.  Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{mydata$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment.

The data values themselves can be plotted, or for a single variable, counts or proportions can be plotted. For a categorical \code{x}-variable paired with a continuous variable, means and other statistics can be plotted  at each level of the \code{x}-variable. If \code{x} is continuous, it is binned first, with the standard \code{\link{Histogram}} binning parameters available, such as \code{bin.width}, to override default values. The \code{values} parameter sets the values to plot, with \code{data} the default. For example, requesting \code{count} as the value to plot for a continuous variable generates the scatterplot with the counts plotted against the binned values. By default the connecting line segments are provided, so a frequency polygon results. Turn off the lines by setting \code{lwd=0}.

VALUE LABELS\cr
The value labels for each axis can be over-ridden from their values in the data to user supplied values with the \code{value.labels} option. This option is particularly useful for Likert-style data coded as integers. Then, for example, a 0 in the data can be mapped into a "Strongly Disagree" on the plot. These value labels apply to integer categorical variables, and also to factor variables. To enhance the readability of the labels on the graph, any blanks in a value label translate into a new line in the resulting plot. Blanks are also transformed as such for the labels of factor variables. 

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function or \code{\link{VariableLabels}}.  If variable labels exist, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output. 

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced, that is, a call to the standard R \code{\link{plot}} function with \code{type="p"} for points. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{type="l"}, which connects each adjacent pair of points with a line segment.

Specifying multiple, continuous \code{x}-variables against a single y variable, or vice versa, results in multiple plots on the same graph. The color of the points of the second variable is the same as that of the first variable, but with a transparent fill. For more than two \code{x}-variables, multiple colors are displayed, one for each \code{x}-variable.

BUBBLE PLOT FREQUENCY MATRIX (BPFM)\cr
Multiple categorical variables for \code{x} may be specified in the absence of a \code{y} variable. A bubble plot results that illustrates the frequency of each response for each of the variables in a common figure in which the \code{x}-axis contains all of the unique labels for all of the variables plotted. Each line of information, the bubbles and counts for a single variable, replaces the standard bar chart in a more compact display. Usually the most meaningful when each variable in the matrix has the same response categories, that is, levels, such as for a set of shared Likert scales. The BPFM is considerably condensed presentation of frequencies for a set of variables than are the corresponding bar charts.

SCATTERPLOT MATRIX\cr
A single vector of continuous variables specified as \code{x}, with no \code{y}-variable, generates a scatterplot matrix of the specified variable. A continuous variable is defined as a numeric variable with more than n.cat unique responses. To force an item with a small number of unique responses, such as from a 5-pt Likert scale, to be treated as continuous, set \code{n.cat} to a number lower than 5, such as \code{n.cat=0} in the function call. 

The scatterplot matrix is displayed according to the current color theme. Specific colors such as \code{fill}, \code{stroke}, etc. can also be provided. The upper triangle shows the correlation coefficient, and the lower triangle each corresponding scatterplot, with, by default, the non-linear loess best fit line. The \code{code} fit option can be used to provide the linear least squares line instead, along with the corresponding \code{stroke.fit} for the color of the fit line.   

SIZE VARIABLE\cr
A variable specified with \code{size=} is a numerical variable that activates a bubble plot in which the size of each bubble is determined by the value of the corresponding value of \code{size}, which can be a variable or a constant.

To explicitly vary the shapes, use \code{shape} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{fill}, such as with R standard color names. If \code{fill} is specified without \code{shape}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable. 

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{ellipse} function, written by Duncan Murdoch and E. D. Chow, from the \code{ellipse} package. The axes are automatically lengthened to provide space for the entire ellipse that extends beyond the maximum and minimum data values. The specific level of the ellipse can be specified with a numerical value in the form of a proportion. Multiple numerical values of \code{ellipse} may also be specified to obtain multiple ellipses. 

ONE VARIABLE PLOT\cr
The one variable plot is a 1-dimensional scatterplot, that is, a dot chart. For a numerical variable, results are based on the standard \code{\link{stripchart}} function. Colors are provided by default and can also be specified. For gray scale output, potential outliers are plotted with squares and actual outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The definition of outliers are from the R \code{\link{boxplot}} function.  The plot can also be obtained as a bubble plot for a categorical variable.

TIME CHARTS\cr
Specifying one or more \code{x}-variables with no \code{y}-variables, and \code{run=TRUE} plots the \code{x}-variables in a run chart. The values of the specified \code{x}-variable are plotted on the \code{y}-axis, with Index on the \code{x}-axis. Index is the ordinal position of each data value, from 1 to the number of values. 

If the specified \code{x}-variable is of type \code{Date}, or is a time series, a time series plot is generated for each specified variable. If a formal R time-series, univariate or multivariate, specify as the \code{x}-variable. Or, specify the \code{x}-variable of type Date, and then specify the  \code{y}-variable as one or more time series to plot. The \code{y}-variable can be formatted as tidy data with all the values in a single column, or as wide-formatted data with the time-series variables in separate columns. 

2-D KERNEL DENSITY\cr
With \code{smooth=TRUE}, the R function \code{\link{smoothScatter}} is invoked according to the current color theme. Useful for very large data sets. The \code{points.smooth} parameter plots points from the s of the lowest density. The \code{bins.smooth} parameter specifies the number of bins in both directions for the density estimation. The \code{trans.smooth} parameter specifies the exponent in the function that maps the density scale to the color scale to allow customization of the intensity of the plotted gradient colors. Higher values result in less color saturation, de-emphasizing points from regions of lessor density. These parameters are respectively passed directly to the \code{\link{smoothScatter}} \code{nrpoints}, \code{nbin} and \code{transformation} parameters. Grid lines are turned off, but can be displayed by setting the \code{grid} parameter.

COLORS\cr
Individual colors in the plot can be manipulated with options such as \code{fill} for the interior color of a plotted point. A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{theme}}. The default color theme is \code{dodgerblue}. A gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{theme}}, such as \code{"sienna"} and \code{"orange.black"}. Use the option \code{ghost=TRUE} for a black background, no grid lines and partial transparency of plotted colors. 

Colors can also be changed for individual aspects of a scatterplot as well. To provide a warmer tone by slightly enhancing red, try a background color such as \code{bg="snow"}. Obtain a very light gray with \code{bg="gray99"}.  To darken the background gray, try \code{bg="gray97"} or lower numbers. See the \code{lessR} function \code{\link{showColors}}, which provides an example of all available named colors.

For the color options, such as \code{grid}, the value of \code{"off"} is the same as 
\code{"transparent"}.\cr

ANNOTATIONS\cr
Use the \code{add} and related parameters to annotate the plot with text and/or geometric figures. Each object is placed according from one to four corresponding coordinates, the needed coordinates required to plot that object, as shown in the following table. \code{x}-coordinates may have the value of \code{"mean.x"} and \code{y}-coordinates may have the value of \code{"mean.y"}.\cr

\tabular{lll}{
Value \tab Object \tab Coordinates\cr
----------- \tab ------------------- \tab ----------------\cr
text \tab text \tab x1, x2\cr
\code{"rect"} \tab rectangle \tab x1, y1, x2, y2\cr
\code{"line"} \tab line segment \tab x1, y1, x2, y2\cr
\code{"arrow"} \tab arrow \tab x1, y1, x2, y2\cr
\code{"v.line"} \tab vertical line  \tab x1\cr
\code{"h.line"} \tab horizontal line  \tab y1\cr
\code{"means"} \tab horiz, vert lines  \tab \cr
----------- \tab ------------------- \tab ----------------\cr
}

The value of \code{add} specifies the object. For a single object, enter a single value. Then specify the value of the needed corresponding coordinates, as specified in the above table. For multiple placements of that object, specify vectors of corresponding coordinates. To annotate multiple objects, specify multiple values for \code{add} as a vector. Then list the corresponding coordinates for up to each of four coordinates in the order of the objects listed in \code{add}. See the examples for more explanation.

Can also specify vectors of different properties, such as \code{add.stroke}. That way, different objects can be of different colors, different transparency levels, etc.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf.file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ADDITIONAL OPTIONS\cr
Commonly used graphical parameters that are available to the standard R function \code{\link{plot}} are also generally available to \code{\link{Plot}}, such as:

\describe{
\item{cex.main, col.lab, font.sub, etc.}{Settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}
\item{main}{Title of the graph, see \code{\link{title}}.}
\item{xlim}{The limits of the plot on the \code{x}-axis, expressed as c(x1,x2), where \code{x1} and \code{x2} are the limits. Note that \code{x1 > x2} is allowed and leads to a reversed axis.}
\item{ylim}{The limits of the plot on the \code{y}-axis.}

ONLY VARIABLES ARE REFERENCED\cr
A referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, such as the default \code{mydata}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > ScatterPlot(rnorm(50), rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > X <- rnorm(50)   # create vector X in user workspace
    > Y <- rnorm(50)   # create vector Y in user workspace
    > ScatterPlot(X,Y)     # directly reference X and Y}
}

}

\references{
Murdoch, D, and  Chow, E. D. (2013).  \code{ellipse} function from the \code{ellipse} package package. 

Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 8, NY: Routledge.

Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R, Springer. http://lmdvr.r-forge.r-project.org/
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{stripchart}}, \code{\link{title}}, \code{\link{par}}, \code{\link{loess}}, \code{\link{Correlation}}, \code{\link{theme}}.
}


\examples{
# read the data
mydata <- rd("Employee", format="lessR", quiet=TRUE)
mydata <- Subset(random=.4, quiet=TRUE)  # less computationally intensive
# many examples commented out to reduce CPU time for the CRAN submission

#---------------------------------------------------
# traditional scatterplot with two numeric variables
#---------------------------------------------------

# scatterplot with all defaults
Plot(Years, Salary)
# or use abbreviation sp in place of Plot

# new shape and point size, no grid or background color
# Plot(Years, Salary, size=2, shape="diamond", bg="off", grid="off")

# bubble plot with size determined by the value of Pre
# display the value for the bubbles with values of  min, median and max
# Plot(Years, Salary, size=Pre, size.cut=3)

# variables of interest are in a data frame not the default mydata
# plot 0.6 and 0.9 data ellipses
# change color theme to gray scale, then back to default
global("gray")
Plot(eruptions, waiting, ellipse=seq(.6,.9), data=faithful)
global("dodgerblue")

# scatterplot with two x-variables, plotted against Salary
Plot(c(Pre, Post), Salary)

# increase span (smoothing) from default of .7 to 1.25
# span is a loess parameter and generates a caution that can be
#   ignored that it is not a graphical parameter -- we know that
# Plot(Years, Salary, fit="loess", span=1.25)

# 2-D kernel density (more useful for larger sample sizes) 
# Plot(Years, Salary, smoothed=TRUE)


#------------------------------------------------------
# scatterplot matrix from a vector of numeric variables
#------------------------------------------------------

# with least squares fit line and color options
Plot(c("Salary", "Years", "Pre"), fit="ls", bg="powderblue", stroke.fit="red")


#--------------------------------------------------------
# Trellis graphics and by.group with two numeric variables
#---------------------------------------------------------

# Trellis plot with condition on 1-variable
# Plot(Years, Salary, by=Dept)

# Trellis plot with condition on 2-variables and groups
Plot(Years, Salary, by=Dept, by2=Gender, fit="ls", by.group=HealthPlan)

# vary both shape and color with a least-squares fit line for each group
# Plot(Years, Salary, by=Gender, fit="ls", 
#     color=c("darkgreen", "brown"), shape=c("F","M"), size=.8)

# compare the men and women Salary according to Years worked
# Plot(Years, Salary, by.group=Gender, ellipse=.50)

#--------------------------------------------------
# analysis of a single numeric variable (or vector)
#--------------------------------------------------

# 1-variable scatterplots
# ------------------------
# 1-variable scatterplot, continuous
# custom colors for outliers
Plot(Salary, out15="hotpink", out30="darkred")

# one variable scatterplot with added jitter of points and a boxplot
# Plot(Salary, method="jitter", boxplot=TRUE)

# binned values to plot counts
# ----------------------------
# bin the values of Salary to plot counts as a frequency polygon
Plot(Salary, values="count")  # bin the values

# time charts
#------------
# run chart, with fill area
Plot(Salary, run=TRUE, area="steelblue")

# two run charts in same plot
# or could do a multivariate time series
# Plot(c(Pre, Post), run=TRUE)

# Trellis graphics run chart with custom line width
# Plot(Salary, run=TRUE, by=Gender, lwd=3)

# daily time series plot
# create the daily time series from R built-in data set airquality
oz.ts <- ts(airquality$Ozone, start=c(1973, 121), frequency=365)
Plot(oz.ts)

# multiple time series plotted from dates and stacked
# black background with translucent areas
# date <- seq(as.Date("2013/1/1"), as.Date("2016/1/1"), by = "quarter")
# x1 <- rnorm(13, 100, 15)
# x2 <- rnorm(13, 100, 15)
# x3 <- rnorm(13, 100, 15)
# df <- data.frame(date, x1, x2, x3)
# Plot(date, x1:x3, data=df, area="steelblue3", stroke="steelblue2",
#      trans=.55, bg="gray10", grid="gray25")


#------------------------------------------
# analysis of a single categorical variable
#------------------------------------------

# default 1-D bubble plot
# frequency plot, replaces bar chart 
Plot(Dept)

# abbreviated category labels
# Plot(Dept, label.max=2)

# plot of frequencies for each category (level), replaces bar chart 
# Plot(Dept, values="count")


#----------------------------------------------------
# scatterplot of numeric against categorical variable 
#----------------------------------------------------

# generate a chart with the plotted mean of each level
# rotate x-axis labels and then offset to fit
Plot(Dept, Salary, rotate.x=45, offset=1)


#-------------------
# Cleveland dot plot 
#-------------------

# row.names on the y-axis
Plot(Salary, row.names)

# standard scatterplot
# Plot(Salary, row.names, sort.yx=FALSE, segments.y=FALSE, grid="on")

# Cleveland dot plot with two x-variables
# Plot(c(Pre, Post), row.names)


#------------
# annotations
#------------

# add text at the one location specified by x1 and x2
# Plot(Years, Salary, add="Hi There", x1=12, y1=80000)

# add text at three different specified locations 
# Plot(Years, Salary, add="Hi There", x1=c(12, 16, 18), y1=c(80000, 100000, 60000))

# add three different text blocks at three different specified locations
# Plot(Years, Salary, add=c("Hi", "Bye", "Wow"), x1=c(12, 16, 18), y1=c(80000, 100000, 60000))

# add an 0.95 data ellipse and horizontal and vertical lines through the
#  respective means
Plot(Years, Salary, ellipse=TRUE, add=c("v.line", "h.line"), x1="mean.x", y1="mean.y")
# can be done also with the following short-hand
# Plot(Years, Salary, ellipse=TRUE, add=c("means"))

# a rectangle requires two points, <x1,y1> and <x2,y2>
# Plot(Years, Salary, add="rect", x1=12, y1=80000, x2=16, y2=115000,
#      add.trans=.8, add.fill="gold", add.stroke="gold4", add.lwd=0.5)

# the first object, a rectangle, requires all four coordinates
# the vertical line at x=2 requires only an x1 coordinate, listed 2nd 
# Plot(Years, Salary, add=c("rect", "v.line"), x1=c(10, 2), y1=80000, x2=12, y2=115000)

# two different rectangles with different locations, fill colors and translucence
# Plot(Years, Salary, add=c("rect", "rect"), 
#      x1=c(10, 2), y1=c(60000, 45000), x2=c(12, 75000), y2=c(80000, 55000),
#      add.fill=c("gold3", "green"), add.trans=c(.8,.4))


#----------------------------------------------------
# analysis of two categorical variables (Likert data)
#----------------------------------------------------

mydata <- rd("Mach4", format="lessR", quiet=TRUE)  # Likert data, 0 to 5
mydata <- Subset(random=.4, quiet=TRUE)  # less computationally intensive

# size of each plotted point (bubble) depends on its joint frequency
# triggered by default when  < n.cat=8 unique values for each variable
Plot(m06, m07)

# use value labels for the integer values, modify color options
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree")
Plot(m06,  m07, value.labels=LikertCats,
     fill="powderblue", stroke="blue", bubble.text="darkred")

# get correlation analysis instead of cross-tab analysis:
#   maximum number of categories of equally spaced integer values
#   to define a variable as categorical here specified as 0
Plot(m06, m07, n.cat=0)

# proportions within each level of the other variable
Plot(m06, m07, proportion=TRUE)


#-----------------------------
# Bubble Plot Frequency Matrix
#-----------------------------

Plot(c(m06,m07,m09,m10), value.labels=LikertCats)


#---------------
# function curve
#---------------

x <- seq(10,50,by=2) 
y1 <- sqrt(x)
y2 <- x**.33
# x is sorted with equal intervals so run chart by default
Plot(x, y1)
# custom function plot
Plot(x, y1, ylab="My Y", xlab="My X", main="My Curve", stroke="blue", 
  bg="snow", area="lightsteelblue", grid="lightsalmon")

# multiple plots, need data frame
mydata <- data.frame(x, y1, y2)
Plot(x, c(y1, y2))



#-----------
# modern art
#-----------

clr <- colors()
clr[-(153:353)]  # get rid of most of the grays
n <- sample(2:30, size=1)
x <- rnorm(n)
y <- rnorm(n)
color1 <- clr[sample(1:length(clr), size=1)]
color2 <- clr[sample(1:length(clr), size=1)]
Plot(x, y, run=TRUE, area=color1, stroke=color2,
   xy.ticks=FALSE, main="Modern Art", xlab="", ylab="",
   cex.main=2, col.main="lightsteelblue", n.cat=0)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }



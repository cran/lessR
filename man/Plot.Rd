\name{Plot}
\alias{Plot}
\alias{pl}
\alias{ScatterPlot}
\alias{sp}
\alias{BoxPlot}
\alias{bx}
\alias{ViolinPlot}
\alias{vp}

\title{Plot One or Two Continuous and/or Categorical Variables}

\description{
Abbreviation:\cr
\verb{  }Violin Plot only: \code{vp}, \code{ViolinPlot}\cr
\verb{  }Box Plot only: \code{bx}, \code{BoxPlot}\cr
\verb{  }Scatter Plot only: \code{sp}, \code{ScatterPlot}\cr

A scatterplot displays the values of a distribution, or the relationship between the two distributions in terms of their joint values, as a set of points in an \emph{n}-dimensional coordinate system, in which the coordinates of each point are the values of \emph{n} variables for a single observation (row of data). From the identical syntax, from any combination of continuous or categorical variables variables \code{x} and \code{y}, \code{Plot(x)} or \code{Plot(x,y)}, where \code{x} or \code{y} can be a vector, by default generates a family of related 1- or 2-variable scatterplots, possibly enhanced, as well as related statistical analyses. A categorical variable is either non-numeric, such as an R factor, or may be defined to consist of a small number of equally spaced integer values_ The maximum number of such values to define such an integer variable as categorical is set by the \code{n_cat} parameter, with a default value of 0, that is, by default, all variables with numerical values are defined as continuous variables. 

\code{Plot} is a general function, which produces a wide variety of scatterplots, which, for a single variable, can be in the context of violin plots and box plots, as outlined in the following list. The parameter definitions that follow this list are grouped, with parameters that relate to the same type of plot defined in the same group.

\code{Plot(x,y)}: x and y continuous yields traditional scatterplot of two continuous variables\cr
\code{Plot(x,y)}: x and y categorical, to solve the over-plot problem, yields a bubble (balloon) scatterplot, the size of each bubble based on the corresponding joint frequency as a replacement for the two dimensional bar chart\cr
\code{Plot(x,y)}: x (or y) categorical and the other variable continuous, yields a scatterplot with means at each level of the categorical variable\cr
\code{Plot(x,y)}: x (or y) categorical with unique (ID) values and the other variable continuous, yields a Cleveland dot plot\cr
\code{Plot(X,y)} or \code{Plot(x,Y)}: one vector variable defined by several continuous variables, paired with another single continuous variable, yields multiple scatterplots on the same graph\cr 
\code{Plot(x)}: one continuous variable generates either, a violin/box/scatterplot (VBS plot), introduced here, or a run chart with \code{run=TRUE}, or \code{x} can be an R time series variable for a time series chart\cr
\code{Plot(x)}: one categorical variable yields a 1-dimensional bubble plot to solve the over-plot problem for a more compact replacement of the traditional bar chart\cr
\code{Plot(X)}: one vector of continuous variables, with no \code{y}-variable, results in a scatterplot matrix\cr
\code{Plot(X)}: one vector of categorical \code{x}-variables, with no \code{y}-variable, generalizes to a matrix of 1-dimensional bubble plots, here called the bubble plot frequency matrix, to replace a series of bar charts\cr

Represent the influence of additional categorical variables with \code{by1} or \code{by2} to generate Trellis plots conditioned on one or two variables from implicit calls to functions from Deepayan Sarkar's (2009) lattice package. Use \code{by} to group multiple variables on the same plot, or on multiple panels if Trellis graphics are activated. For a third variable, which is continuous, specify \code{size} for a bubble plot. By default, the \code{values} of analysis that generate the plotted points is \code{data}, or choose other values to plot, which are statistics computed from the data such as \code{mean}.
}

\usage{
Plot(x, y=NULL, data=d, rows=NULL,
         stat_x=c("data", "count", "proportion", "\%"),
         stat_yx=c("data", "sum", "mean", "sd", "dev", "min", "median", "max"),
         n_cat=getOption("n_cat"),

         by=NULL, by1=NULL, by2=NULL,
         n_row=NULL, n_col=NULL, aspect="fill",

         theme=getOption("theme"),
         fill=NULL,
         color=NULL,
         trans=getOption("trans_pt_fill"),

         size=NULL, size_cut=NULL, shape="circle", means=TRUE,
         sort_yx=c("0", "-", "+"), segments_y=FALSE, segments_x=FALSE,
         jitter_x=0, jitter_y=0,

         ID="row.name", ID_size=0.7,
         MD.cut=0, out_cut=0, out_shape="circle", out_size=1,

         vbs_plot="vbs", vbs_size=0.9, bw=NULL, bw_iter=10,
         violin_fill=getOption("violin_fill"), 
         box_fill=getOption("box_fill"), 
         vbs_pt_fill="black",
         vbs_mean=FALSE, fences=FALSE,
         k=1.5, box_adj=FALSE, a=-4, b=3,

         radius=NULL, power=0.5, low_fill=NULL, hi_fill=NULL,

         smooth=FALSE, smooth_points=100, smooth_size=1,
         smooth_exp=0.25, smooth_bins=128,

         fit="off", fit_se=0.95, ellipse=0, 

         bin=FALSE, bin_start=NULL, bin_width=NULL, bin_end=NULL,
         breaks="Sturges", cumulate=FALSE,

         run=FALSE, lwd=2, area_fill="transparent", area_origin=0, 
         center_line=c("default", "mean", "median", "zero", "off"),
         show_runs=FALSE, stack=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
         lab_adj=c(0,0), margin_adj=c(0,0,0,0),

         rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
         offset=getOption("offset"),

         xy_ticks=TRUE, value_labels=NULL, label_max=20, origin_x=NULL,
         legend_title=NULL, scale_x=NULL, scale_y=NULL,

         add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

         enhance=FALSE, eval_df=NULL, digits_d=NULL, quiet=getOption("quiet"),
         do_plot=TRUE, width=NULL, height=NULL, pdf_file=NULL, 
         fun_call=NULL, \ldots)

pl(\ldots)
ScatterPlot(\ldots)
sp(\ldots)
BoxPlot(\ldots)
bx(\ldots)
ViolinPlot(\ldots)
vp(\ldots)
}

\arguments{
  \item{x}{By itself, or with \code{y}, by default, a \emph{primary variable},
        that is, plotted by its values mapped to coordinates.
        The \bold{data values} can be
        continuous or categorical, cross-sectional or a time series.
        If \code{x} is sorted, with equal intervals
        separating the values, or is a time series, then by default
        plots the points sequentially, joined by line segments_
        Can specify multiple \code{x}-variables or multiple \code{y}-variables
        as vectors, but not both. Can be in a data frame or defined
        in the global environment.}
  \item{y}{An optional second \emph{primary variable.} Variable with values
        to be mapped to coordinates of points in
        the plot on the vertical axis_ Can be continuous or categorical.
        Can be in a data frame or defined in the global environment.} 
  \item{data}{Optional data frame that contains one or both of \code{x} and
        \code{y}. Default data frame is \code{d}.}
  \item{rows}{A logical expression that specifies a subset of rows of the data frame
       to analyze.}
  \item{stat_x}{Transform data for categorical variable \code{x}
       into a simple table of paired levels of
       \code{x} with numerical values of \code{y}, height of each bar.
       Apply specified transformation such as \code{"count"}
       to each of the levels of \code{x}. Dot plot analogous to a bar chart.}
  \item{stat_yx}{Transformation of \code{y} for each level of \code{x}.
       Transform data into a simple table of paired levels of
       \code{x} with numerical values of \code{y}. Apply specified transformation
       of \code{y} to the levels of \code{x}, that is,
       \code{"mean"}, \code{sd}, etc.}

  \item{n_cat}{Number of categories, specifies the largest number of
        unique, equally spaced integer values of a variable for which
        the variable will be analyzed as categorical instead of continuous.
        Default is 0. Use to specify that such variables  are to be analyzed
        as categorical, a kind of informal R factor.}\cr

  \item{by}{A categorical variable to provide a scatterplot for
        each level of the numeric primary variables \code{x} and
        \code{y} on the \emph{same} plot, a \emph{grouping variable}.
        For two-variable plots, applies to the panels of a
        \bold{Trellis graphic} if \code{by1} is specified.}
  \item{by1}{A categorical variable called a \emph{conditioning variable} that
        activates Trellis graphics, provided by Deepayan Sarkar's (2007) lattice
        package, to provide
        a \emph{separate} panel of numeric primary variables \code{x}
        and \code{y} for each level of the variable. Re-order the levels by
        first converting to a factor variable with \code{\link{factor}} or
        lessR \code{\link{factors}}.}
  \item{by2}{A second \emph{conditioning variable} to generate Trellis
        plots jointly conditioned on both the \code{by1} and \code{by2} variables,
        with \code{by2} as the row variable, which yields a scatterplot (panel)
        for \emph{each} cross-classification of the levels of numeric \code{x} and
        \code{y} variables.}
  \item{n_row}{Optional specification for the number of rows in the layout
        of a multi-panel display with Trellis graphics. Specify
        \code{n_col} or \code{n_row}, but not both.}
  \item{n_col}{Optional specification for the number of columns in the
        layout of a multi-panel display with
        Trellis graphics. Specify \code{n_col} or \code{n_row}, but 
        not both. If set to 1, then
        the strip that labels each group locates to the left of each plot
        instead of the top.}
  \item{aspect}{Lattice parameter for the aspect ratio of the panels,
        defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to "bank" to 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr

  \item{theme}{Color theme for this analysis. Make persistent across analyses
        with \code{\link{style}}.}
  \item{fill}{Either fill color of the points or the area under a line chart.
        Can also set with the lessR function \code{\link{getColors}} to
        select from a variety of color palettes. For points, default is
        \code{pt_fill} and for area under a line chart, \code{violin_fill}.
        For a line chart, set to \code{"on"} for default color_}
  \item{color}{Border color of the points or line_color for line plot,
        can be a vector to customize the color for each point or a color
        range such as "blues" (see \code{\link{getColors}}. Default is
        \code{pt_color} from the lessR \code{\link{style}} function.}
  \item{trans}{Transparency factor of the fill color of each point.
        Default is
        \code{trans_pt_fill} from the lessR \code{\link{style}} function.}\cr

  \item{size}{When set to a constant, the scaling factor for \bold{standard points}
      (not bubbles) or a line, with default of 1.0 for points and 2.0 for a line.
       Set to 0 to not plot the points or lines. When set to a variable, activates a 
       bubble plot with the size of each bubble further determined
       by the value of \code{radius}. Applies to the standard two-variable
       scatterplot as well as to the scatterplot component of the
       integrated Violin-Box-Scatterplot (VBS) of a single continuous variable.}
  \item{size_cut}{If \code{TRUE} (or \code{1}), then for a bubble plot in which the
       bubble sizes are defined by a
       \code{size} variable, show the value
       of the sizing variable for selected bubbles in the center of
       the bubbles, unless the bubble is too small.
       If \code{FALSE}, no value is displayed.
      If a number greater than 1, then display the value only for the
      indicated number of values, such as just the max and min for a setting of 2,
      the default value when bubbles represent a size
      variable.  Color of the displayed text set by \code{bubble_text} from
      the \code{\link{style}} function.}
  \item{shape}{The plot character(s). The default value is a circle with both an
       color and filled interior, specified with \code{color} and \code{fill}.
       Possible values are \code{circle}, \code{square}, \code{diamond},
       \code{triup} (triangle up), \code{tridown} (triangle down), all
       uppercase and lowercase letters, all digits, and most punctuation characters.
       The numbers 21 through 25 as defined by the R \code{\link{points}} function
       also apply. If plotting levels according to \code{by}, then list one
       shape for each level to be plotted.}
  \item{means}{If the one variable is categorical the other variable continuous,
       then if \code{TRUE}, by default, plot means with the scatterplot. Also
       applies to a 1-D scatterplot.}
  \item{sort_yx}{Sort the values of \code{y} by the values of \code{x}, such as
        for a Cleveland dot plot, that is, a numeric \code{x}-variable paired
        with a categorical \code{y}-variable with unique values_ If a \code{x}
        is a vector of two variables, sort by their difference.}
  \item{segments_y}{For one \code{x}-variable, draw a line segment from the
        \code{y}-axis to
        each plotted point, such as for the Cleveland dot plot. For two
        \code{x}-variables, the line segments connect the two points.}
  \item{segments_x}{Draw a line segment from the \code{x}-axis for each
        plotted point.}
  \item{jitter_x}{Randomly perturbs the plotted points of 
        a scatterplot horizontally according to an internally 
        computed formula, or can be explicitly specified.}
  \item{jitter_y}{Randomly perturbs the plotted points of 
        a scatterplot vertically according to an internally 
        computed formula, or can be explicitly specified.}\cr


  \item{ID}{Name of variable to provide the \bold{labels for the selected
       plotted points for outlier identification}, row names of data frame
       by default. To label all
       the points use the \code{add} parameter described later.}
  \item{ID_size}{Size of the plotted labels.
        Modify text color of the labels with the \code{\link{style}} function 
        parameter \code{ID_color}.}
  \item{MD.cut}{Mahalanobis distance cutoff to define an outlier in a 2-variable
       scatterplot.}
  \item{out_cut}{Count or proportion of plotted points to label, in order of their 
       distance from the scatterplot center (means), counting down from
       the more extreme point. For two-variable plots, assess distance
       from the center with Mahalanobis distance. For VBS plots of a single
       continuous variable, refers to outliers on each side of the plot.}
  \item{out_shape}{Shape of outlier points in a 2-variable scatterplot
        or a VBS plot.
        Modify fill color from the current \code{theme} with the
        \code{\link{style}} function parameters \code{out_fill} and
        \code{out2_fill}.}
  \item{out_size}{Size of outlier points in a 2-variable scatterplot
        or VBS plot.}\cr

  \item{vbs_plot}{A character string that specifies the components of the 
        \bold{integrated Violin-Box-Scatterplot (VBS) of a continuous variable}.
        A \code{"v"} in the string indicates a violin plot, a \code{"b"}
        indicates a box plot with flagged outliers, and a \code{"s"}
        indicates a 1-variable scatterplot. Default value is \code{"vbs"}. 
        The characters can be in any order and upper- or lower-case.
        Generalize to Trellis plots with the 
        \code{by1} and \code{by2} parameters, but currently only applies
        to horizontal displays.
        Modify fill and border colors from the current \code{theme} with
        the \code{\link{style}} function parameters \code{violin_fill},
        \code{violin_color}, \code{box_fill} and \code{box_color}.}
  \item{vbs_size}{Width of the violin plot to the plot area. Make the
        violin (and also the accompanying box plot) larger or smaller by
        making the plot area and/or this value larger or smaller.}
  \item{bw}{Bandwidth for the smoothness of the violin plot. Higher values 
        for smoother plots. Default is to calculate a bandwidth that provides
        a relative smooth density plot.}
  \item{bw_iter}{Number of iterations used to modify default R bandwidth
        to further smooth the obtained density estimate. When set, also
        displays the iterations and corresponding results.}
  \item{violin_fill}{Fill color for a \bold{violin plot}.}
  \item{box_fill}{Fill color for a box plot.}
  \item{vbs_pt_fill}{Points in a VBS scatterplot are black by default because
        the background is the violin, which is based on the current theme
        color_ To use the values for \code{pt_fill} and \code{pt_color} 
        specified by the \code{\link{style}} function, set to \code{"default"}.
        Or set to any desired color_}
  \item{vbs_mean}{Show the mean on the box plot with a strip the color
        of \code{out_fill}, which can be changed with the 
        \code{\link{style}} function.}
  \item{fences}{If \code{TRUE}, draw the inner upper and lower fences as
        dotted line segments_}
  \item{k}{IQR multiplier for the basis of calculating the distance of the 
        whiskers of the box plot from the box. Default is Tukey's setting
        of 1.5.}
  \item{box_adj}{Adjust the box and whiskers, and thus outlier detection,
        for skewness using the medcouple statistic as the robust measure
        of skewness according to Hubert and Vandervieren (2008).}
  \item{a, b}{Scaling factors for the adjusted box plot to set the length
        of the whiskers. If explicitly set, activates \code{box_adj}.}\cr

  \item{radius}{Scaling factor of the bubbles in a \bold{bubble plot}, which
        sets the radius of the largest displayed bubble in inches. To
        activate, either set the value of \code{size} to
        a third variable where the default is 0.10,
        or for categorical variables, either a
        factor or an integer variable with the number of unique values less than
        \code{n_cat}, the size of the bubbles represents
        frequency, with a default of 0.22.} 
  \item{power}{Relative size of the scaling of the bubbles to each other.
        Default value of 0.5 scales the bubbles so that the area of each
        bubble is the value of the corresponding sizing variable. Value of 1
        scales so the radius of the bubble 
        is the value of the sizing variable, increasing the discrepancy of size
        between the variables.}
  \item{low_fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient of the fill color
        beginning with this color_}
  \item{hi_fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient of the fill color
        ending with this color_}\cr
  
  \item{smooth}{\bold{Smoothed density plot} for two numerical variables. By 
        default, set to \code{TRUE} for 2500 or more rows of data.}
  \item{smooth_points}{Number of points superimposed on the density plot in the
        areas of the lowest density to help identify outliers, which controls
        how dark are the smoothed points.}
  \item{smooth_size}{Size of points superimposed on the density plot.}
  \item{smooth_exp}{Exponent of the function that maps the density scale to
        the color scale.}
  \item{smooth_bins}{Number of bins in both directions for the density
        estimation.}\cr

  \item{fit}{The \bold{best fit line}.  Default value is \code{"off"}, with
        options for non-linear\code{"loess"} and for linear least squares,
        indicated by \code{"lm"}. If
        potential outliers are identified according to \code{out_cut},
        a second (dashed) fit line is displayed calculated \emph{without}
        the outliers. Modify the line color from with the 
        \code{\link{style}} function parameter \code{fit_color}, and the
        line width with \code{fit_lwd}.}
  \item{fit_se}{Confidence level for the error band displayed around the
       line of best fit. On by default at 0.95 if a fit line is specified.
       Can be a vector to display multiple ranges. Set to 0 to turn off.}\cr
 
  \item{ellipse}{Confidence level of a \bold{data ellipse} for a scatterplot 
        of only a single
        \code{x}-variable and a single \code{y}-variable according to the
        contours of the corresponding bivariate normal density function. Can
        specify the confidence level(s) for a single or vector of
        numeric values from 0 to 1,
        to plot one or more specified ellipses. For Trellis graphics, only the
        maximum level applies with only one ellipse per panel.
        Modify fill and border colors with the \code{\link{style}} function 
        parameters \code{ellipse_fill} and \code{ellipse_color}.}\cr

  \item{bin}{If \code{TRUE}, display the default frequency distribution
        for the text output of the Violin-Box-Scatter (VBS) Plot, or,
        if \code{values} is set to \code{"count"}, a frequency polygon.}
  \item{bin_start}{Optional specified starting value of the bins for a
        \bold{frequency polygon or for the text output of a
        Violin-Box-Scatter (VBS) Plot}. Also, sets \code{bin} to \code{TRUE}.}
  \item{bin_width}{Optional specified bin width value. Also, sets
        \code{bin} to \code{TRUE}.}
  \item{bin_end}{Optional specified value that is within the last bin, so the
        actual endpoint of the last bin may be larger than the specified value.}
  \item{breaks}{The method for calculating the bins, or an explicit
        specification of the bins, such as with the standard R
        \code{\link{seq}} function or other options provided by the 
        \code{\link{hist}} function.  Also, sets \code{bin} to \code{TRUE}.}
  \item{cumulate}{Specify a cumulative frequency polygon.}\cr
        
  \item{run}{If set to \code{TRUE}, generate a \bold{run chart}, i.e., line chart,
        in which
        points are plotted in the sequential order of occurrence in the data table.
        By default, the points are connected by line
        segments to form a run chart. Set by default when the \code{x}-values
        are sorted with equal intervals or a single variable is a time series.
        Customize the color of the line segments with \code{segments_color}
        with function \code{\link{style}}.}
  \item{lwd}{Width of the line segments_ Set to zero to remove the line
        segments_}       
  \item{area_fill}{Parameter \code{fill} applies to points if only points and
       if only a run chart or time series chart, then to area under
       line segments_ But \code{area_fill} takes precedence over \code{fill}
       for the area under the line segments, which is needed to provide both
       fill for points and this area. Like \code{fill}, the value of \code{"on"}
       sets the default fill color for points.}
  \item{area_origin}{Origin for the filled area under the time series line. Values
       less than this value are below the corresponding reference line, values
       larger are above the line.}
  \item{center_line}{Plots a dashed line through the middle of a run chart.  The two
        possible values for the line are \code{"mean"} and \code{"median"}.
        Provides a center line for the \code{"median"} by default, when the values
        randomly vary about the mean. A value of \code{"zero"} specifies the center
        line should go through zero. Currently does not apply to Trellis plots.}
  \item{show_runs}{If \code{TRUE}, display the individual runs in the run analysis.
        Also, sets \code{run} to \code{TRUE}.}
  \item{stack}{If \code{TRUE}, multiple time plots are stacked on each other, with
       \code{area} set to \code{TRUE} by default.}\cr

  \item{xlab, ylab}{\bold{Axis label} for \code{x}-axis or \code{y}-axis_
       If not specified, then the label becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy_ticks} is \code{FALSE}, no \code{ylab} is displayed.
       Customize these and related parameters with parameters such as
       \code{lab_color} from the \code{\link{style}} function.}
  \item{main}{Label for the title of the graph.  If the corresponding variable
       labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below \code{xlab}.}
  \item{lab_adj}{Two-element vector -- x-axis label, y-axis label -- adjusts
       the position of the axis labels in approximate inches. + values move
       the labels away from plot edge. Not applicable to Trellis graphics.}
  \item{margin_adj}{Four-element vector -- top, right, bottom and left --
       adjusts the margins of the plotted figure in approximate inches.
       + values move the corresponding margin away from plot edge. 
       Not applicable to Trellis graphics.}\cr

  \item{rotate_x}{Degrees that the axis values for the value labels on
        the \code{x}-axis are rotated, usually to accommodate longer values,
        typically used in conjunction with \code{offset}. When equal 90
        the value labels are perpendicular to the x-axis and a different
        algorithm places the labels so that \code{offset} is not needed.}
  \item{rotate_y}{Degrees that the axis values for the value labels on
        the \code{y}-axis are rotated, usually to accommodate longer values,
        typically used in conjunction with \code{offset}.}
  \item{offset}{The amount of spacing between the axis values and the axis_ Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}\cr

  \item{xy_ticks}{Flag that indicates if tick marks and associated \bold{axis values}
        on the axes are to be displayed. To rotate the axis values, use
        \code{rotate_x}, \code{rotate_y}, and \code{offset} from the
        \code{\link{style}} function.}
  \item{value_labels}{Labels for the \code{x}-axis on the graph to override 
        existing data values, including factor levels. If the variable is a 
        factor and \code{value_labels} is not specified (is \code{NULL}), then the
        value_labels are set to the factor levels with each space replaced by
        a new line character. If \code{x} and \code{y}-axes have the same scale,
        they also apply to the \code{y}-axis_}
  \item{label_max}{Maximum size of labels for the values of a categorical variable.
        Not a literal maximum as preserving unique values may require a larger number
        of characters than specified.}
  \item{origin_x}{Origin of \code{x}-axis_ Starting value of \code{x}, by
       default the minimum value of \code{x}, except when \code{stat_x}
       is set to \code{"count"} or related  where the origin is zero
       by default, but can be modified.}
  \item{legend_title}{Title of the legend for a multiple-variable \code{x}
       or \code{y} plot.}
  \item{scale_x}{If specified, a vector of three values that define the
        x-axis with numerical values: starting value, ending value, and number
        of intervals.}
  \item{scale_y}{If specified, a vector of three values that define the
        y-axis with numerical values: starting value, ending value, and number
        of intervals.}\cr

  \item{add}{\bold{Draw one or more objects}, text or a geometric figures,
       on the plot.
       Possible values are any text to be written, the first argument, which is
       \code{"text"}, or,
       \code{"labels"} to label each point with the row name, or, 
       \code{"rect"} (rectangle), \code{"line"}, \code{"arrow"},
       \code{"v_line"} (vertical line), and \code{"h_line"} (horizontal line).
       The value \code{"means"} is short-hand for vertical and horizontal lines
       at the respective means. Does not apply to Trellis graphics.
       Customize with parameters such as \code{add_fill} and \code{add_color}
       from the \code{\link{style}} function.}
  \item{x1}{First x-coordinate to be considered for each object, can be
       \code{"mean_x"}. Not used for \code{"h_line"}.}
  \item{y1}{First y-coordinate to be considered for each object, can be
       \code{"mean_y"}. Not used for\code{"v_line"}.}
  \item{x2}{Second x-coordinate to be considered for each object, can be
       \code{"mean_x"}. Only used for \code{"rect"}, \code{"line"} and
       \code{arrow}.}
  \item{y2}{Second y-coordinate to be considered for each object, can be
       \code{"mean_y"}.  Only used for \code{"rect"}, \code{"line"} and
       \code{arrow}.}\cr

  \item{enhance}{For a two-variable scatterplot, if \code{TRUE},
        automatically add the 0.95 data ellipse, 
        labeling of outliers beyond a Mahalanobis distance of 6 from the 
        ellipse center, the best-fitting least squares line of all the data,
        the best-fitting least squares line of the regular data without the
        outliers, and a horizontal and vertical line to represent the mean of
        each of the two variables.}
  \item{eval_df}{Determines if to check for existing data frame and
        specified variables. By default is \code{TRUE} 
        unless the \code{shiny} package is loaded then set to \code{FALSE} so
        that Shiny will run. Needs to be set to \code{FALSE} if using
        the pipe \code{\%\>\%} notation.}
  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{do_plot}{If \code{TRUE}, the default, then generate the plot.}
  \item{width}{Width of the plot window in inches, defaults to 5 except in RStudio
        to maintain an approximate square plotting area.}
  \item{height}{Height of the plot window in inches, defaults to 4.5 except for
        1-D scatterplots and when in RStudio.}
  \item{pdf_file}{Indicate to direct pdf graphics to the specified name of
        the pdf file.}
  \item{fun_call}{Function call. Used with \code{knitr} to pass the function call when
        obtained from the abbreviated function call \code{sp}.}\cr

  \item{\ldots}{Other parameter values for non-Trellis graphics as defined by and
      processed by standard R functions \code{\link{plot}} and \code{\link{par}},
      including\cr
      \code{xlim} and \code{ylim} for setting the range of the \code{x} and
      \code{y}-axes\cr
      \code{cex.main} for the size of the title\cr
      \code{col.main} for the color of the title\cr
      \code{cex} for the size of the axis value labels\cr
      \code{sub} and \code{col.sub} for a subtitle and its color
  }
}


\details{
VARIABLES and TRELLIS PLOTS\cr
There is at least one primary variable, \code{x}, which defines the coordinate system for plotting in terms of the \code{x}-axis, the horizontal axis_ Plots may also specify a second primary variable, \code{y}, which defines the \code{y}-axis of the coordinate system. One of these primary variables may be a vector. The simplest plot is from the specification of only one or two primary variables, each as a single variable, which generates a single scatterplot of either one or two variables, necessarily on a single plot, called a panel, defined by a single \code{x}-axis and usually a single \code{y}-axis_

For numeric primary variables, a single panel may also contain multiple plots of two types. Form the first type from subsets of observations (rows of data) based on values of a categorical variable. Specify this plot with the \code{by} parameter, which identifies the grouping variable to generate a scatterplot of the primary variables for each of its levels. The points for each group are plotted with a different shape and/or color_ By default, the colors vary, though to maintain the color scheme, if there are only two levels of the grouping variable, the points for one level are filled with the current theme color and the points for the second level are plotted with transparent interiors. 

Or, obtain multiple scatterplots on the same panel with multiple numeric \code{x}-variables, or multiple \code{y}-variables. To obtain this graph, specify one of the primary variables as a vector of multiple variables. 

Trellis graphics, from Deepayan Sarkar's (2009) \code{\link{lattice}} package, may be implemented in which multiple panels for one numeric \code{x}-variable and one numeric \code{y}-variable are displayed according to the levels of one or two categorical variables, called conditioning variables.  A variable specified with \code{by} is a conditioning variable that results in a Trellis plot, the scatterplot of \code{x} and \code{y} produced at \emph{each} level of the \code{by1} variable. The inclusion of a second conditioning variable, \code{by2}, results in a separate scatterplot panel for \emph{each} combination of cross-classified values of both \code{by1} and \code{by2}. A grouping variable according to \code{by} may also be specified, which is then applied to each panel.

Control the panel dimensions and the overall size of the Trellis plot with the following parameters: \code{width} and \code{height} for the physical dimensions of the plot window, \code{n_row} and \code{n_col} for the number of rows and columns of panels, and \code{aspect} for the ratio of the height to the width of each panel. The plot window is the standard graphics window that displays on the screen, or it can be specified as a pdf file with the \code{pdf_file} parameter. 

CATEGORICAL VARIABLES\cr
Conceptually, there are continuous variables and categorical variables. Categorical variables have relatively few unique data values_ However, categorical variables can be defined with non-numeric values, but also with numeric values, such as responses to a five-point Likert scale from Strongly Disagree to Strongly Agree, with responses coded 1 to 5. The three \code{by} --variables -- \code{by1}, \code{by2} and \code{by} -- only apply to graphs created with numeric \code{x} and/or \code{y} variables, continuous or categorical.

The standard and most general way to define a categorical variable is as an R factor, such as created with the lessR \code{\link{factors}} function. \code{lessR} provides the option to define an integer variable with equally spaced values as categorical based on the value of \code{n_cat}, which can be set locally or globally with the \code{\link{style}} function. For example, for a variable with data values from 5-point Likert scale, a value of \code{n_cat} of 5 will define the define the variable as categorical. The default value is 0. To explicitly analyze the values as categorical, set \code{n_cat} to a value larger than 0, at least the size of the number of unique integer values_ Can also annotate a graph of the values of an integer categorical variable with \code{value_labels} option.  

A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 26 possible paired values to plot, so most of the plotted points overlap with others. In this situation, that is, when a single variable or two variables with Likert response scales are specified, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values_ To request a sunflower plot in lieu of the bubble plot, set the \code{shape} to \code{"sunflower"}.

DATA\cr
The default input data frame is \code{d}. Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{d$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment.

The data values themselves can be plotted, or for a single variable, counts or proportions can be plotted on the \code{y}-axis_ For a categorical \code{x}-variable paired with a continuous variable, means and other statistics can be plotted  at each level of the \code{x}-variable. If \code{x} is continuous, it is binned first, with the standard \code{\link{Histogram}} binning parameters available, such as \code{bin_width}, to override default values_ The \code{stat_x} or \code{stat_yx} parameter sets the values to plot, with \code{data} the default. By default, the connecting line segments are provided, so a frequency polygon results. Turn off the lines by setting \code{lwd=0}.
       
The \code{rows} parameter subsets rows (cases) of the input data frame according to a logical expression. Use the standard R operators for logical statements as described in \code{\link{Logic}} such as \code{&} for and, \code{|} for or and \code{!} for not, and use the standard R relational operators as described in \code{\link{Comparison}} such as \code{==} for logical equality \code{!=} for not equals, and \code{>} for greater than. See the Examples.

VALUE LABELS\cr
The value labels for each axis can be over-ridden from their values in the data to user supplied values with the \code{value_labels} option. This option is particularly useful for Likert-style data coded as integers. Then, for example, a 0 in the data can be mapped into a "Strongly Disagree" on the plot. These value labels apply to integer categorical variables, and also to factor variables. To enhance the readability of the labels on the graph, any blanks in a value label translate into a new line in the resulting plot. Blanks are also transformed as such for the labels of factor variables. 

However, the lessR function \code{\link{factors}} allows for the easy creation of factors, one variable or a vector of variables, in a single statement, and is generally recommended as the method for providing value labels for the variables.

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function or \code{\link{VariableLabels}}.  If variable labels exist, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output. 

ONE VARIABLE PLOT\cr
The one variable plot of one continuous variable generates either a violin/box/scatterplot (VBS plot), or a run chart with \code{run=TRUE}, or \code{x} can be an R time series variable for a time series chart. For the box plot,
for gray scale output potential outliers are plotted with squares and outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The default definition of outliers is based on the standard boxplot rule of values more than 1.5 IQR's from the box. The definition of outliers may be adjusted (Hubert and Vandervieren, 2008), such that the whiskers are computed from the medcouple index of skewness (Brys, Hubert, & Struyf, 2004).

The plot can also be obtained as a bubble plot of frequencies for a categorical variable.

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced from a call to the standard R \code{\link{plot}} function. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{type="l"}, which connects each adjacent pair of points with a line segment.

Specifying multiple, continuous \code{x}-variables against a single y variable, or vice versa, results in multiple plots on the same graph. The color of the points of the second variable is the same as that of the first variable, but with a transparent fill_ For more than two \code{x}-variables, multiple colors are displayed, one for each \code{x}-variable.

BUBBLE PLOT FREQUENCY MATRIX (BPFM)\cr
Multiple categorical variables for \code{x} may be specified in the absence of a \code{y} variable. A bubble plot results that illustrates the frequency of each response for each of the variables in a common figure in which the \code{x}-axis contains all of the unique labels for all of the variables plotted. Each line of information, the bubbles and counts for a single variable, replaces the standard bar chart in a more compact display. Usually the most meaningful when each variable in the matrix has the same response categories, that is, levels, such as for a set of shared Likert scales. The BPFM is considerably condensed presentation of frequencies for a set of variables than are the corresponding bar charts.

SCATTERPLOT MATRIX\cr
A single vector of continuous variables specified as \code{x}, with no \code{y}-variable, generates a scatterplot matrix of the specified variable. A continuous variable is defined as a numeric variable with more than n_cat unique responses. To force an item with a small number of unique responses, such as from a 5-pt Likert scale, to be treated as continuous, set \code{n_cat} to a number lower than 5, such as \code{n_cat=0} in the function call. 

The scatterplot matrix is displayed according to the current color theme. Specific colors such as \code{fill}, \code{color}, etc. can also be provided. The upper triangle shows the correlation coefficient, and the lower triangle each corresponding scatterplot, with, by default, the non-linear loess best fit line. The \code{code} fit option can be used to provide the linear least squares line instead, along with the corresponding \code{fit_color} for the color of the fit line.   

SIZE VARIABLE\cr
A variable specified with \code{size=} is a numerical variable that activates a bubble plot in which the size of each bubble is determined by the value of the corresponding value of \code{size}, which can be a variable or a constant.

To explicitly vary the shapes, use \code{shape} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{fill}, such as with R standard color names. If \code{fill} is specified without \code{shape}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable. 

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{ellipse} function, written by Duncan Murdoch and E. D. Chow, from the \code{ellipse} package. The axes are automatically lengthened to provide space for the entire ellipse that extends beyond the maximum and minimum data values_ The specific level of the ellipse can be specified with a numerical value in the form of a proportion. Multiple numerical values of \code{ellipse} may also be specified to obtain multiple ellipses. 

BOXPLOTS\cr
For a single variable the preferred plot is the integrated violin/box/scatter plot or VBS plot. Only the violin or box plot can be obtained with the corresponding aliases \code{\link{ViolinPlot}} and \code{\link{BoxPlot}}, or by setting \code{vbs_plot} to \code{"v"} or \code{"b"}. To view a box plot of a continuous variable (Y) across the levels of a categorical variable (X), either as part of the full VBS plot, or by itself, there are two possibilities:\cr
1. Plot(Y,X) or BoxPlot(Y, X)\cr
2. Plot(Y, by1=X) or BoxPlot(Y, by1=X)\cr
Both styles produce the same information. What differs is the color scheme.

The first possibility places the multiple box plots on a single pane and also, for the default color scheme \code{"colors"}, displays the sequence of box plots with the default qualitative color palette from the lessR function \code{\link{getColors}}.
All colors are displayed at the same level of gray-scale saturation and brightness to avoid perceptual bias. \code{\link{BarChart}} and \code{\link{PieChart}} use the same default colors as well.

The second possibility with \code{by1} produces the different box plots on a separate panel, that is, a Trellis chart. These box plots are displayed with a single hue, the first color, blue, in the default qualitative sequence.

TIME CHARTS\cr
Specifying one or more \code{x}-variables with no \code{y}-variables, and \code{run=TRUE} plots the \code{x}-variables in a run chart. The values of the specified \code{x}-variable are plotted on the \code{y}-axis, with Index on the \code{x}-axis_ Index is the ordinal position of each data value, from 1 to the number of values_ 

If the specified \code{x}-variable is of type \code{Date}, or is a time series, a time series plot is generated for each specified variable. If a formal R time-series, univariate or multivariate, specify as the \code{x}-variable. Or, specify the \code{x}-variable of type Date, and then specify the  \code{y}-variable as one or more time series to plot. The \code{y}-variable can be formatted as tidy data with all the values in a single column, or as wide-formatted data with the time-series variables in separate columns. 

2-D KERNEL DENSITY\cr
With \code{smooth=TRUE}, the R function \code{\link{smoothScatter}} is invoked according to the current color theme. Useful for very large data sets. The \code{smooth_points} parameter plots points from the regions of the lowest density. The \code{smooth_bins} parameter specifies the number of bins in both directions for the density estimation. The \code{smooth_exp} parameter specifies the exponent in the function that maps the density scale to the color scale to allow customization of the intensity of the plotted gradient colors. Higher values result in less color saturation, de-emphasizing points from regions of lessor density. These parameters are respectively passed directly to the \code{\link{smoothScatter}} \code{nrpoints}, \code{nbin} and \code{transformation} parameters. Grid lines are turned off, but can be displayed by setting the \code{grid_color} parameter.

COLORS\cr
A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{style}}. The default color theme is \code{"lightbronze"}. A gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{style}}, such as \code{"sienna"} and \code{"darkred"}. Use the option \code{style(sub_theme="black")} for a black background and partial transparency of plotted colors. 

Colors can also be changed for individual aspects of a scatterplot as well with the \code{\link{style}} function. To provide a warmer tone by slightly enhancing red, try a background color such as \code{panel_fill="snow"}. Obtain a very light gray with \code{panel_fill="gray99"}.  To darken the background gray, try \code{panel_fill="gray97"} or lower numbers. See the \code{lessR} function \code{\link{showColors}}, which provides an example of all available named R colors with their RGB values_

For the color options, such as \code{violin_color}, the value of \code{"off"} is the same as \code{"transparent"}.
\cr

ANNOTATIONS\cr
Use the \code{add} and related parameters to annotate the plot with text and/or geometric figures_ Each object is placed according from one to four corresponding coordinates, the required coordinates to plot that object, as shown in the following table. \code{x}-coordinates may have the value of \code{"mean_x"} and \code{y}-coordinates may have the value of \code{"mean_y"}.\cr

\tabular{lll}{
Value \tab Object \tab Required Coordinates\cr
----------- \tab ------------------- \tab ----------------\cr
\code{"\emph{text}"} \tab text \tab x1, y1\cr
\code{"rect"} \tab rectangle \tab x1, y1, x2, y2\cr
\code{"line"} \tab line segment \tab x1, y1, x2, y2\cr
\code{"arrow"} \tab arrow \tab x1, y1, x2, y2\cr
\code{"v_line"} \tab vertical line  \tab x1\cr
\code{"h_line"} \tab horizontal line  \tab y1\cr
\code{"means"} \tab horiz, vert lines  \tab \cr
----------- \tab ------------------- \tab ----------------\cr
}

The value of \code{add} specifies the object. For a single object, enter a single value. Then specify the value of the needed corresponding coordinates, as specified in the above table. For multiple placements of that object, specify vectors of corresponding coordinates. To annotate multiple objects, specify multiple values for \code{add} as a vector. Then list the corresponding coordinates, for up to each of four coordinates, in the order of the objects listed in \code{add}. 

Can also specify vectors of different properties, such as \code{add_color}. That is, different objects can be different colors, different transparency levels, etc.

PDF OUTPUT\cr
To obtain pdf output, use the \code{pdf_file} option, perhaps with the optional \code{width} and \code{height} options. These files are written to the default working directory, which can be explicitly specified with the R \code{\link{setwd}} function.

ADDITIONAL OPTIONS\cr
Commonly used graphical parameters that are available to the standard R function \code{\link{plot}} are also generally available to \code{\link{Plot}}, such as:

\describe{
\item{cex.main, col.lab, font.sub, etc.}{Settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}
\item{main}{Title of the graph, see \code{\link{title}}.}
\item{xlim}{The limits of the plot on the \code{x}-axis, expressed as c(x1,x2), where \code{x1} and \code{x2} are the limits. Note that \code{x1 > x2} is allowed and leads to a reversed axis_}
\item{ylim}{The limits of the plot on the \code{y}-axis_}
}

ONLY VARIABLES ARE REFERENCED\cr
A referenced variable in a \code{lessR} function can only be a variable name. This referenced variable must exist in either the referenced data frame, such as the default \code{d}, or in the user's workspace, more formally called the global environment. That is, expressions cannot be directly evaluated. For example:

\code{    > Plot(rnorm(50), rnorm(50))   # does NOT work}

Instead, do the following:
\preformatted{    > X <- rnorm(50)   # create vector X in user workspace
    > Y <- rnorm(50)   # create vector Y in user workspace
    > Plot(X,Y)     # directly reference X and Y}
}


\value{
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. The output here is just for the outlier analysis of the two-variable scatterplot with continuous variables. The outlier identification must be activated for the analysis, such as from parameter \code{MD.cut}.

READABLE OUTPUT\cr
code{out_outlier}: Mahalanobis Distance of each outlier.\cr

STATISTICS\cr
code{outliers_indices}: Location of the outliers in the x and y vectors.\cr
}


\references{
Brys, G., Hubert, M., & Struyf, A. (2004). A robust measure of skewness. Journal of Computational and Graphical Statistics, 13(4), 996-1017.

Murdoch, D, and  Chow, E. D. (2013).  \code{ellipse} function from the \code{ellipse} package package. 

Gerbing, D. W. (2014). R Data Analysis without Programming, Chapter 8, NY: Routledge.

Hubert, M. and Vandervieren, E. (2008). An adjusted boxplot for skewed distributions, Computational Statistics and Data Analysis 52, 51865201.

Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R, Springer. http://lmdvr.r-forge.r-project.org/
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{plot}}, \code{\link{stripchart}}, \code{\link{title}}, \code{\link{par}}, \code{\link{loess}}, \code{\link{Correlation}}, \code{\link{style}}.
}


\examples{
# read the data
d <- rd("Employee", quiet=TRUE)
d <- Subset(random=.6, quiet=TRUE)  # less computationally intensive

#---------------------------------------------------
# traditional scatterplot with two numeric variables
#---------------------------------------------------

# scatterplot with all defaults
Plot(Years, Salary)
# or use abbreviation sp in place of Plot
# or use full expression ScatterPlot in place of Plot

# maximum information, minimum input: scatterplot +
#  means, outliers, ellipse, least-squares lines with and w/o outliers
Plot(Years, Salary, enhance=TRUE)

# extend x and y axes
Plot(Years, Salary, scale_x=c(-10, 35, 10), scale_y=c(0,200000,10))


\donttest{

# just males employed more than 5 years
Plot(Years, Salary, rows=(Gender=="M" & Years > 5))

# plot 0.95 data ellipse with the points identified that represent
#   outliers defined by a Mahalanobis Distance larger than 6 
# save outliers into R object out, then remove from d
d[1, "Salary"] <- 200000
out <- Plot(Years, Salary, ellipse=0.95, MD.cut=6)
d <- d[-out$outlier_indices,]

# new shape and point size, no grid or background color
# then put style back to default
style(panel_fill="powderblue", grid_color="off")
Plot(Years, Salary, size=2, shape="diamond")
style()

# translucent data ellipses without points or edges
#  show the idealized joint distribution for bivariate normality
style(ellipse_color="off")
Plot(Years, Salary, size=0, ellipse=seq(.1,.9,.10))
style()

# bubble plot with size determined by the value of Pre
# display the value for the bubbles with values of  min, median and max
Plot(Years, Salary, size=Pre, size_cut=3)

# variables in a data frame not the default d
# plot 0.6 and 0.9 data ellipses with partially transparent points
# change color theme to gold with black background
style("gold", sub_theme="black")
Plot(eruptions, waiting, trans=.5, ellipse=seq(.6,.9), data=faithful)

# scatterplot with two x-variables, plotted against Salary
# define a new style, then back to default
style(window_fill=rgb(247,242,230, maxColorValue=255),
  panel_fill="off", panel_color="off", pt_fill="black", trans=0,
  lab_color="black", axis_text_color="black",
  axis_y_color="off", grid_x_color="off", grid_y_color="black",
  grid_lty="dotted", grid_lwd=1)
Plot(c(Pre, Post), Salary)
style()

# increase span (smoothing) from default of .7 to 1.25
# span is a loess parameter, which generates a caution that can be
#   ignored that it is not a graphical parameter -- we know that
# display confidence intervals about best-fit line at
#   0.95 confidence level
Plot(Years, Salary, fit="loess", span=1.25)

# 2-D kernel density (more useful for larger sample sizes) 
Plot(Years, Salary, smooth=TRUE)
}

#------------------------------------------------------
# scatterplot matrix from a vector of numeric variables
#------------------------------------------------------

# with least squares fit line
Plot(c(Salary, Years, Pre), fit="lm")


#--------------------------------------------------------------
# Trellis graphics and by for groups with two numeric variables
#--------------------------------------------------------------

# Trellis plot with condition on 1-variable
# optionally re-order default alphabetical R ordering by converting
#   to a factor with lessR factors (which also does multiple variables
d$Gender <- factors(Gender, levels=c("M", "F"))
Plot(Years, Salary, by1=Gender)

\donttest{

# all three by variables
Plot(Years, Salary, by1=Dept, by2=Gender, by=Plan)

# vary both shape and color with a least-squares fit line for each group
style(color=c("darkgreen", "brown"))
Plot(Years, Salary, by1=Gender, fit="lm", shape=c("F","M"), size=.8)
style("gray")

# compare the men and women Salary according to Years worked
#   with an ellipse for each group
Plot(Years, Salary, by=Gender, ellipse=.50)
}

#--------------------------------------------------
# analysis of a single numeric variable (or vector)
#--------------------------------------------------

# One continuous variable
# -----------------------
# integrated Violin/Box/Scatterplot, a VBS plot
Plot(Salary)

\donttest{

# by variable, different colors for different values of the variable
# all on one panel
Plot(Salary, by=Dept)

# large sample size
x <- rnorm(10000)
Plot(x)

# custom colors for outliers, which might not appear in this subset data
style(out_fill="hotpink", out2_fill="purple")
Plot(Salary)
style()

# no violin plot or scatterplot, just a boxplot
Plot(Salary, vbs_plot="b")
# or, the same with the mnemonic
BoxPlot(Salary)

# two related displays of box plots for different levels of a
#   categorical variable
BoxPlot(Salary, by1=Dept)


# binned values to plot counts
# ----------------------------
# bin the values of Salary to plot counts as a frequency polygon
# the counts are plotted as points instead of the data
Plot(Salary, stat_x="count")  # bin the values

# time charts
#------------
# run chart, with default fill area
Plot(Salary, run=TRUE, area_fill="on")

# two run charts in same plot
# or could do a multivariate time series
Plot(c(Pre, Post), run=TRUE)

# Trellis graphics run chart with custom line width, no points
Plot(Salary, run=TRUE, by1=Gender, lwd=3, size=0)

# daily time series plot
# create the daily time series from R built-in data set airquality
oz.ts <- ts(airquality$Ozone, start=c(1973, 121), frequency=365)
Plot(oz.ts)

# multiple time series plotted from dates and stacked
# black background with translucent areas, then reset theme to default
style(sub_theme="black", color="steelblue2", trans=.55, 
  window_fill="gray10", grid_color="gray25")
date <- seq(as.Date("2013/1/1"), as.Date("2016/1/1"), by="quarter")
x1 <- rnorm(13, 100, 15)
x2 <- rnorm(13, 100, 15)
x3 <- rnorm(13, 100, 15)
df <- data.frame(date, x1, x2, x3)
rm(date); rm(x1); rm(x2); rm(x3)
Plot(date, x1:x3, data=df)
style()

# trigger a time series with a Date variable specified first
# stock prices for three companies by month:  Apple, IBM, Intel
d <- rd("StockPrice")
# only plot Apple
Plot(date, Price, rows=(Company=="Apple"))
# Trellis plots, one for each company
Plot(date, Price, by1=Company, n_col=1)
# all three plots on the same panel, three shades of blue
Plot(date, Price, by=Company, color="blues")
}

#------------------------------------------
# analysis of a single categorical variable
#------------------------------------------
d <- rd("Employee")

# default 1-D bubble plot
# frequency plot, replaces bar chart 
Plot(Dept)

\donttest{

# abbreviated category labels
Plot(Dept, label_max=2)

# plot of frequencies for each category (level), replaces bar chart 
Plot(Dept, stat_x="count")


#----------------------------------------------------
# scatterplot of numeric against categorical variable 
#----------------------------------------------------

# generate a chart with the plotted mean of each level
# rotate x-axis labels and then offset from the axis
style(rotate_x=45, offset=1)
Plot(Dept, Salary)
style()


#-------------------
# Cleveland dot plot 
#-------------------

# row.names on the y-axis
Plot(Salary, row.names)

# standard scatterplot
Plot(Salary, row.names, sort_yx="0", segments_y=FALSE)

# Cleveland dot plot with two x-variables
Plot(c(Pre, Post), row.names)


#------------
# annotations
#------------

# add text at the one location specified by x1 and x2
Plot(Years, Salary, add="Hi There", x1=12, y1=80000)
# add text at three different specified locations 
Plot(Years, Salary, add="Hi", x1=c(12, 16, 18), y1=c(80000, 100000, 60000))

# add three different text blocks at three different specified locations
Plot(Years, Salary, add=c("Hi", "Bye", "Wow"), x1=c(12, 16, 18),
  y1=c(80000, 100000, 60000))

# add an 0.95 data ellipse and horizontal and vertical lines through the
#  respective means
Plot(Years, Salary, ellipse=0.95, add=c("v_line", "h_line"),
  x1="mean_x", y1="mean_y")
# can be done also with the following short-hand
Plot(Years, Salary, ellipse=0.95, add=c("means"))
 
# a rectangle requires two points, four coordinates, <x1,y1> and <x2,y2>
style(add_trans=.8, add_fill="gold", add_color="gold4", add_lwd=0.5)
Plot(Years, Salary, add="rect", x1=12, y1=80000, x2=16, y2=115000)

# the first object, a rectangle, requires all four coordinates
# the vertical line at x=2 requires only an x1 coordinate, listed 2nd 
Plot(Years, Salary, add=c("rect", "v_line"), x1=c(10, 2),
  y1=80000, x2=12, y2=115000)

# two different rectangles with different locations, fill colors and translucence
style(add_fill=c("gold3", "green"), add_trans=c(.8,.4))
Plot(Years, Salary, add=c("rect", "rect"), 
  x1=c(10, 2), y1=c(60000, 45000), x2=c(12, 75000), y2=c(80000, 55000))
}

#----------------------------------------------------
# analysis of two categorical variables (Likert data)
#----------------------------------------------------

d <- rd("Mach4", quiet=TRUE)  # Likert data, 0 to 5

# size of each plotted point (bubble) depends on its joint frequency
# triggered by default when replication of joint values and
#   less than 9 unique data values for each 
# n_cat=6 means treat responses as categorical for up to 6 equally-spaced
#   integer values
Plot(m06, m07, n_cat=6)

# use value labels for the integer values, modify color options
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
   "Slightly Agree", "Agree", "Strongly Agree")
style(fill="powderblue", color="blue", bubble_text="darkred")
Plot(m06,  m07, value_labels=LikertCats, n_cat=6)
style()  # reset theme

\donttest{

# proportions within each level of the other variable
Plot(m06, m07, stat_x="proportion", n_cat=6)

# get correlation analysis instead of cross-tab analysis
# rely upon the default value of n_cat=0 so that integer
#   valued variables are analyzed as numerical
Plot(m06, m07)

#-----------------------------
# Bubble Plot Frequency Matrix
#-----------------------------
# applies to categorical variables, since Mach IV Likert items
#   are 0 to 5 integer values, set n_cat to indicate the
#   numeric values represent categories
Plot(c(m06,m07,m09,m10), value_labels=LikertCats, n_cat=6)


#---------------
# function curve
#---------------

x <- seq(10,50,by=2) 
y1 <- sqrt(x)
y2 <- x**.33
# x is sorted with equal intervals so run chart by default
Plot(x, y1)

# multiple plots from variable vectors need to have the variables
#  in a data frame
d <- data.frame(x, y1, y2)
# if variables are in the user workspace and in a data frame
#   with the same names, the user workspace versions are used,
#   which do not work with vectors of variables, so remove
rm(x); rm(y1); rm(y2)
Plot(x, c(y1, y2))


#-----------
# modern art
#-----------
clr <- colors()  # get list of color names
color0 <- clr[sample(1:length(clr), size=1)]
clr <- clr[-(153:353)]  # get rid of most of the grays

n <- sample(5:30, size=1)
x <- rnorm(n)
y <- rnorm(n)
color1 <- clr[sample(1:length(clr), size=1)]
color2 <- clr[sample(1:length(clr), size=1)]

style(window_fill=color0, color=color2)
Plot(x, y, run=TRUE, area_fill="on",
  xy_ticks=FALSE, main="Modern Art", xlab="", ylab="",
  cex.main=2, col.main="lightsteelblue", n_cat=0, center_line="off")
style() # reset style to default
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }


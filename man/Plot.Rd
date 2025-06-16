\name{Plot}
\alias{Plot}
\alias{ScatterPlot}
\alias{sp}
\alias{BoxPlot}
\alias{bx}
\alias{ViolinPlot}
\alias{vp}

\title{Scatterplots including Time Series and Violin/Box/Scatterplot}

\description{
Abbreviation:\cr
\verb{  }Violin Plot only: \code{vp}, \code{ViolinPlot}\cr
\verb{  }Box Plot only: \code{bx}, \code{BoxPlot}\cr
\verb{  }Scatter Plot only: \code{sp}, \code{ScatterPlot}\cr

A scatterplot displays the values of a distribution, or the relationship between the two distributions, in terms of their joint values, as a set of points in an \emph{n}-dimensional coordinate system, in which the coordinates of each point are the values of \emph{n} variables for a single observation (row of data). From the identical syntax, from any combination of continuous or categorical variables variables \code{x} and \code{y}, \code{Plot(x)} or \code{Plot(x,y)}, where \code{x} or \code{y} can be a vector, by default generates a family of related 1- or 2-dimensional scatterplots, possibly enhanced, plus related statistical analyses. Define a categorical variable as an R factor. If \code{x} is a Date variable, then a time series is plotted.

\code{Plot()} produces a wide variety of scatterplots as outlined in the following list.


\tabular{ll}{
\strong{Variable Type}     \tab  \strong{Meaning}\cr
-------------------------- \tab --------------------------------------\cr
\code{x}, \code{y}, or \code{z} \tab single continuous variable\cr
\code{xDate} \tab date variable, defined as a R Date type, which can be implicitly created from entered numeric dates\cr
\code{xCat}, \code{yCat}, or \code{zCat} \tab categorical variable, typically defined as an R factor\cr
\code{xUnique} or \code{yUnique} \tab categorical variable with all values unique\cr
\code{X} or \code{Y} \tab vector of continuous variables\cr
\code{Xcat} \tab vector of categorical variables\cr
-------------------------- \tab --------------------------------------
}

\strong{Two variables}\cr
\code{Plot(x,y)}: traditional scatterplot of two continuous variables\cr
\code{Plot(xDate,y)}:  a variable of type Date paired with a continuous variable yields a time-series plot\cr
\code{Plot(xCat,yCat)}: to solve the over-plot problem, plot a scatterplot of two categorical variables as a bubble scatterplot, the size of each bubble based on the corresponding joint frequency\cr
\code{Plot(xCat,y)} or \code{Plot(x,yCat)}: one variable categorical and the other variable continuous, yields a scatterplot with means at each level of the categorical variable\cr
\code{Plot(xCat,y, stat="mean")} or \code{Plot(x,yCat, stat="mean")}: one variable categorical and the other variable continuous, yields a Cleveland dot plot with a specified statistic such as the \code{"mean"} of the continuous variable at each level of the categorical variable\cr
\code{Plot(xUnique,y)} or \code{Plot(x,yUnique)}: one categorical with unique (ID) values and the other variable continuous, yields a Cleveland dot (lollipop) plot, where the unique values can be variable \code{row.names}\cr

\strong{One variable}\cr
\code{Plot(x)}: one continuous variable generates either a violin/box/scatterplot
    (VBS plot), named here, or a run chart, generated from the \code{x}-variable named
    \code{.Index}. Or \code{x} can be an R time series object created with
    \code{ts()} for a time series visualization\cr
\code{Plot(xCat)}: one categorical variable yields a 1-dimensional bubble plot to
    solve the over-plot problem for a more compact replacement of the traditional
    bar chart\cr

\strong{Three, four, or more variables}\cr
\code{Plot(x,y, size=z)}: \code{x} and \code{y} continuous yields a bubble of two continuous variables with \code{z} setting the size of the corresponding plotted point, i.e., bubble\cr
\code{Plot(x,y, by=zCat)}: plots a different scatterplot of \code{x} and \code{y} for each level of \code{zCat} on the same panel\cr
\code{Plot(x,y, facet1=zCat)}: plots a different scatterplot of \code{x} and \code{y} for each level of \code{zCat} on separate panels, i.e., Trellis or facet plots\cr
\code{Plot(x,y, facet1=z1Cat, facet2=z2Cat)}: plots a different scatterplot of \code{x} and \code{y} for each combination of levels of \code{zCat1} and \code{zCat2} on separate panels, i.e., Trellis or facet plots\cr
\code{Plot(X,y)} or \code{Plot(x,Y)}: one vector variable of several continuous variables, paired with another single continuous variable, yields multiple scatterplots on the same graph\cr
\code{Plot(Y,xUnique)}: one categorical with unique (ID) values, such as \code{row.names} and the other variable a vector of continuous variables yields a Cleveland dot plot of all the continuous variables, usually two\cr

\strong{One vector of variables}\cr
\code{Plot(X)}: one vector of numerical variables, with no \code{y}-variable, results in a scatterplot matrix of the variables\cr
\code{Plot(Xcat)}: one vector of categorical \code{x}-variables coded as character or factor variables, with no \code{y}-variable, generalizes to a matrix of 1-dimensional bubble plots, here called the bubble plot frequency matrix, to replace a series of bar charts
}


\usage{
Plot(

    # -------------------------------------------------------
    # Data from which to construct the plot for x- and y-axis
    x, y=NULL, data=d, filter=NULL,


    # -------------------------------
    # Enhancements and customizations
    # -------------------------------

    # --------------------------------------------------
    # Stratification: Same panel or Trellis (facet) plot [x, or x and y]
    by=NULL, facet1=NULL, facet2=NULL,
    n_row=NULL, n_col=NULL, aspect="fill",

    # --------------------------------------------------------------------
    # Analogy of physical Marks on paper that create the points and labels
    # See  ?style  for more options with the  style()  function
    theme=getOption("theme"),
    fill=NULL, color=NULL,
    transparency=getOption("trans_pt_fill"),

    enhance=FALSE, means=TRUE,
    size=NULL, size_cut=NULL, shape="circle", line_width=1.5,
    segments=FALSE, segments_y=FALSE, segments_x=FALSE,

    # ----------------------
    # Sort and jitter points
    sort=c("0", "-", "+"),
    jitter_x=NULL, jitter_y=NULL,

    # ----------------
    # Outlier analysis
    ID="row.name", ID_size=0.60,
    MD_cut=0, out_cut=0, out_shape="circle", out_size=1,

    # -------------------------------------------------
    # Fit line, confidence interval, confidence ellipse
    fit=c("off", "loess", "lm", "ls", "null", "exp", "quad",
          "power", "log"),
    fit_power=1, fit_se=0.95,
    fit_color=getOption("fit_color"), fit_new=NULL, 
    plot_errors=FALSE, ellipse=0,


    # ----------------------------------------------------------
    # Types of plots
    # ----------------------------------------------------------

    # -----------------------------------------------------------------------
    # Time series and forecasting, plot x values sequentially [xDate, y or Y]
    ts_unit=NULL, ts_agg=c("sum", "mean"), ts_NA=NULL,
    ts_ahead=0, ts_method=c("es", "lm"), ts_format=NULL,
    ts_fitted=FALSE, ts_level=NULL, ts_trend=NULL, ts_seasons=NULL,
    ts_type=c("additive", "multiplicative"), ts_PI=0.95,
    stack=FALSE, area_fill="transparent", area_split=0, n_date_tics=NULL,
    # Run chart (indicate with .Index for the name of the x-variable)
    show_runs=FALSE, center_line=c("off", "mean", "median", "zero"),

    # -----------------------------------
    # Lollipop chart from aggregated data [xCategorical and y]
    stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
    stat_x=c("count", "proportion", "\%"),

    # ----------------------------------
    # Integrated violin/box/scatter plot  [x only]
    vbs_plot="vbs", vbs_ratio=0.9, bw=NULL, bw_iter=10,
    violin_fill=getOption("violin_fill"),
    box_fill=getOption("box_fill"),
    vbs_pt_fill="black",
    vbs_mean=FALSE, fences=FALSE, n_min_pivot=1,
    k=1.5, box_adj=FALSE, a=-4, b=3,

    # -----------
    # Bubble plot [xCategorical, or xCategorical and yCategorical]
    radius=NULL, power=0.5, low_fill=NULL, hi_fill=NULL,

    # -----------------------------------------------------------
    # Large data sets, smoothing, contours and binning  [x and y]
    type=c("regular", "smooth", "contour"),
    smooth_points=100, smooth_size=1,
    smooth_power=0.25, smooth_bins=128, n_bins=1,
    contour_n=10, contour_nbins=50, contour_points=FALSE,

    # ------------------------------------------------------
    # Bins for frequency polygon or text output of VBS plots
    bin=FALSE, bin_start=NULL, bin_width=NULL, bin_end=NULL,
    breaks="Sturges", cumulate=FALSE,


    # ----------------------
    # Axes labels and values
    # ----------------------

    # -----------------------
    # Axis labels and spacing
    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    label_adjust=c(0,0), margin_adjust=c(0,0,0,0),  # top, right, bottom, left
    pad_x=c(0,0), pad_y=c(0,0),

    # ---------------------
    # Axis values specified
    scale_x=NULL, scale_y=NULL, origin_x=NULL, origin_y=NULL,

    # ---------------------
    # Axis values formatted
    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),
    axis_fmt=c("K", ",", ".", ""), axis_x_prefix="", axis_y_prefix="",
    xy_ticks=TRUE, n_axis_x_skip=0, n_axis_y_skip=0, 

    # ------
    # legend
    legend_title=NULL,


    # -------------
    # Miscellaneous
    # -------------

    # ----------------------------------------------------
    # Add one or more objects, text, or geometric figures
    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    # ---------------------------------------------
    # Output: turn off, to PDF file, decimal digits
    quiet=getOption("quiet"), do_plot=TRUE,
    pdf_file=NULL, width=6.5, height=6,
    digits_d=NULL,

    # -------------------------------------------------------------
    # Deprecated, to be removed in future versions
    n_cat=getOption("n_cat"), value_labels=NULL,  # use R factors instead
    rows=NULL, by1=NULL, by2=NULL, smooth=FALSE,

    # -----
    # Other
    eval_df=NULL, fun_call=NULL, \dots)


ScatterPlot(\dots)
sp(\dots)
BoxPlot(\dots)
bx(\dots)
ViolinPlot(\dots)
vp(\dots)
}

\arguments{
  \item{x}{By itself, or with \code{y}, by default, a \emph{primary variable},
        that is, plotted by its values mapped to coordinates.
        The \bold{data values} can be
        continuous or categorical, cross-sectional or a time series.
        If \code{x} is sorted, with equal intervals
        separating the values, or is a time series, then by default
        plots the points sequentially, joined by line segments. If named
        \code{.Index}, then a run chart is generated from the corresponding
        \code{y} variable.
        Can specify multiple \code{x}-variables or multiple \code{y}-variables
        as vectors, but not both. Can be in a data frame or defined
        in the global environment.}
  \item{y}{An optional second \emph{primary variable.} Variable with values
        to be mapped to coordinates of points in
        the plot on the vertical axis. Can be continuous or categorical.
        Can be in a data frame or defined in the global environment.}
  \item{data}{Optional data frame that contains one or both of \code{x} and
        \code{y}. Default data frame is \code{d}.}
  \item{filter}{A logical expression or a vector of integers that specify the
        row numbers of the rows to retain to define a subset of rows of
        the data frame to analyze.}\cr


  \item{by}{A categorical variable to provide a scatterplot for
        each level of the numeric primary variables \code{x} and
        \code{y} on the \emph{same} plot, a \emph{grouping variable}.
        For two-variable plots, applies to the panels of a
        \bold{Trellis graphic} if \code{facet1} is specified.}
  \item{facet1}{A categorical variable, the \emph{conditioning variable},
        which activates Trellis (facet) graphics, provided by Deepayan
        Sarkar's (2007) lattice package, to provide
        a \emph{separate} panel of numeric primary variables \code{x}
        and \code{y} for each level of the \code{facet1} variable.
        Re-order the levels by first converting to a factor variable with
        \code{\link{factor}} or lessR \code{\link{factors}}.}
  \item{facet2}{A second \emph{conditioning variable} to generate Trellis (facet)
        plots jointly conditioned on both the \code{facet1} and \code{facet2}
        variables, with \code{facet2} as the row variable, which yields a
        scatterplot (panel) for \emph{each} cross-classification of the levels
        of numeric \code{x} and \code{y} variables.}
  \item{n_row}{Optional specification for the number of rows 
        in the layout of a multi-panel display with Trellis graphics. Specify
        \code{n_col} or \code{n_row}, but not both.}
  \item{n_col}{Optional specification for the number of columns in the
        layout of a multi-panel display with
        Trellis (facet) graphics. Specify \code{n_col} or \code{n_row}, but
        not both. The default sets to 1.}
  \item{aspect}{Lattice parameter for the aspect ratio of the Trellis panels
        or facets, defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to 1 for square panels.
        Set to \code{"xy"} to specify a ratio calculated
        to display at 45 degrees, that is, with the line slope approximately
        45 degrees.}\cr


  \item{theme}{Color theme for this analysis. Make persistent across analyses
        with \code{\link{style}}.}
  \item{fill}{Either fill color of the points or the area under a line chart.
        Can also set with the lessR function \code{\link{getColors}} to
        select from a variety of color palettes. For points, default is
        \code{pt_fill} and for area under a line chart, \code{violin_fill}.
        For a line chart, set to \code{"on"} for default color.}
  \item{color}{Border color of the points or line_color for line plot.
        Can be a vector to customize the color for each point or a color
        range such as "blues" (see \code{\link{getColors}}. Default is
        \code{pt_color} from the lessR \code{\link{style}} function.}
  \item{transparency}{Transparency factor of the fill color of each point.
        Default is
        \code{trans_pt_fill} from the lessR \code{\link{style}} function.}\cr


  \item{enhance}{For a two-variable scatterplot, if \code{TRUE},
        automatically add the 0.95 data ellipse,
        labeling of outliers beyond a Mahalanobis distance of 6 from the
        ellipse center, the best-fitting least squares line of all the data,
        the best-fitting least squares line of the regular data without the
        outliers, and a horizontal and vertical line to represent the mean of
        each of the two variables.}
  \item{means}{If the one variable is categorical, expressed as a factor, and
       the other variable continuous, then if \code{TRUE}, by default,
       plot means with the scatterplot. Also applies to a 1-D scatterplot.}
  \item{size}{When set to a constant, the scaling factor for \bold{standard points}
      (not bubbles) or a line, with default of 1.0 for points and 2.0 for a line.
       Set to 0 to not plot the points or lines. If \code{area_fill} for a line
       chart, then default is 0. When set to a variable, activates a
       bubble plot with the size of each bubble further determined
       by the value of \code{radius}. Applies to the standard two-variable
       scatterplot as well as to the scatterplot component of the
       integrated Violin-Box-Scatterplot (VBS) of a single continuous variable.}
  \item{size_cut}{If \code{1} (or \code{TRUE}), then for a bubble plot in which the
       bubble sizes are defined by a
       \code{size} variable, show the value
       of the sizing variable for selected bubbles in the center of
       the bubbles, unless the bubble is too small.
       If \code{0} (or \code{FALSE}), no value is displayed.
       If a number greater than 1, then display the value only for the
       indicated number of values, such as just the max and min for a setting
       of 2, the default value when bubbles represent a size
       variable.  Color of the displayed text set by \code{bubble_text} from
       the \code{\link{style}} function.}
  \item{shape}{The plot character(s). The default value is \code{"circle"}
       with both a default exterior
       color and filled interior, explicitly specified with
       \code{"color"} and \code{"fill"}.
       Other possible values, with fillable interiors,
       are \code{"circle"}, \code{"square"}, \code{"diamond"},
       \code{"triup"} (triangle up), and \code{"tridown"} (triangle down), all
       uppercase and lowercase letters, all digits, and most punctuation characters.
       The numbers 0 through 25 as defined by the R \code{\link{points}} function
       also apply. If plotting levels for different groups
       according to \code{by}, then list as a vector with with one shape for each
       level to be plotted or set to \code{"vary"} to have shapes selected by
       default across the \code{by} groups.}
  \item{line_width}{Width of the line segments that connect adjacent points,
        such as plotting time series data. Set to zero to remove the line segments.}
  \item{segments}{Designed for interaction plots of means, connects each pair of
        successive points with a line segment. Pass a data frame of the means,
        such as from \code{\link{pivot}}. To turn off connecting line segments
        for sorted, equal intervals data, set to \code{FALSE}. Currently, does
        not apply to Trellis plots.}
  \item{segments_y}{For one \code{x}-variable, draw a line segment from the
        \code{y}-axis to
        each plotted point, such as for the Cleveland dot plot. For two
        \code{x}-variables, the line segments connect the two points.}
  \item{segments_x}{Draw a line segment from the \code{x}-axis for each
        plotted point.}\cr


  \item{sort}{Sort the values of \code{y} by the values of \code{x}, such as
        for a Cleveland dot plot, that is, a numeric \code{x}-variable paired
        with a categorical \code{y}-variable with unique values. If a \code{x}
        is a vector of two variables, sort by their difference.}
  \item{jitter_x}{Randomly perturbs the plotted points of
       a scatterplot
       horizontally within the limits of the explicitly specified value, or
       set to \code{NULL} to rely upon the computed default value.}
  \item{jitter_y}{Defaults to 0. Same as \code{jitter_x} except
        vertical jitter.}\cr


  \item{ID}{Name of variable to provide the \bold{labels for the selected
       plotted points for outlier identification}, row names of data frame
       by default. To label all
       the points use the \code{add} parameter described later.}
  \item{ID_size}{Size of the plotted labels.
        Modify text color of the labels with the \code{\link{style}} function
        parameter \code{ID_color}.}
  \item{MD_cut}{Mahalanobis distance cutoff to define an outlier in a 2-variable
       scatterplot.}
  \item{out_cut}{Count or proportion of plotted points to label, in order of their
       distance from the center (means) of the univariate distribution or 
       scatterplot, counting down from
       the more extreme point. For two-variable plots, assess distance
       from the center with Mahalanobis distance. For box plots or
       VBS plots of a single
       continuous variable, refers to outliers on each side of the plot, 
       and any value greater than the number of outliers determined by
       the box plot is ignored.}
  \item{out_shape}{Shape of outlier points in a 2-variable scatterplot
        or a VBS plot.
        Modify fill color from the current \code{theme} with the
        \code{\link{style}} function parameters \code{out_fill} and
        \code{out2_fill}.}
  \item{out_size}{Size of outlier points in a 2-variable scatterplot
        or VBS plot.}\cr

  \item{fit}{The \bold{best fit line}. Default value is \code{"off"}, with
        options \code{"loess"} for non-linear fit, \code{"lm"} for linear model
        least squares, \code{"null"} for the null model, \code{"exp"} for
        exponential growth or decay, \code{"power"} for the general
        power model in conjunction with \code{fit_power}, and
        \code{"quad"} for an increasing or decreasing function for
        the specific power value of 2. If
        potential outliers are identified according to \code{out_cut},
        a second (dashed) fit line is displayed calculated \emph{without}
        the outliers.}
  \item{fit_power}{Power that describes response Y as a power function of the
       predictor variable X, required for the value of \code{fit} of \code{"power"}.
       Optionally, and experimentally, applies to \code{fit} values \code{"exp"}.}
  \item{fit_se}{Confidence level for the error band displayed around the
       line of best fit. On by default at 0.95 if a fit line is specified,
       but turned off if \code{plot_errors=TRUE}.
       Can be a vector to display multiple ranges. Set to 0 to turn off.}
  \item{fit_color}{Color of the fit line.}
  \item{fit_new}{When parameter \code{fit} is set to a fit curve such as
       \code{"lm"} or \code{"quad"}, then predicted values from the model
       are predicted for these specified values.}
  \item{plot_errors}{Plot the line segment that joins each point to the
        regression line, "loess" or "lm", illustrating the size of the
        residuals.}
  \item{ellipse}{Confidence level of a data ellipse for a scatterplot
        of only a single
        \code{x}-variable and a single \code{y}-variable according to the
        contours of the corresponding bivariate normal density function. Can
        specify the confidence level(s) for a single or vector of
        numeric values from 0 to 1,
        to plot one or more specified ellipses. For Trellis graphics, only the
        maximum level applies with only one ellipse per panel.
        Modify fill and border colors with the \code{\link{style}} function
        parameters \code{ellipse_fill} and \code{ellipse_color}.}\cr


  \item{ts_unit}{Specify the time unit from which to plot 
       \bold{time series data},
        plotted when the \code{x}-variable is of type \code{Date}.
        Default value is the time unit that describes the time intervals as
        they occur in the data.
        Aggregation according to the time unit will occur as specified, such as
        a daily time series aggregated to \code{"months"}. Dates are currently
        stored as variable type \code{Date()} which stores information as
        calendar dates without times of the day.
        Valid values include: \code{"days"}, \code{"weeks"},
        \code{"months"}, \code{"quarters"}, and \code{"years"}, as well as 
        \code{"days7"} to provide seasonality for daily data on a weekly instead
        of annual basis.
        Otherwise, for forecasting, the time unit for detecting seasonality
        will usually be `"months"` or `"quarters"`. }
  \item{ts_agg}{Function by which to aggregate over time according to
        \code{ts_unit}. Default is \code{"sum"} with an option for
        \code{"means"}.}
  \item{ts_NA}{By default, \code{y} missing values, those with value \code{NA},
    do not plot, leaving a blank space. Or, specify a value, usually 0,
    to replace the \code{NA} to plot a y-value such as 0 for the corresponding
    date on the x-axis. However, forecasting with missing data does not work.}
  \item{ts_ahead}{Forecast this specified number of \code{ts_units} ahead
        of the last time period in the time series data.}
  \item{ts_method}{Default is \code{"es"} for exponential smoothing forecasting.
       Or, choose \code{"lm"} for at least squares linear regression model.}
  \item{ts_format}{A specified format for R function \code{as.Date()}
    that describes the values of the date variable on the x-axis,
    needed if the function cannot identify the date format to properly
    decode the given date values. For example, describe a character string
    date such as \code{"09/01/2024"} by the format \code{"\%m/\%d/\%Y"}.
    See \code{details} for more information.}
  \item{ts_fitted}{If \code{TRUE}, for each data value display the fitted value,
        ts_level, trend, and seasonal component.}
  \item{ts_level}{Holt-Winters exponential smoothing level parameter,
       \code{alpha}. By default, the algorithm chooses an optimal numerical
       value from 0 to 1, or user specify.}
  \item{ts_trend}{Trend parameter. If \code{FALSE}, then no trend in the
       model estimation. For Holt-Winters exponential smoothing, the trend parameter,
       \code{beta}. By default, the algorithm chooses an optimal numerical
       value from 0 to 1, or user specify.}
  \item{ts_seasons}{The seasonality parameter, which applies to both
      exponential smoothing, \code{gamma}, and de-seasonalized
      regression forecasting.
      For exponential smoothing, by default, the algorithm chooses an optimal
      numerical value from 0 to 1, or user specify. Or set to \code{FALSE}
      to remove the effect. Defaults to \code{FALSE} for annual data, which
      cannot exhibit intra-annual seasonality. For linear
      regression time series forecasting, set to \code{FALSE} to not
      de-seasonalize.}
  \item{ts_type}{Type of seasonal model for exponential smoothing forecasting.
      Default is \code{"additive"} with a \code{"multiplicative"} option.}
  \item{ts_PI}{Level of the prediction interval about the forecasted
       Holt-Winters values with a default of \code{0.95}.}
  \item{stack}{If \code{TRUE}, multiple time plots are stacked on each other, with
       \code{area} set to \code{TRUE} by default.}
  \item{area_fill}{Specifies the area under the line segments, if present.
       If \code{stack} is \code{TRUE}, then
       default is gradation from default color range, e.g., \code{"blues"}.
       If not specified, and \code{fill} is specified with no plotted points
       and \code{area_fill} is not specified,,
       then \code{fill} generally specifies the area under the line segments.}
  \item{area_split}{[Applies only to a Trellis plot activated with parameter
       \code{facet1}.] Value of \code{y} that defines a reference line that splits
       the filled area under the time series line. Values of \code{y}
       less than this value are below the corresponding reference line, values
       larger are above the line.}
  \item{n_date_tics}{Suggested number of ticks for the dates on the \code{x}-axis 
       to override the default of approximately 7.}
  \item{show_runs}{If \code{TRUE}, display the individual runs in the run analysis.
        Also, sets \code{run} to \code{TRUE}. Customize the color of the line
        segments with \code{segments_color} with function \code{\link{style}}.}
  \item{center_line}{Plots a dashed line through the middle of a run chart.
        Provides a center line for the \code{"median"} by default, when the values
        randomly vary about the mean. \code{"mean"} and \code{"zero"} specify that
        the center line goes through the mean or zero, respectively.
        Currently does not apply to Trellis plots.} \cr


  \item{stat}{Apply \bold{specified aggregation} such as \code{"mean"} for the
       numerical \code{y} variable to each of the levels of categorical
       variable \code{x}.
       The resulting dot plot, or Cleveland plot, is analogous to a bar chart.}
  \item{stat_x}{If no \code{y} variable is specified, for constructing a
        frequency polygon, with access to the \code{bin_width} parameter.
        Either do the
        default \code{count} for each bin or the \code{proportion}, also
        indicated by \code{\%}.}\cr


  \item{vbs_plot}{A character string that specifies the components of the
        \bold{integrated Violin-Box-Scatterplot (VBS) of a continuous variable}.
        A \code{"v"} in the string indicates a violin plot, a \code{"b"}
        indicates a box plot with flagged outliers, and a \code{"s"}
        indicates a 1-variable scatterplot. Default value is \code{"vbs"}.
        The characters can be in any order and upper- or lower-case.
        Generalize to Trellis plots with the
        \code{facet1} and \code{facet2} parameters, but currently only applies
        to horizontal displays.
        Modify fill and border colors from the current \code{theme} with
        the \code{\link{style}} function parameters \code{violin_fill},
        \code{violin_color}, \code{box_fill} and \code{box_color}.}
  \item{vbs_ratio}{Height of the violin plot relative to the plot area. Make the
        violin (and also the accompanying box plot) larger or smaller by
        making the plot area and/or this value larger or smaller.}
  \item{bw}{Bandwidth for the smoothness of the violin plot. Higher values
        for smoother plots. Default is to calculate a bandwidth that provides
        a relative smooth density plot.}
  \item{bw_iter}{Number of iterations used to modify default R bandwidth
        to further smooth the obtained density estimate. When set, also
        displays the iterations and corresponding results.}
  \item{violin_fill}{Fill color for a violin plot.}
  \item{box_fill}{Fill color for a box plot.}
  \item{vbs_pt_fill}{Points in a VBS scatterplot are black by default because
        the background is the violin, which is based on the current theme
        color. To use the values for \code{pt_fill} and \code{pt_color}
        specified by the \code{\link{style}} function, set to \code{"default"}.
        Or set to any desired color.}
  \item{vbs_mean}{Show the mean on the box plot with a strip the color
        of \code{out_fill}, which can be changed with the
        \code{\link{style}} function.}
  \item{fences}{If \code{TRUE}, draw the inner upper and lower fences as
        dotted line segments.}
  \item{n_min_pivot}{For the pivot table for a VBS plot over at least a \code{by}
       or \code{facet1} categorical variable, the minimum sample size for a group
       for the corresponding row to be displayed. The default value is 1 for
       all rows to be displayed except for those groups that do not contain data.
       Set to 0 to view all groups.}
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
        or for categorical variables, a
        factor, the size of the bubbles represents
        frequency, with a default of 0.22.}
  \item{power}{Relative size of the scaling of the bubbles to each other.
        Value of 0.5 scales the bubbles so that the area of each
        bubble is the value of the corresponding sizing variable. Default
        value is 0.5,. Value of 1 scales so the radius of 
        the bubble is the value of the sizing variable, increasing
        the discrepancy of size between the variables.}
  \item{low_fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient of the fill color
        beginning with this color.}
  \item{hi_fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of these plots, sets a color gradient of the fill color
        ending with this color.}\cr


  \item{type}{Set to \code{"smooth"} for a \bold{smoothed density plot}
        for two numerical variables or \code{"contour"} for a \bold{contour plot}.}
  \item{smooth_points}{Number of points superimposed on the density plot in the
        areas of the lowest density to help identify outliers, which controls
        how dark are the smoothed points.}
  \item{smooth_size}{Size of points superimposed on the density plot.
        The default value is 1, which results and a very small size.}
  \item{smooth_power}{Exponent of the function that maps the density scale to
        the color scale. Smaller than default of 0.25 yields darker plots.}
  \item{smooth_bins}{Number of bins in both directions for the density
        estimation.}
  \item{n_bins}{Specify the number of bins for a single numeric
       \code{x}-variable from which to visualize the mean or median of a
       numeric \code{y}-variable for each bin. Points are plotted
       as bubbles, the size dependent on the sample size for the bin,
       unless \code{size} is specified at a constant value. Default value
       is 1 for no binning.}
   \item{contour_n}{Number of contour levels in a contour plot, with the
       default value of 10.}
   \item{contour_nbins}{Number of bins constructed for each \code{x} and
        \code{y} variables from which the form the 2D grid of 
        estimated densities.}
   \item{contour_points}{If \code{TRUE}, then plot the points in the 
        scatterplot with a light shade of gray with a white border,
        the data from which the contour curves are estimated. Not responsive
        to color changes but can use the \code{size} parameter to change the size
        of the plotted points, with a default size of 0.72.}\cr


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


  \item{xlab, ylab}{\bold{Axis label} for \code{x}-axis or \code{y}-axis.
       If not specified, then the label becomes
       the name of the corresponding variable label if it exists, or, if not, the
       variable name. If \code{xy_ticks} is \code{FALSE}, no \code{ylab}
       is displayed. Customize these and related parameters with parameters
       such as \code{lab_color} from the \code{\link{style}} function.}
  \item{main}{Label for the title of the graph.  If the corresponding variable
       labels exist,
       then the title is set by default from the corresponding variable labels.}
  \item{sub}{Sub-title of graph, below \code{xlab}. Not yet implemented.}
  \item{label_adjust}{Two-element vector -- x-axis label, y-axis label -- adjusts
       the position of the axis labels in approximate inches. + values move
       the labels away from plot edge. Not applicable to Trellis graphics.}
  \item{margin_adjust}{Four-element vector -- top, right, bottom and left --
       adjusts the margins of the plotted figure in approximate inches.
       + values move the corresponding margin away from plot edge.
       Can use in conjunction with \code{offset} that can move axis values
       into a larger margin space. Not applicable to VBS and Trellis graphics.}
  \item{pad_x}{Proportion of padding added to left and right sides of the
       \code{x}-axis, respectively. Value from 0 to 1 for each of the two 
       elements. If only one element specified, value is applied to both
       sides.}
  \item{pad_y}{Proportion of padding added to bottom and top sides of the
       \code{y}-axis, respectively. Value from 0 to 1 for each of the two 
       elements. If only one element specified, value is applied to both
       sides.}\cr

  \item{scale_x}{If specified, a vector of three values that define the
        x-axis with numerical values: starting value, ending value, and number
        of intervals.}
  \item{scale_y}{If specified, a vector of three values that define the
        y-axis with numerical values: starting value, ending value, and number
        of intervals.}
  \item{origin_x}{Origin of \code{x}-axis. Starting value of \code{x}, by
       default the minimum value of \code{x}, except for time series plots
       and when \code{stat}
       is set to \code{"count"} or related  where the origin is zero
       by default.}
  \item{origin_y}{Origin of \code{y}-axis. Starting value of \code{y}, by
       default the minimum value of \code{x}, except for time series plots
       and when \code{stat}
       is set to \code{"count"} or related  where the origin is zero
       by default.}\cr

  \item{rotate_x}{\bold{Rotation in degrees of the value labels} on
        the \code{x}-axis, usually to accommodate longer values,
        typically used in conjunction with \code{offset}. When equal 90
        the value labels are perpendicular to the x-axis and a different
        algorithm places the labels so that \code{offset} is not needed.}
  \item{rotate_y}{Degrees that the axis values for the value labels on
        the \code{y}-axis are rotated, usually to accommodate longer values,
        typically used in conjunction with \code{offset}.}
  \item{offset}{The amount of spacing between the axis values and the axis. Default
        is 0.5. Larger values such as 1.0 create additional space for the label when
        longer axis value names are rotated. Can use in conjunction with
        \code{margin_adjust} to create space in the margin to accommodate 
        the axis values.}\cr
  \item{axis_fmt}{Numeric format of the axis labels for both axes. Default is to
        round thousands to \code{"K"}, such as 100000 to 100K. Also can
        specify \code{","} to insert commas in large numbers with a 
        decimal point or \code{"."} to insert periods, or \code{""}
        to turn off formatting.}
  \item{axis_x_prefix}{Prefix for axis labels on the \code{x}-axis,
        such as \code{"$"}.}
  \item{axis_y_prefix}{Prefix for axis labels on the \code{y}-axis,
        such as \code{"$"}.}
  \item{xy_ticks}{Flag that indicates if tick marks and associated \bold{value
        labels} on the axes are to be displayed. To rotate the axis values, use
        \code{rotate_x}, \code{rotate_y}, and \code{offset} from the
        \code{\link{style}} function.}
  \item{n_axis_x_skip}{Particularly useful for Trellis or facet plots in which
        the axis text labels are subject to overlapping, specify the interval
        for skipping the label on the $x$-axis. A value of zero means to
        include all the labels, the default.
        A value of one means to English every other label, 
        a value of two indicates to include every second label, etc.
        Also consider the \code{rotate_x} parameter.}
  \item{n_axis_y_skip}{Same as for the $x$-axis but applies to the $y$-axis.}\cr


  \item{legend_title}{Title of the legend for a multiple-variable \code{x}
       or \code{y} plot.}\cr


  \item{add}{\bold{Overlay one or more objects}, text or a geometric figures,
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


  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{do_plot}{If \code{TRUE}, the default, then generate the plot.}
  \item{pdf_file}{Indicate to direct pdf graphics to the specified name of
        the pdf file.}
  \item{width}{Width of the plot window in inches, defaults to 5 except in RStudio
        to maintain an approximate square plotting area.}
  \item{height}{Height of the plot window in inches, defaults to 4.5 except for
        1-D scatterplots and when in RStudio.}
  \item{digits_d}{Number of significant digits for each of the displayed summary
        statistics.}\cr


  \item{n_cat}{Number of categories, specifies the largest number of
        unique, equally spaced integer values of a variable for which
        the variable will be analyzed as categorical instead of continuous.
        Default is 0. Use to specify that such variables are to be analyzed
        as categorical, a kind of informal R factor.
        \bold{[deprecated]}: Best to convert a categorical integer variable
        to a factor.}
  \item{value_labels}{For factors, default is the factor labels, and for
        character variables, default is the character values.
        Or, provide labels for the \code{x}-axis on the graph to override
        these values. If the variable is a
        factor and \code{value_labels} is not specified (is \code{NULL}), then the
        value_labels are set to the factor levels with each space replaced by
        a new line character. If \code{x} and \code{y}-axes have the same scale,
        they also apply to the \code{y}-axis. Control the plotted size
        with \code{axis_cex} and \code{axis_x_cex} from the lessR
        \code{\link{style} function.}
        \bold{[deprecated]}: Better to convert a categorical integer variable to
        a factor.}
  \item{rows}{\bold{Deprecated} old parameter name that is now called \code{filter}.}
  \item{by1}{\bold{Deprecated} old parameter name, replaced with the more descriptive
        \code{facet1}.}
  \item{by2}{\bold{Deprecated} old parameter name, replaced with the more descriptive
        \code{facet2}.}\cr
  \item{smooth}{\bold{Deprecated} old parameter name, replaced with 
       \code{type="smooth"} to also allow for \code{type="contour"}.}\cr


  \item{eval_df}{Determines if to check for existing data frame and
        specified variables. By default is \code{TRUE}
        unless the \code{shiny} package is loaded then set to \code{FALSE} so
        that Shiny will run. Needs to be set to \code{FALSE} if using
        the pipe \code{\%\>\%} notation.}
  \item{fun_call}{Function call. Used with \code{knitr} to pass the function
        call when
        obtained from the abbreviated function call \code{sp}.}\cr

  \item{\dots}{Other parameter values for non-Trellis graphics as defined by and
      processed by standard R functions \code{\link{plot}} and \code{\link{par}},
      including\cr
      \code{cex.main} for the size of the title\cr
      \code{col.main} for the color of the title\cr
      \code{sub} and \code{col.sub} for a subtitle and its color
  }
}


\details{
VARIABLES and TRELLIS PLOTS\cr
There is at least one primary variable, \code{x}, which defines the coordinate system for plotting in terms of the \code{x}-axis, the horizontal axis. Plots may also specify a second primary variable, \code{y}, which defines the \code{y}-axis of the coordinate system. One of these primary variables may be a vector. The simplest plot is from the specification of only one or two primary variables, each as a single variable, which generates a single scatterplot of either one or two variables, necessarily on a single plot, called a panel, defined by a single \code{x}-axis and usually a single \code{y}-axis_

For numeric primary variables, a single panel may also contain multiple plots of two types. Form the first type from subsets of observations (rows of data) based on values of a categorical variable. Specify this plot with the \code{by} parameter, which identifies the grouping variable to generate a scatterplot of the primary variables for each of its levels. The points for each group are plotted with a different shape and/or color. By default, the colors vary, though to maintain the color scheme, if there are only two levels of the grouping variable, the points for one level are filled with the current theme color and the points for the second level are plotted with transparent interiors.

Or, obtain multiple scatterplots on the same panel with multiple numeric \code{x}-variables, or multiple \code{y}-variables. To obtain this graph, specify one of the primary variables as a vector of multiple variables.

Trellis graphics (facets), from Deepayan Sarkar's (2009) \code{lattice} package, may be implemented in which multiple panels for one numeric \code{x}-variable and one numeric \code{y}-variable are displayed according to the levels of one or two categorical variables, called conditioning variables.  A variable specified with \code{by} is a conditioning variable that results in a Trellis plot, the scatterplot of \code{x} and \code{y} produced at \emph{each} level of the \code{facet1} variable. The inclusion of a second conditioning variable, \code{facet2}, results in a separate scatterplot panel for \emph{each} combination of cross-classified values of both \code{facet1} and \code{facet2}. A grouping variable according to \code{by} may also be specified, which is then applied to each panel. If there are 1000 or less unique values of \code{x}, an analysis of the maximum number of repetitions for each value of \code{facet1} is provided.

Control the panel dimensions and the overall size of the Trellis plot with the following parameters: \code{width} and \code{height} for the physical dimensions of the plot window, \code{n_row} and \code{n_col} for the number of rows and columns of panels, and \code{aspect} for the ratio of the height to the width of each panel. The plot window is the standard graphics window that displays on the screen, or it can be specified as a pdf file with the \code{pdf_file} parameter.

CATEGORICAL VARIABLES\cr
Conceptually, there are continuous variables and categorical variables. Categorical variables have relatively few unique data values. However, categorical variables can be defined with non-numeric values, but also with numeric values, such as responses to a five-point Likert scale from Strongly Disagree to Strongly Agree, with responses coded 1 to 5. The three \code{by} --variables -- \code{facet1}, \code{facet2} and \code{by} -- only apply to graphs created with numeric \code{x} and/or \code{y} variables, continuous or categorical.

A scatterplot of Likert type data is problematic because there are so few possibilities for points in the scatterplot. For example, for a scatterplot of two five-point Likert response data, there are only 26 possible paired values to plot, so most of the plotted points overlap with others. In this situation, that is, when a single variable or two variables with Likert response scales are specified, a bubble plot is automatically provided, with the size of each point relative to the joint frequency of the paired data values. To request a sunflower plot in lieu of the bubble plot, set the \code{shape} to \code{"sunflower"}.

DATA\cr
The default input data frame is \code{d}. Specify another name with the \code{data} option.  Regardless of its name, the data frame need not be attached to reference the variables directly by its name, that is, no need to invoke the \code{d$name} notation. The referenced variables can be in the data frame and/or the user's workspace, the global environment.

The data values themselves can be plotted, or for a single variable, counts or proportions can be plotted on the \code{y}-axis. For a categorical \code{x}-variable paired with a continuous variable, means and other statistics can be plotted  at each level of the \code{x}-variable. If \code{x} is continuous, it is binned first, with the standard \code{\link{Histogram}} binning parameters available, such as \code{bin_width}, to override default values. The \code{stat} parameter sets the values to plot, with \code{data} the default. By default, the connecting line segments are provided, so a frequency polygon results. Turn off the line segments by setting \code{line_width=0}.

The \code{filter} parameter subsets rows (cases) of the input data frame according to a logical expression or a set of integers that specify the row numbers of the rows to retain. Use the standard R operators for logical statements as described in \code{\link{Logic}} such as \code{&} for and, \code{|} for or and \code{!} for not, and use the standard R relational operators as described in \code{\link{Comparison}} such as \code{==} for logical equality \code{!=} for not equals, and \code{>} for greater than. Or, to specify vector of integers that correspond to row numbers, define a vector using standard R notation. See the Examples.

VALUE LABELS\cr
[DEPRECATED. Use \code{factor()} instead.] The value labels for each axis can be over-ridden from their values in the data to user supplied values with the \code{value_labels} option. This option is particularly useful for Likert-style data coded as integers. Then, for example, a 0 in the data can be mapped into a "Strongly Disagree" on the plot. These value labels apply to integer categorical variables, and also to factor variables. To enhance the readability of the labels on the graph, any blanks in a value label translate into a new line in the resulting plot. Blanks are also transformed as such for the labels of factor variables.

However, the lessR function \code{\link{factors}} allows for the easy creation of factors, one variable or a vector of variables, in a single statement, and is generally recommended as the method for providing value labels for the variables.

VARIABLE LABELS\cr
Although standard R does not provide for variable labels, \code{lessR} can store the labels in the data frame with the data, obtained from the \code{\link{Read}} function or \code{\link{VariableLabels}}.  If variable labels exist, then the corresponding variable label is by default listed as the label for the corresponding axis and on the text output.

ONE VARIABLE PLOT\cr
The one variable plot of one continuous variable generates either a violin/box/scatterplot (VBS plot), or a run chart with if \code{.Index} appears as the name of the first variable listed, or \code{x} can be an R time series variable for a time series chart. For the box plot,
for gray scale output potential outliers are plotted with squares and outliers are plotted with diamonds, otherwise shades of red are used to highlight outliers. The default definition of outliers is based on the standard boxplot rule of values more than 1.5 IQR's from the box. The definition of outliers may be adjusted (Hubert and Vandervieren, 2008), such that the whiskers are computed from the medcouple index of skewness (Brys, Hubert, & Struyf, 2004).

The plot can also be obtained as a bubble plot of frequencies for a categorical variable.

TWO VARIABLE PLOT\cr
When two variables are specified to plot, by default if the values of the first variable, \code{x}, are unsorted, or if there are unequal intervals between adjacent values, or if there is missing data for either variable, a scatterplot is produced from a call to the Base R \code{\link{plot}} function. By default, sorted values with equal intervals between adjacent values of the first of the two specified variables yields a function plot if there is no missing data for either variable, that is, a call to the standard R \code{\link{plot}} function with \code{segments=TRUE}, which connects each adjacent pair of points with a line segment.

Specifying multiple, continuous \code{x}-variables against a single y variable, or vice versa, results in multiple plots on the same graph. The color of the points of the second variable is the same as that of the first variable, but with a transparent fill. For more than two \code{x}-variables, multiple colors are displayed, one for each \code{x}-variable.

BUBBLE PLOT FREQUENCY MATRIX (BPFM)\cr
Multiple categorical variables for \code{x} may be specified in the absence of a \code{y} variable. A bubble plot results that illustrates the frequency of each response for each of the variables in a common figure in which the \code{x}-axis contains all of the unique labels for all of the variables plotted. Each line of information, the bubbles and counts for a single variable, replaces the standard bar chart in a more compact display. Usually the most meaningful when each variable in the matrix has the same response categories, that is, levels, such as for a set of shared Likert scales. The BPFM is considerably condensed presentation of frequencies for a set of variables than are the corresponding bar charts.

SCATTERPLOT MATRIX\cr
A single vector of continuous variables specified as \code{x}, with no \code{y}-variable, generates a scatterplot matrix of the specified variable.

The scatterplot matrix is displayed according to the current color theme. Specific colors such as \code{fill}, \code{color}, etc. can also be provided. The upper triangle shows the correlation coefficient, and the lower triangle each corresponding scatterplot, with, by default, the non-linear loess best fit line. The \code{code} fit option can be used to provide the linear least squares line instead, along with the corresponding \code{fit_color} for the color of the fit line.

SIZE VARIABLE\cr
A variable specified with \code{size=} is a numerical variable that activates a bubble plot in which the size of each bubble is determined by the value of the corresponding value of \code{size}, which can be a variable or a constant.

To explicitly vary the shapes, use \code{shape} and a list of shape values in the standard R form with the \code{\link{c}} function to combine a list of values, one specified shape for each group, as shown in the examples. To explicitly vary the colors, use \code{fill}, such as with R standard color names. If \code{fill} is specified without \code{shape}, then colors are varied, but not shapes.  To vary both shapes and colors, specify values for both options, always with one shape or color specified for each level of the \code{by} variable.

Shapes beyond the standard list of named shapes, such as \code{"circle"}, are also available as single characters.  Any single letter, uppercase or lowercase, any single digit, and the characters \code{"+"}, \code{"*"} and \code{"#"} are available, as illustrated in the examples. In the use of \code{shape}, either use standard named shapes, or individual characters, but not both in a single specification.

SCATTERPLOT ELLIPSE\cr
For a scatterplot of two numeric variables, the \code{ellipse=TRUE} option draws the .95 data ellipse as computed by the \code{ellipse} function, written by Duncan Murdoch and E. D. Chow, from the \code{ellipse} package. The axes are automatically lengthened to provide space for the entire ellipse that extends beyond the maximum and minimum data values. The specific level of the ellipse can be specified with a numerical value in the form of a proportion. Multiple numerical values of \code{ellipse} may also be specified to obtain multiple ellipses.

BOXPLOTS\cr
For a single variable the preferred plot is the integrated violin/box/scatter plot or VBS plot. Only the violin or box plot can be obtained with the corresponding aliases \code{\link{ViolinPlot}} and \code{\link{BoxPlot}}, or by setting \code{vbs_plot} to \code{"v"} or \code{"b"}. To view a box plot of a continuous variable (Y) across the levels of a categorical variable (X), either as part of the full VBS plot, or by itself, there are two possibilities:\cr
1. Plot(Y,X) or BoxPlot(Y, X)\cr
2. Plot(Y, facet1=X) or BoxPlot(Y, facet1=X)\cr
Both styles produce the same information. What differs is the color scheme.

The first possibility places the multiple box plots on a single pane and also, for the default color scheme \code{"colors"}, displays the sequence of box plots with the default qualitative color palette from the lessR function \code{\link{getColors}}.
All colors are displayed at the same level of gray-scale saturation and brightness to avoid perceptual bias. \code{\link{BarChart}} and \code{\link{PieChart}} use the same default colors as well.

The second possibility with \code{facet1} produces the different box plots on a separate panel, that is, a Trellis chart. These box plots are displayed with a single hue, the first color, blue, in the default qualitative sequence.

TIME CHARTS\cr
See \url{https://web.pdx.edu/~gerbing/lessR/examples/Time.html} for more explanation and examples.

Specify \code{.Index} as the name of the \code{x}-variable. The \code{y} variable is then plotted as a run chart. The values of the specified \code{x}-variable are plotted on the \code{y}-axis, with Index on the \code{x}-axis. Index is the ordinal position of each data value, from 1 to the number of values.

If the specified \code{x}-variable is of type \code{Date}, or is an R time series, a time series plot is generated for each specified variable. If data are represented as a formal R time-series, univariate or multivariate, specify as the \code{x}-variable. One possibility is to specify the \code{x}-variable of type \code{Date}, or, have \code{Plot} do the \code{as.Date()} conversion implicitly from entered character-string numerical dates such as "08/18/1952". Then specify the  \code{y}-variable as one or more time series to plot. The \code{y}-variable can be formatted as long-form data with all the values in a single column, or as wide-formatted data with the time-series variables in separate columns.

\code{Plot()} makes a reasonable attempt to decode a character string decimal date value as the \code{x}-axis variable as read from a text data file such as a \code{csv} file. However, some date formats are not available for conversion by default, such as date values that include the name of the month instead of its number. In general, there can be no guarantee that a date format is not miss-inferred as they can be inherently ambiguous.

If the default date conversion is not working or is not available, then manually supply the date format following one of the format examples in the following table according to the parameter \code{ts_format}.

\tabular{ll}{
Example Date \tab Format\cr
--------------------------- \tab ----------------- \cr
\code{"2022-09-01"} \tab \code{"\%Y-\%m-\%d"}\cr
\code{"2022/9/1"} \tab \code{"\%Y/\%m/\%d"}\cr
\code{"2022.09.01"} \tab \code{"\%Y.\%m.\%d"}\cr
\code{"09/01/2022"} \tab \code{"\%m/\%d/\%Y"}\cr
\code{"9/1/22"} \tab \code{"\%m/\%d/\%y"}\cr
\code{"September 1, 2022"} \tab \code{"\%B \%d, \%Y"}\cr
\code{"Sep 1, 2022"} \tab \code{"\%b \%d, \%Y"}\cr
\code{"20220901"} \tab \code{"\%Y\%m\%d"}\cr
--------------------------- \tab ----------------- \cr
}

Also, \code{Plot()} will convert character string dates such as \code{2024 Aug} and \code{2024 Q1}. Use three-letter abbreviations for the months or use Q1, Q2, Q3, or Q4 for the quarter.

The parameter \code{ts_unit} aggregates the date variable according to its specified value. The aggregation is based on two functions from the \code{xts} package, \code{endpoints()} and \code{period.apply()}. For example, a data variable has daily values but is plotted with aggregated quarterly values.

Specify the function by which to aggregate with the parameter \code{ts_agg}. The default is \code{"sum"}.

In terms of missing data, if the date value exists and the corresponding y-value is missing, with value \code{NA}, then the visualization leaves a corresponding y-value blank. If the date value is also missing, then the nearest adjacent points are connected by line segment which runs over the missing data value. For example, consider a daily time series such that "2021-01-07" and "2021-01-09" are both present with their corresponding y values, but there is no date value or y value for January 8, that is, "2021-01-08". The entire row of data is missing. The resulting visualization plot the y-value for January 7 and also for January 9, with a line segment connecting those two points. There is no corresponding label on the x-axis for the missing data value but the January 9 value is appropriately placed two days after the January 7 value on the visualization.


2-D KERNEL DENSITY\cr
Set \code{type} to \code{"smooth"} to implicitly call the R function \code{\link{smoothScatter}}, according to the current color theme. Useful for very large data sets. The \code{smooth_points} parameter plots points from the regions of the lowest density. The \code{smooth_bins} parameter specifies the number of bins in both directions for the density estimation. The \code{smooth_power} parameter specifies the exponent in the function that maps the density scale to the color scale to allow customization of the intensity of the plotted gradient colors. Higher values result in less color saturation, de-emphasizing points from regions of lessor density. These parameters are respectively passed directly to the \code{\link{smoothScatter}} \code{nrpoints}, \code{nbin} and \code{transformation} parameters. Grid lines are turned off,
by default, but can be displayed by setting the \code{grid_color} parameter.

Or, plot contour curves by setting \code{type} to \code{"contour"}.

COLORS\cr
A color theme for all the colors can be chosen for a specific plot with the \code{colors} option with the \code{lessR} function \code{\link{style}}. The default color theme is \code{"lightbronze"}. A gray scale is available with \code{"gray"}, and other themes are available as explained in \code{\link{style}}, such as \code{"sienna"} and \code{"darkred"}. Use the option \code{style(sub_theme="black")} for a black background and partial transparency of plotted colors.

Colors can also be changed for individual aspects of a scatterplot as well with the \code{\link{style}} function. To provide a warmer tone by slightly enhancing red, try a background color such as \code{panel_fill="snow"}. Obtain a very light gray with \code{panel_fill="gray99"}.  To darken the background gray, try \code{panel_fill="gray97"} or lower numbers. See the \code{lessR} function \code{\link{showColors}}, which provides an example of all available named R colors with their RGB valu

For the color options, such as \code{violin_color}, the value of \code{"off"} is the same as \code{"transparent"}.
\cr

ANNOTATIONS\cr
Use the \code{add} and related parameters to annotate the plot with text and/or geometric figures. Each object is placed according from one to four corresponding coordinates, the required coordinates to plot that object, as shown in the following table. \code{x}-coordinates may have the value of \code{"mean_x"} and \code{y}-coordinates may have the value of \code{"mean_y"}.\cr

\tabular{lll}{
Value \tab Object \tab Required Coordinates\cr
----------- \tab ------------------- \tab -----------------------\cr
\code{"text"} \tab text \tab x1, y1\cr
\code{"point"} \tab text \tab x1, y1\cr
\code{"rect"} \tab rectangle \tab x1, y1, x2, y2\cr
\code{"line"} \tab line segment \tab x1, y1, x2, y2\cr
\code{"arrow"} \tab arrow \tab x1, y1, x2, y2\cr
\code{"v_line"} \tab vertical line  \tab x1\cr
\code{"h_line"} \tab horizontal line  \tab y1\cr
\code{"means"} \tab horiz, vert lines  \tab \cr
----------- \tab ------------------- \tab -----------------------\cr
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
\item{xlim}{The limits of the plot on the \code{x}-axis, expressed as c(x1,x2), where \code{x1} and \code{x2} are the limits. Note that \code{x1 > x2} is allowed and leads to a reversed axis.}
\item{ylim}{The limits of the plot on the \code{y}-axis.}
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
The output can optionally be saved into an \code{R} object, otherwise it simply appears in the console. Each value in the output will only appear if activated in the analysis. For example, the outlier identification must be activated for the analysis, such as from parameter \code{MD_cut}, for \code{out_outliers} to appear in the output.

Here is an example of saving the output to an R object with any valid R name, such as \code{p}: \code{p <- Plot(Years, Salary)}. To see the names of the output objects for that specific analysis, enter \code{names(p)}. To display any of the objects, precede the name with \code{p$}, such as \code{p$out_stats}. View the output at the R console or within a markdown document that displays your results.

READABLE OUTPUT\cr
\code{out_stats}: Correlational analysis.\cr
\code{out_outliers}: Mahalanobis Distance of each outlier.\cr
\code{out_frcst}: Forecasted values.\cr
\code{out_fitted}: Fitted values to data.\cr
\code{out_coefs}: Linear and seasonal coefficients from forecasting.\cr
\code{out_smooth}: Smoothing parameters from exponential smoothing forecasting.\cr
\code{out_bubble}: Bubble plot parameters, \code{radius} and \code{power}.\cr
\code{out_reg}: Regression statistics from setting the \code{fit parameter}.\cr
\code{out_parm}: Parameter settings for a VBS plot of a continuous variable.\cr
\code{out_pivot}: Pivot table from VBS plot of a continuous variable based on any combination of specified values of parameters \code{by}, \code{facet1}, and \code{facet2}.\cr
\code{out_by}:  Pivot table from VBS plot of a continuous variable for the levels of the \code{by} variable, if present.\cr
\code{out_facet1}:  Pivot table from VBS plot of a continuous variable for the levels of the \code{facet1} variable, if present.\cr
\code{out_facet2}:  Pivot table from VBS plot of a continuous variable for the levels of the \code{facet2} variable, if present.

STATISTICS\cr
\code{outliers}: Row numbers that contain the outliers.\cr
}


\references{
Brys, G., Hubert, M., & Struyf, A. (2004). A robust measure of skewness. Journal of Computational and Graphical Statistics, 13(4), 996-1017.

Murdoch, D, and  Chow, E. D. (2013).  \code{ellipse} function from the \code{ellipse} package package.

Gerbing, D. W. (2023). R Data Analysis without Programming, 2nd edition, Chapter 10, NY: Routledge.

Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 5, NY: CRC Press.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871.

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
d <- d[.(random(0.6)),]  # less computationally intensive
dd=d

#---------------------------------------------------
# traditional scatterplot with two numeric variables
#---------------------------------------------------

Plot(Years, Salary, by=Gender, size=2, fit="lm",
     fill=c(M="olivedrab3", W="gold1"),
     color=c(M="darkgreen", W="gold4"))

# scatterplot with all defaults
Plot(Years, Salary)
# or use abbreviation sp in place of Plot
# or use full expression ScatterPlot in place of Plot

# maximum information, minimum input: scatterplot +
#  means, outliers, ellipse, least-squares lines with and w/o outliers
Plot(Years, Salary, enhance=TRUE)

# extend x and y axes
Plot(Years, Salary, scale_x=c(-10, 35, 10), scale_y=c(0,200000,10))

Plot(Years, Salary, add="Hi", x1=c(12, 16, 18), y1=c(80000, 100000, 60000))

Plot(Salary, row_names)

d <- factors(Gender, levels=c("M", "F"))
Plot(Years, Salary, facet1=Gender)
d <- dd

\donttest{

# just males employed more than 5 years
Plot(Years, Salary, filter=(Gender=="M" & Years > 5))

# plot 0.95 data ellipse with the points identified that represent
#   outliers defined by a Mahalanobis Distance larger than 6
# save outliers into R object out
d[1, "Salary"] <- 200000
out <- Plot(Years, Salary, ellipse=0.95, MD_cut=6)

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
Plot(eruptions, waiting, transparency=.5, ellipse=seq(.6,.9), data=faithful)

# scatterplot with two x-variables, plotted against Salary
# define a new style, then back to default
style(window_fill=rgb(247,242,230, maxColorValue=255),
  panel_fill="off", panel_color="off", pt_fill="black", transparency=0,
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
Plot(Years, Salary, type="smooth")
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
#   to a factor with lessR factors (which also does multiple variables)
# always save to the full data frame with factors
d <- factors(Gender, levels=c("M", "W"))
Plot(Years, Salary, facet1=Gender)
d <- Read("Employee", quiet=TRUE)

\donttest{

# two Trellis classification variables with a single continuous
Plot(Salary, facet1=Dept, facet2=Gender)

# all three by (categorical) variables
Plot(Years, Salary, facet1=Dept, facet2=Gender, by=Plan, n_axis_y_skip=1)

# vary both shape and color with a least-squares fit line for each group
style(color=c("darkgreen", "brown"))
Plot(Years, Salary, facet1=Gender, fit="lm", shape=c("F","M"), size=.8)
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

Plot(Years, by=Gender, size=1.25,
     fill=c("olivedrab3", "gold1"),
     color=c("darkgreen", "gold4"))

\donttest{

# by variable, different colors for different values of the variable
# two panels
Plot(Salary, facet1=Dept)

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
BoxPlot(Salary, facet1=Dept)


# binned values to plot counts
# ----------------------------
# bin the values of Salary to plot counts as a frequency polygon
# the counts are plotted as points instead of the data
Plot(Salary, stat_x="count")  # bin the values


# time charts
#------------
# run chart, with default fill area
Plot(.Index, Salary, area_fill="on")

# two run charts in same panel
# or could do a multivariate time series
Plot(.Index, c(Pre, Post))

# Trellis graphics run chart with custom line width, no points
Plot(.Index, Salary, facet1=Gender, line_width=3, size=0)

# daily time series plot
# create the daily time series from R built-in data set airquality
oz.ts <- ts(airquality$Ozone, start=c(1973, 121), frequency=365)
Plot(oz.ts)

# multiple time series plotted from dates and stacked
# black background with translucent areas, then reset theme to default
style(sub_theme="black", color="steelblue2", transparency=.55,
  window_fill="gray10", grid_color="gray25")
date <- seq(as.Date("2013/1/1"), as.Date("2016/1/1"), by="quarter")
x1 <- rnorm(13, 100, 15)
x2 <- rnorm(13, 100, 15)
x3 <- rnorm(13, 100, 15)
df <- data.frame(date, x1, x2, x3)
rm(date); rm(x1); rm(x2); rm(x3)
Plot(date, x1:x3, data=df)
style()

# aggregate monthly data to plot by quarter
n.q <- 42
month <- seq(as.Date("2013/1/1"), length=n.q, by="months")
x <- rnorm(n.q, 100, 15)
Plot(month, x, ts_unit="quarters")


# trigger a time series with a Date variable specified first
# stock prices for three companies by month:  Apple, IBM, Intel
d <- rd("StockPrice")
# only plot Apple
Plot(Month, Price, filter=(Company=="Apple"))
# Trellis plots, one for each company
Plot(Month, Price, facet1=Company, n_col=1)
# all three plots on the same panel, three shades of blue
Plot(Month, Price, by=Company, color="blues")
# exponential smoothing forecast for next 12 months, 
#   aggregate monthly data by mean over quarters
Plot(Month, Price, ts_ahead=12, ts_unit="quarters")
}

#------------------------------------------
# analysis of a single categorical variable
#------------------------------------------
d <- rd("Employee")

# default 1-D bubble plot
# frequency plot, replaces bar chart
Plot(Dept)

\donttest{

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
Plot(Salary, row_names)

# standard scatterplot
Plot(Salary, row_names, segments_y=FALSE)

# Cleveland dot plot with two x-variables
Plot(c(Pre, Post), row_names)


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
Plot(Years, Salary, ellipse=0.95, add="means")

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

# use value labels for the integer values, modify color options
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
   "Slightly Agree", "Agree", "Strongly Agree")
style(fill="powderblue", color="blue", bubble_text="darkred")
d <- factors(m01:m20, 0:5, labels=LikertCats)
Plot(m01:m10)
style()  # reset theme

\donttest{

Plot(m06, m07)

#-----------------------------
# Bubble Plot Frequency Matrix
#-----------------------------

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
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }


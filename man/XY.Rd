\name{XY}
\alias{XY}
\alias{Plot}
\alias{ScatterPlot}
\alias{sp}

\title{Scatterplots including Time Series}

\description{

\code{lessR} introduces the concept of a \emph{data view} visualization function, in which the choice of visualization function directly reflects the structure of the data and the analyst's goal for understanding the data. The function \code{XY()} visualizes the joint distribution of two continuous variables,
or more generally the relationship among multiple variables, along with associated
summary statistics. The specific representation is selected with the
\code{type} argument:
\itemize{
  \item Scatterplot of points with \code{type = "scatter"} (default)
  \item Smoothed scatterplot with \code{type = "smooth"}
  \item Contour plot (2-dimensional density display) with \code{type = "contour"}
}

For the standard scatterplot, stratification divides the data into groups
via the arguments \code{by} and \code{facet}. The \code{by} argument overlays
groups in a single coordinate system for direct comparison, while
\code{facet} produces coordinated small multiples on separate panels.
Depending on the combination of variable types supplied to \code{x} and
\code{y}, \code{XY()} can also create time series, bubble plots, and
Cleveland-style dot or lollipop plots.

When running in RStudio, static graphics are drawn in the \code{Plots}
window. When the default \code{use_plotly=TRUE}, the interactive
\pkg{plotly} version of the scatterplot (including time series) display is also drawn in the \code{Viewer} window.

A scatterplot displays the joint values of one or more variables as points
in an \(n\)-dimensional coordinate system, in which the coordinates of each
point are the values of \(n\) variables for a single observation (row of
data). With a common syntax, \code{XY(x, y, ...)} generates a family of
related 1- or 2-dimensional relationship plots, possibly enhanced with
fitted lines, ellipses, and outlier labeling. Categorical variables should
typically be defined as factors, and date variables should be stored as
\code{Date} objects. When \code{x} is of class \code{Date}, the display is
treated as a time-indexed series.

\code{XY()} therefore serves as the relationship-oriented counterpart to
\code{Chart()} (aggregated values) and \code{X()} (univariate distributions)
within the three-function visualization framework implemented in \pkg{lessR}.
}

\usage{
XY(

    # -------------------------------------------------------
    # Data from which to construct the plot for x- and y-axis
    x, y=NULL, data=d, filter=NULL,

    # --------------------------------------------------
    # Stratification: Same panel or Trellis (facet) plot [x, or x and y]
    by=NULL, facet=NULL,
    n_row=NULL, n_col=NULL, aspect="fill",

    # ----------------------------------------------------------
    # Types of plots
    type=c("scatter", "smooth", "contour"), 


    # -------------------------------
    # Enhancements and customizations
    # -------------------------------

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
    fit_color=getOption("fit_color"), fit_lwd=getOption("fit_lwd"),
    fit_new=NULL, plot_errors=FALSE, ellipse=0,

    # -----------------------------------------------------------------------
    # Time series and forecasting, plot x values sequentially [xDate, y or Y]
    ts_unit=NULL, ts_ahead=0, ts_method=c("es", "lm"),
    ts_source=c("fable", "classic"), ts_agg=c("sum", "mean"),
    ts_NA=NULL, ts_format=NULL, ts_fitted=FALSE, ts_PI=.95, 
    ts_trend=NULL, ts_seasons=NULL, ts_error=NULL, 
    ts_alpha=NULL, ts_beta=NULL, ts_gamma=NULL, ts_new_x=NULL,
    ts_stack=FALSE, ts_area_fill="transparent", ts_area_split=0,
    ts_n_x_tics=NULL,
    # Run chart (indicate with .Index for the name of the x-variable)
    show_runs=FALSE, center_line=c("off", "mean", "median", "zero"),

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
    use_plotly=getOption("lessR.use_plotly"),
    pdf_file=NULL, width=6.5, height=6,
    digits_d=NULL,

    # -------------------------------------------------------------
    # Deprecated, to be removed in future versions
    n_cat=getOption("n_cat"), value_labels=NULL,  # use R factors instead
    rows=NULL, facet1=NULL, facet2=NULL, smooth=FALSE,
    stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
    stat_x=c("count", "proportion", "\%"),

    # ---------------------------------
    # Other
    eval_df=NULL, fun_call=NULL, \dots)
}

\arguments{
  \item{x}{The \emph{variable} plotted on the \code{x}-axis.
        The \strong{data values} can be continuous or categorical,
        cross-sectional or a time series. If \code{x} is sorted with equal
        intervals separating the values, or is a time series, then by default
       the points are plotted sequentially and connected with line segments.
        If named \code{.Index}, a run chart is generated from the corresponding
        \code{y} variable. Can specify multiple \code{x}-variables or multiple
        \code{y}-variables as vectors, but not both.}
  \item{y}{The \emph{variable} plotted on the vertical \code{y}-axis.
        Can be continuous or categorical.}
  \item{data}{Optional data frame that contains one or both of \code{x} and
        \code{y}. Default data frame is \code{d}.}
  \item{filter}{A logical expression or a vector of integers that specify the
        row numbers to retain, defining a subset of rows of the data frame to
        analyze.}\cr

  \item{by}{A \emph{grouping variable}, a categorical variable that provides
        separate group profiles of the primary numeric variables \code{x}
        and, optionally, \code{y} on the \emph{same} plot.
        For two-variable plots, the same grouping applies to the panels of a
        \strong{Trellis graphic} if \code{facet1} is specified.}
  \item{facet}{One categorical variable, or a vector of two categorical
        variables, that activates \bold{facet graphics} (Trellis plots) via
        the \pkg{lattice} package, providing a separate plot of the primary
        variable(s) \code{x} and \code{y} for each level of the variable or
        combination of two levels.}
  \item{n_row}{Optional specification for the number of rows
        in the layout of a multi-panel display with Trellis graphics.
        Specify \code{n_col} or \code{n_row}, but not both.}
  \item{n_col}{Optional specification for the number of columns in the
        layout of a multi-panel display with Trellis (facet) graphics.
        Specify \code{n_col} or \code{n_row}, but not both.
        The default is \code{1}.}
  \item{aspect}{Lattice parameter for the aspect ratio of the Trellis panels
        (facets), defined as height divided by width.
        The default value is \code{"fill"} to have the panels
        expand to occupy as much space as possible. Set to \code{1} for
        square panels. Set to \code{"xy"} to specify a ratio that “banks”
        to 45 degrees, that is, with the typical line slope approximately
        45 degrees.}\cr

  \item{type}{Display type. The default is \code{"scatter"} for a scatter plot.
        Set to \code{"smooth"} for a \strong{smoothed density plot}
        for two numeric variables, or \code{"contour"} for a
        \strong{contour plot} summarizing the bivariate density.}\cr

  \item{theme}{Color theme for this analysis. Make persistent across analyses
        with \code{\link{style}}.}
  \item{fill}{Fill color for the points or for the area under a line chart.
        Can also be set via the lessR function \code{\link{getColors}}
        to select from a variety of color palettes. For points, the default is
        \code{pt_fill}; for the area under a line chart, \code{violin_fill}.
        For a line chart, set to \code{"on"} to use the default color.}
  \item{color}{Border color of the points or line color for a line plot.
        Can be a vector to customize the color for each point or a color
        range such as \code{"blues"} (see \code{\link{getColors}}).
        Default is \code{pt_color} from the lessR \code{\link{style}} function.}
  \item{transparency}{Transparency factor of the fill color of each point.
        Default is \code{trans_pt_fill} from the lessR \code{\link{style}}
        function, which is 0.1 for points. Bubbles have a default
        transparency of 0.3.}\cr

  \item{enhance}{For a two-variable scatterplot, if \code{TRUE},
        automatically add the 0.95 data ellipse,
        labels for outliers beyond a Mahalanobis distance of 6 from the
        ellipse center, the least squares line for all data,
        the least squares line for the data excluding outliers, and
        horizontal and vertical lines representing the means of the two
        variables.}
  \item{means}{If one variable is categorical (factor) and the other is
       continuous, then if \code{TRUE} the group means are plotted in the
       scatterplot by default. Also applies to a 1-D scatterplot.}
  \item{size}{When set to a constant, the scaling factor for \strong{standard points}
       (not bubbles) or for a line, with default of 1.0 for points and 2.0 for a line.
       Set to 0 to suppress plotting of points or lines. If \code{ts_area_fill} is
       used for a line chart, then the default is 0. When set to a variable,
       activates a bubble plot, with the size of each bubble further determined
       by the value of \code{radius}. Applies to the standard two-variable
       scatterplot.}
  \item{size_cut}{For a bubble plot in which bubble sizes are defined by a
       \code{size} variable, controls whether and how many bubble labels are
       shown. If \code{1} (or \code{TRUE}), show the value of the sizing
       variable for selected bubbles in the center of the bubbles, except
       when the bubble is too small. If \code{0} (or \code{FALSE}), no values
       are displayed. If a number greater than 1, display labels only for the
       indicated number of values (e.g., the maximum and minimum when set
       to \code{2}). The color of the displayed text is set by
       \code{bubble_text} from the \code{\link{style}} function.}
  \item{shape}{Plot character(s). The default value is \code{"circle"}, with
       both a default exterior color and filled interior, explicitly specified
       by \code{"color"} and \code{"fill"}.
       Other possible values with fillable interiors
       are \code{"circle"}, \code{"square"}, \code{"diamond"},
       \code{"triup"} (triangle up), and \code{"tridown"} (triangle down),
       all uppercase and lowercase letters, all digits, and most punctuation
       characters. The integers 0–25 as defined by the base R
       \code{\link{points}} function also apply. If plotting levels for
       different groups according to \code{by}, specify a vector of shapes
       with one shape per group, or set to \code{"vary"} to have shapes
       selected automatically across \code{by} groups.}
  \item{line_width}{Width of the line segments that connect adjacent points,
        such as in time series plots. Set to 0 to remove line segments.}
  \item{segments}{Designed for interaction plots of means: connects each pair of
        successive points with a line segment. Pass a data frame of means,
        such as from \code{\link{pivot}}. To turn off connecting line segments
        for sorted, equal-interval data, set to \code{FALSE}. Currently,
        does not apply to Trellis plots.}
  \item{segments_y}{For one \code{x}-variable, draw a line segment from the
        \code{y}-axis to each plotted point, such as for a Cleveland dot plot.
        For two \code{x}-variables, the line segments connect the two points.}
  \item{segments_x}{Draw a line segment from the \code{x}-axis for each
        plotted point.}\cr

  \item{sort}{Sort the values of \code{y} by the values of \code{x}, such as
        for a Cleveland dot plot (numeric \code{x} paired with a categorical
        \code{y} with unique values). If \code{x} is a vector of two variables,
        sort by their difference.}
  \item{jitter_x}{Randomly perturb the plotted points of
       a scatterplot horizontally within the limits of the specified value,
       or set to \code{NULL} to rely upon the computed default value.}
  \item{jitter_y}{Vertical jitter. Defaults to \code{0}. Same interpretation
        as \code{jitter_x}.}\cr

  \item{ID}{Name of a variable that provides the \strong{labels for selected
       plotted points for outlier identification}, row names of the data frame
       by default. To label all points, use the \code{add} parameter
       (see below).}
  \item{ID_size}{Size of the plotted labels.
        Modify label text color with the \code{\link{style}} function
        parameter \code{ID_color}.}
  \item{MD_cut}{Mahalanobis distance cutoff to define an outlier in a
       two-variable scatterplot.}
  \item{out_cut}{Count or proportion of plotted points to label, in order of their
       distance from the center (means) of the univariate distribution or 
       scatterplot, sorted from most to least extreme.
       For two-variable plots, distance from the center is based on Mahalanobis
       distance.}
  \item{out_shape}{Shape of outlier points in a two-variable scatterplot
        or VBS plot.
        Modify fill color from the current \code{theme} with
        \code{out_fill} and \code{out2_fill} in \code{\link{style}}.}
  \item{out_size}{Size of outlier points in a two-variable scatterplot
        or VBS plot.}\cr

  \item{fit}{Type of \strong{best fit line}. Default is \code{"off"}.
        Options include \code{"loess"} for a non-linear smooth,
        \code{"lm"} for a linear least squares model,
        \code{"null"} for the null (intercept-only) model,
        \code{"exp"} for exponential growth or decay,
        \code{"power"} for a general power model in conjunction with
        \code{fit_power}, and \code{"quad"} for a quadratic (power 2)
        model. If potential outliers are identified according to
        \code{out_cut}, a second (dashed) fit line is displayed based on
        the data \emph{excluding} those outliers.}
  \item{fit_power}{Power that describes the response \code{y} as a power
       function of the predictor \code{x}, required when
       \code{fit = "power"}. Optionally, and experimentally, may apply
       to \code{fit = "exp"}.}
  \item{fit_se}{Confidence level for the error band displayed around the
       line of best fit. Default is 0.95 when a fit line is specified,
       but is turned off when \code{plot_errors = TRUE}.
       Can be a vector to display multiple confidence bands.
       Set to 0 to suppress error bands.}
  \item{fit_color}{Color of the fit line.}
  \item{fit_lwd}{Line width of the fit line.}
  \item{fit_new}{When \code{fit} is set to a fitted curve such as
       \code{"lm"} or \code{"quad"}, predicted values are computed at the
       \code{x}-values supplied here.}
  \item{plot_errors}{If \code{TRUE}, plot line segments joining each point to
        the regression line (\code{"loess"} or \code{"lm"}), illustrating the
        size of the residuals.}
  \item{ellipse}{Confidence level of a data ellipse for a scatterplot
        of one \code{x}-variable and one \code{y}-variable according to the
        contours of the corresponding bivariate normal density.
        Specify the confidence level(s) as a single number or a vector of
        values between 0 and 1 to plot one or more ellipses.
        Ellipses are not available in the interactive \code{plotly} version.
        For Trellis graphics, only the maximum level is used, with one ellipse
        per panel. Modify fill and border colors with the \code{\link{style}}
        parameters \code{ellipse_fill} and \code{ellipse_color}.}\cr
\item{ts_unit}{Time unit for plotting \strong{time series data} when \code{x}
  is of type \code{Date}. Default is to infer the unit from the observed date
  spacing. Specify \code{ts_unit} to override the inferred unit and, when
  requested, aggregate to a coarser unit. Valid values are \code{"days"},
  \code{"weeks"}, \code{"months"}, \code{"quarters"}, \code{"years"}, and
  \code{"days7"} (daily data with a 7-day seasonal cycle).}
  \item{ts_ahead}{Number of \code{ts_unit} steps ahead of the last time period
        to forecast.}
  \item{ts_method}{Forecasting method. Default is \code{"es"} for exponential
       smoothing models. Alternatively, choose \code{"lm"} for a least squares
       linear regression model that accounts for seasonality.}
  \item{ts_source}{Source of forecasting functions. Default is \code{"fable"}
        for access to \code{ETS()} and \code{TSLM()} from the \pkg{fable}
        package and related functions from the \pkg{fpp3} ecosystem, or
        \code{"classic"} for base R forecasting functions.}
  \item{ts_agg}{Function used to aggregate over time according to
        \code{ts_unit}. Default is \code{"sum"} with an option for
        \code{"mean"}.}
  \item{ts_NA}{By default, missing \code{y} values (\code{NA}) are not plotted,
        leaving gaps. Alternatively, specify a value, usually 0, to replace
        \code{NA} and plot that value (e.g., 0) at the corresponding date.
        Forecasting with missing data is not supported.}
  \item{ts_format}{Optional format string for \code{as.Date()} describing
        the values of the date variable on the \code{x}-axis, needed if the
        function cannot infer the date format. For example, the character
        string \code{"09/01/2024"} can be described by
        \code{"\%m/\%d/\%Y"}. See \code{details} for more information.}
  \item{ts_fitted}{If \code{TRUE} and \code{ts_ahead} is used for forecasting,
        display for each data point the fitted value, level, trend, and
        seasonal component.}
  \item{ts_PI}{Prediction interval level about the forecasted values,
       with default of \code{0.95}.}
  \item{ts_trend}{Trend parameter. Default value is \code{NULL}, allowing the
       procedure to choose the specification that yields the best-fitting
       model for the data. Values \code{"A"} and \code{"M"} indicate additive
       and multiplicative models, respectively. Specify \code{"N"} to omit
       the trend component. See the forecasting documentation for valid
       combinations of trend, season, and error.}
  \item{ts_seasons}{Seasonality parameter. See \code{ts_trend}.}
  \item{ts_error}{Error parameter. See \code{ts_trend}.}
  \item{ts_alpha}{Exponential smoothing level parameter. Sets the value for
       \code{HoltWinters()}, and acts as a suggestion for
       \code{ETS()} in the \code{"fable"} framework.}
  \item{ts_beta}{Exponential smoothing trend parameter. See \code{ts_alpha}.}
  \item{ts_gamma}{Exponential smoothing seasonal parameter. See \code{ts_alpha}.}
  \item{ts_new_x}{A data frame of predictor variable names and new values for
       exogenous regressors used for model fitting and forecasting.
       The number of rows of new data values determines the number of future
       time periods for which forecasts are generated.
       Currently applies only to the default \code{"fable"} linear model
       for \code{ts_method = "lm"}.}
  \item{ts_stack}{If \code{TRUE}, multiple time plots are stacked on each other,
        with \code{area} set to \code{TRUE} by default.}
  \item{ts_area_fill}{Fill under line segments, if present.
       If \code{ts_stack} is \code{TRUE}, the default is a gradient from the
       default color range (e.g., \code{"blues"}).
       If not specified, and \code{fill} is set with no plotted points and
       \code{ts_area_fill} is not specified, then \code{fill} generally
       controls the area under the line segments.}
  \item{ts_area_split}{Applies only to a Trellis plot activated with \code{facet1}.
       Value of \code{y} that defines a reference line splitting the filled
       area under the time series line. Values of \code{y} below this threshold
       lie below the reference line; values above lie above the line.}
  \item{ts_n_x_tics}{Suggested number of ticks for dates on the \code{x}-axis,
       overriding the default of approximately seven ticks.}
  \item{show_runs}{If \code{TRUE}, display the individual runs in the run analysis.
        Also sets \code{run} to \code{TRUE}. Customize the color of the line
        segments with \code{segments_color} via \code{\link{style}}.}
  \item{center_line}{Plots a dashed line through the middle of a run chart.
        Default center line is the \code{"median"}, suitable when values
        randomly vary about the median. Alternatively, \code{"mean"} and
        \code{"zero"} specify that the center line goes through the mean or
        zero, respectively. Currently not supported for Trellis plots.}\cr


  \item{vbs_plot}{A character string that specifies the components of the
        \strong{integrated Violin–Box–Scatterplot (VBS) of a continuous variable}.
        A \code{"v"} in the string indicates a violin plot, a \code{"b"}
        indicates a box plot with flagged outliers, and a \code{"s"}
        indicates a 1-variable scatterplot. Default value is \code{"vbs"}.
        The characters can be in any order and in upper- or lower-case.
        Generalize to Trellis plots with the
        \code{facet1} and \code{facet2} parameters, but currently only for
        horizontal displays.
        Modify fill and border colors from the current \code{theme} with
        the \code{\link{style}} parameters \code{violin_fill},
        \code{violin_color}, \code{box_fill} and \code{box_color}.}
  \item{vbs_ratio}{Height of the violin plot relative to the plot area.
        Make the violin (and the accompanying box plot) larger or smaller by
        adjusting either the plot area or this value.}
  \item{bw}{Bandwidth for the smoothness of the violin plot. Higher values
        give smoother plots. Default is to calculate a bandwidth that provides
        a relatively smooth density estimate.}
  \item{bw_iter}{Number of iterations used to adjust the default R bandwidth
        for further smoothing of the density estimate. When set, the
        iterations and corresponding results are displayed.}
  \item{violin_fill}{Fill color for a violin plot.}
  \item{box_fill}{Fill color for a box plot.}
  \item{vbs_pt_fill}{Points in a VBS scatterplot are black by default because
        the background violin is based on the current theme color.
        To use the \code{pt_fill} and \code{pt_color} values specified by
        \code{\link{style}}, set \code{vbs_pt_fill = "default"}.
        Otherwise, set to any desired color.}
  \item{vbs_mean}{Show the mean on the box plot with a strip in the color
        of \code{out_fill}, which can be changed with the
        \code{\link{style}} function.}
  \item{fences}{If \code{TRUE}, draw the inner upper and lower fences as
        dotted line segments.}
  \item{n_min_pivot}{For the pivot table underlying a VBS plot over at least a
       \code{by} or \code{facet1} categorical variable, sets the minimum sample
       size for a group for the corresponding row to be displayed.
       Default is 1 for all non-empty groups. Set to 0 to view all groups.}
  \item{k}{IQR multiplier that determines the distance of the whiskers of the
        box plot from the box. Default is Tukey's setting of 1.5.}
  \item{box_adj}{Adjust the box and whiskers, and thus outlier detection,
        for skewness using the medcouple statistic as a robust measure
        of skewness according to Hubert and Vandervieren (2008).}
  \item{a, b}{Scaling factors for the adjusted box plot to set the lengths
        of the whiskers. If explicitly set, \code{box_adj} is activated.}\cr


  \item{radius}{Scaling factor of the bubbles in a \strong{bubble plot}, which
        sets the radius of the largest displayed bubble in inches. To
        activate bubble scaling, set \code{size} to a third variable, or
        for categorical variables (factors), the bubble sizes represent
        frequencies.}
  \item{power}{Relative scaling of bubble sizes. The default value of 0.5
        scales bubbles so that the area of each bubble is proportional to
        the corresponding sizing variable. A value of 1 scales bubble radii
        directly by the sizing variable, increasing the size differences
        between values.}
  \item{low_fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of such plots, sets the low end of a color gradient
        for bubble fill.}
  \item{hi_fill}{For a categorical variable and the resulting bubble plot,
        or a matrix of such plots, sets the high end of a color gradient
        for bubble fill.}\cr

  \item{smooth_points}{Number of points superimposed on the density plot in
        the areas of lowest density to help identify outliers, thereby
        controlling how dark the smoothed display becomes.}
  \item{smooth_size}{Size of points superimposed on the density plot.
        Default value is 1, resulting in very small points.}
  \item{smooth_power}{Exponent of the function that maps the density scale to
        the color scale. Smaller values than the default of 0.25 yield
        darker plots.}
  \item{smooth_bins}{Number of bins in each direction for the density
        estimation.}
  \item{n_bins}{Number of bins for a single numeric
       \code{x}-variable when visualizing the mean or median of a
       numeric \code{y}-variable for each bin. Points are plotted
       as bubbles, with bubble size proportional to the bin sample size,
       unless \code{size} is specified as a constant. Default is 1
       (no binning).}
  \item{contour_n}{Number of contour levels in a contour plot, with
       default of 10.}
  \item{contour_nbins}{Number of bins constructed for each of \code{x} and
        \code{y}, forming the 2D grid from which densities are estimated.}
  \item{contour_points}{If \code{TRUE}, plot the original scatterplot points
        in a light gray with white borders, representing the data from which
        the contour curves are estimated. Color is not affected by other
        color settings, but point size can be changed with \code{size}.
        Default size is 0.72.}\cr

  \item{bin}{If \code{values = "count"},  display the frequency distribution for 
       a frequency polygon.}
  \item{bin_start}{Optional starting value of the bins for a
        \strong{frequency polygon}.}
  \item{bin_width}{Optional specified bin width. Also sets
        \code{bin = TRUE}.}
  \item{bin_end}{Optional value that lies within the last bin, so the
        actual endpoint of the last bin may be larger than the specified
        value.}
  \item{breaks}{Method for calculating bins, or an explicit
        specification of the bins, such as via \code{\link{seq}} or other
        options provided by \code{\link{hist}}. Also sets
        \code{bin = TRUE}.}
  \item{cumulate}{If \code{TRUE}, display a cumulative frequency polygon.}\cr

  \item{xlab, ylab}{\strong{Axis labels} for the \code{x}- and \code{y}-axes.
       If not specified, labels default to the corresponding variable labels
       (if present), otherwise to the variable names. If \code{xy_ticks} is
       \code{FALSE}, no \code{ylab} is displayed. Customize these and related
       parameters with options such as \code{lab_color} in
       \code{\link{style}}.}
  \item{main}{Label for the plot title. If corresponding variable labels
       exist, the title is set by default from those labels.}
  \item{sub}{Subtitle for the graph, below \code{xlab}. Not yet implemented.}
  \item{label_adjust}{Two-element vector (x-axis label, y-axis label) that
       adjusts the positions of axis labels in approximate inches.
       Positive values move labels away from the plot edge.
       Not applicable to Trellis graphics.}
  \item{margin_adjust}{Four-element vector (top, right, bottom, left)
       that adjusts plot margins in approximate inches.
       Positive values move the corresponding margin away from the plot edge.
       Can be used with \code{offset} to move axis values into the expanded
       margin space. Not applicable to Trellis graphics.}
  \item{pad_x}{Proportion of padding added to the left and right sides of the
       \code{x}-axis. Values range from 0 to 1 for each of the two elements.
       If only one element is specified, it is applied to both sides.}
  \item{pad_y}{Proportion of padding added to the bottom and top sides of the
       \code{y}-axis. Values range from 0 to 1 for each of the two elements.
       If only one element is specified, it is applied to both sides.}\cr

  \item{scale_x}{If specified, a vector of three values that define the
        numeric \code{x}-axis: starting value, ending value, and number
        of intervals.}
  \item{scale_y}{If specified, a vector of three values that define the
        numeric \code{y}-axis: starting value, ending value, and number
        of intervals.}
  \item{origin_x}{Origin of the \code{x}-axis. Default is the minimum value
       of \code{x}, except for time series plots and when \code{stat}
       is set to \code{"count"} or a related option, in which case the
       origin defaults to 0.}
  \item{origin_y}{Origin of the \code{y}-axis. Default is the minimum value
       of \code{y}, except for time series plots and when \code{stat}
       is set to \code{"count"} or a related option, in which case the
       origin defaults to 0.}\cr

  \item{rotate_x}{\strong{Rotation in degrees of the axis value labels} on
        the \code{x}-axis, usually to accommodate longer labels,
        typically used with \code{offset}. If set to 90, labels are
        perpendicular to the axis and a different placement algorithm is
        used so that \code{offset} is not needed.}
  \item{rotate_y}{Rotation in degrees of the axis value labels on
        the \code{y}-axis, usually to accommodate longer labels,
        typically used with \code{offset}.}
  \item{offset}{Spacing between axis value labels and the axis. Default
        is 0.5. Larger values (e.g., 1.0) create additional space for
        labels, especially when rotated. Can be used with
        \code{margin_adjust} to create additional margin space for axis
        labels and values.}\cr
  \item{axis_fmt}{Numeric format of axis labels for both axes. Default
        rounds thousands to \code{"K"} (e.g., 100000 becomes 100K).
        Alternatives include \code{","} to insert commas in large numbers
        with a decimal point, \code{"."} to insert periods, or \code{""}
        to turn off formatting.}
  \item{axis_x_prefix}{Prefix for axis labels on the \code{x}-axis,
        such as \code{"$"}.}
  \item{axis_y_prefix}{Prefix for axis labels on the \code{y}-axis,
        such as \code{"$"}.}
  \item{xy_ticks}{Flag indicating whether tick marks and associated
        \strong{value labels} on the axes are displayed. To rotate axis values,
        use \code{rotate_x}, \code{rotate_y}, and \code{offset} (see
        \code{\link{style}}).}
  \item{n_axis_x_skip}{Particularly useful for Trellis or facet plots with
        many labels on the \code{x}-axis. Specifies the spacing for skipping
        labels. A value of 0 includes all labels (default). A value of 1
        skips every other label, 2 includes every third label, etc.
        Also consider \code{rotate_x}.}
  \item{n_axis_y_skip}{Same as \code{n_axis_x_skip} but applies to the
        \code{y}-axis.}\cr

  \item{legend_title}{Title of the legend for a multiple-variable \code{x}
       or \code{y} plot.}\cr

  \item{add}{\strong{Overlay one or more objects}, text or geometric figures,
       on the plot. Possible values include any text (first argument) or
       \code{"text"}; \code{"labels"} to label each point with the row name;
       and geometric objects \code{"rect"} (rectangle), \code{"line"},
       \code{"arrow"}, \code{"v_line"} (vertical line), and
       \code{"h_line"} (horizontal line).
       The value \code{"means"} draws vertical and horizontal lines at the
       respective means. Does not apply to Trellis graphics.
       Customize with parameters such as \code{add_fill} and \code{add_color}
       via \code{\link{style}}.}
  \item{x1}{First x-coordinate for each overlaid object; may be
       \code{"mean_x"}. Not used for \code{"h_line"}.}
  \item{y1}{First y-coordinate for each overlaid object; may be
       \code{"mean_y"}. Not used for \code{"v_line"}.}
  \item{x2}{Second x-coordinate for each object; may be \code{"mean_x"}.
       Used only for \code{"rect"}, \code{"line"}, and \code{"arrow"}.}
  \item{y2}{Second y-coordinate for each object; may be \code{"mean_y"}.
       Used only for \code{"rect"}, \code{"line"}, and \code{"arrow"}.}\cr

  \item{quiet}{If \code{TRUE}, suppress text output. Can change the system
       default with \code{\link{style}}.}
  \item{do_plot}{If \code{TRUE} (default), generate the plot.}
  \item{use_plotly}{If \code{TRUE} (default), draw a \code{plotly} interactive
        plot in the RStudio \code{Viewer} window in addition to the static plot
        in the \code{Plots} window. Not all options are available in the
        interactive version, but key options are supported.}
  \item{pdf_file}{If specified, direct PDF graphics to the given file name.}
  \item{width}{Width of the plot window in inches, default 5 except within
        RStudio, where the default maintains an approximately square plotting
        area.}
  \item{height}{Height of the plot window in inches, default 4.5 except for
        1-D scatterplots and when running in RStudio.}
  \item{digits_d}{Number of significant digits for each displayed summary
        statistic.}\cr

  \item{n_cat}{Maximum number of unique, equally spaced integer values of a
        variable for which it will be analyzed as categorical rather than
        continuous. Default is 0. Use to treat such variables as informal
        factors. \emph{Deprecated}. Best to convert integer-coded categoricals
        to factors explicitly.}
  \item{value_labels}{For factors, default labels are the factor levels;
        for character variables, default labels are the character values.
        Optionally provide labels for the \code{x}-axis to override defaults.
        If the variable is a factor and \code{value_labels} is \code{NULL},
        then labels are set to the factor levels with spaces replaced by
        line breaks. If \code{x} and \code{y} share the same scale, labels
        also apply to the \code{y}-axis. Control label size with
        \code{axis_cex} and \code{axis_x_cex} from the lessR
        \code{\link{style}} function.}
  \item{rows}{\emph{Deprecated}. Old parameter name; use \code{filter}.}
  \item{facet1}{\emph{Deprecated}. Old parameter name; use \code{facet}.}
  \item{facet2}{\emph{Deprecated}. Old parameter name; use a two-element
        vector in \code{facet}.}
  \item{smooth}{\emph{Deprecated}. Use parameter \code{type}.}
  \item{stat}{Apply a \strong{specified aggregation} such as \code{"mean"} to the
       numerical \code{y} variable at each level of a categorical \code{x}
       variable. The resulting dot plot (Cleveland plot) is analogous to a
       bar chart but emphasizes position along a common scale.}
  \item{stat_x}{If no \code{y} variable is specified, constructs a frequency
        polygon for \code{x} with access to \code{bin_width}. Either use the
        default \code{"count"} for each bin or \code{"proportion"}, also
        indicated by \code{\%}.}\cr

  \item{eval_df}{Controls whether to check for an existing data frame and
        specified variables. Default is \code{TRUE}, unless the \pkg{shiny}
        package is loaded, in which case it is set to \code{FALSE} so that
        Shiny can run. Must be set to \code{FALSE} when using the pipe
        operator \code{\%\>\%}.}
  \item{fun_call}{Function call. Used with \pkg{knitr} to pass the function
        call when obtained from the abbreviated helper \code{sp}.}\cr

  \item{\dots}{Other parameters for non-Trellis graphics as recognized by
      base R functions \code{\link{plot}} and \code{\link{par}}, including
      \code{cex.main} for the size of the title,
      \code{col.main} for the color of the title,
      and \code{sub} and \code{col.sub} for a subtitle and its color.}
}


\details{
VARIABLES and TRELLIS PLOTS

There is at least one primary variable, \code{x}, which defines the horizontal
\code{x}-axis. A second primary variable, \code{y}, defines the vertical
\code{y}-axis. Either \code{x} or \code{y} (but not both simultaneously) may be
a vector of variables. The simplest usage—single \code{x} and \code{y}—produces
a single scatterplot on one panel. With \code{type = "scatter"} this is a
standard point plot; alternative values such as \code{"smooth"} and
\code{"contour"} invoke 2-D kernel density and contour displays, respectively.

For numeric primary variables, multiple plots can appear on a single panel in
two ways. First, by defining groups with the \code{by} argument:
\code{by} identifies a categorical grouping variable, and a separate
scatterplot layer is drawn for each of its levels. Group levels are
distinguished by color and/or shape. By default, colors vary across groups;
for two groups, a common pattern is a filled symbol for one group and a point
with transparent interior for the other.

Second, multiple numeric \code{x}-variables or multiple \code{y}-variables can
be supplied as a vector, which produces multiple series on the same panel. This
is commonly used for time series overlays and multi-series line plots.

Trellis graphics (facets), from Deepayan Sarkar's (2009) \code{lattice}
package, may also be used. A variable specified with \code{facet} is a
conditioning variable. A single \code{facet} variable produces one panel for
each of its levels; a length-two vector of facet variables produces one panel
for each cross-classified combination of their levels. Within each panel,
\code{x} and \code{y} are plotted as usual, and an additional grouping variable
specified with \code{by} may be applied to all panels. When \code{x} has at
most 1000 unique values, XY() can provide a brief diagnostic of the maximum
number of repeated values for each level of \code{facet}.

Control the panel dimensions and the overall size of the Trellis display with
\code{width} and \code{height} for the graphics device, \code{n_row} and
\code{n_col} for the number of panels in each direction, and \code{aspect} for
the panel height-to-width ratio. The plot window is the active graphics device
(e.g., the standard R plotting window, RStudio \code{Plots} pane, or a pdf file
when \code{pdf_file} is specified).\cr

CATEGORICAL VARIABLES

Categorical variables have relatively few unique values and are recommended
to be often coded as factors. However, categorical variables may also be coded numerically, such as Likert responses from 1 to 5. The structuring arguments \code{by} and \code{facet} apply when at least one of \code{x} or \code{y} is numeric, but that numeric variable may represent either a truly continuous scale or a discrete Likert-type scale.

Scatterplots of Likert-type data can be challenging because the number of
possible joint values is small. For two five-point scales there are only 25
possible combinations, so many points overlap at the same coordinates. For such
situations, jittering, or dot/mean plots (using \code{stat}) may
provide clearer displays.\cr

DATA

The default input data frame is \code{d}. Another data frame can be specified
with the \code{data} argument. Regardless of the name of the data frame,
variables can be referenced directly by name—no need to attach the data frame
or use \code{d$name}. Referenced variables may live in the data frame, the
global environment, or both.

The plotted values can be the raw data themselves, or summaries derived from
them. With a single numeric variable, counts or proportions can be plotted on
the \code{y}-axis, optionally after binning \code{x}. For a categorical
\code{x}-variable paired with a continuous \code{y}-variable, summary
statistics such as means can be plotted at each level of \code{x}. When
\code{x} is continuous and binning is desired, XY() uses the same binning
parameters as \code{\link{Histogram}}, such as \code{bin_width}, to override
defaults. The \code{stat} parameter controls what is plotted (e.g.,
\code{"data"}, \code{"mean"}, \code{"median"}). By default, connecting line
segments are drawn, yielding a frequency polygon. Turn off line segments by
setting \code{line_width = 0}.

The \code{filter} parameter subsets rows (cases) of the input data frame
according to a logical expression or a vector of integers specifying row
numbers to retain. Use standard R logical operators as described in
\code{\link{Logic}}—for example, \code{&} (and), \code{|} (or), and \code{!}
(not)—and relational operators as described in \code{\link{Comparison}} such as
\code{==} (equality), \code{!=} (not equal), and \code{>} (greater than). To
specify rows directly, create an integer vector using standard R syntax. See
\code{Examples}.\cr

VALUE LABELS\cr
\emph{Deprecated. Use \code{factor()} instead.}  

The \code{value_labels} option can override the axis tick labels with
user-supplied values. This is particularly useful for Likert-style data coded
as integers: for example, a data value \code{0} can be displayed as
\code{"Strongly Disagree"}. These labels apply to integer categorical variables
and to factor variables. Any spaces in a label are translated into line breaks
to improve readability.

In current workflows, the recommended approach is to define factors with the
desired labels directly, typically via \code{\link{factors}}, which allows
convenient creation of labeled factors for one or more variables in a single
statement.\cr

VARIABLE LABELS

Base R does not natively support variable labels, but lessR stores labels in
the data frame alongside the variables, typically created by
\code{\link{Read}} or \code{\link{VariableLabels}}. When variable labels
exist, XY() uses them by default for axis labels and in text output. Otherwise,
the variable names are used.\cr

TWO VARIABLE PLOT

With two variables specified, XY() behaves as follows. If the values of
\code{x} are unsorted, have unequal intervals, or there is missing data in
either variable, a standard scatterplot is produced (for \code{type =
"scatter"}). When \code{x} is sorted with equal intervals and there are no
missing values, the default display connects adjacent points with line
segments, yielding a function or time-series style plot.

Supplying multiple continuous \code{x}-variables against a single \code{y}, or
vice versa, produces multiple series on the same graph. The points for the
second series reuse the first series' color but with transparent fill; for more
series, additional colors from the current theme are used.\cr

SCATTERPLOT MATRIX

If \code{x} is a vector of continuous variables and \code{y} is omitted,
XY() generates a scatterplot matrix. The matrix adopts the current color theme.
Individual colors (e.g., \code{fill}, \code{color}) can be overridden. The
lower triangle shows the pairwise scatterplots, and the upper triangle shows
the corresponding correlation coefficients. By default a non-linear loess fit
line is added to each scatterplot; the \code{fit} parameter can be used to
request a linear least squares line instead, along with \code{fit_color} to set
its color.\cr

SIZE VARIABLE\cr
Specifying a numeric \code{size} variable activates a bubble plot in which the
area of each bubble is determined by the corresponding value of \code{size}
(modified by \code{radius} and \code{power}).

To vary shapes explicitly across groups, use \code{shape} and provide a vector
of values (e.g., created with \code{\link{c}}). One shape is used for each
level of the grouping variable in \code{by}. To vary colors, use \code{fill};
if \code{fill} is specified without \code{shape}, colors vary but shapes do
not. To vary both, specify both \code{shape} and \code{fill} with values for
each \code{by} level.

Beyond the named shapes such as \code{"circle"}, any single character (letters,
digits, \code{"+"}, \code{"*"}, \code{"#"}) may be used as a plotting symbol.
Within a single specification, either use standard named shapes or single
characters, but not both.\cr

SCATTERPLOT ELLIPSE

For a scatterplot of two numeric variables, the \code{ellipse} argument
requests one or more data ellipses, based on the contours of the corresponding
bivariate normal density. For a single \code{x}- and \code{y}-variable pair,
setting \code{ellipse} to a numeric value between 0 and 1 (e.g., 0.95) draws
the corresponding probability contour. A vector of values produces multiple
ellipses. XY() expands the axes as needed so that ellipses extending beyond the
range of the data remain fully visible. For Trellis graphics, only the largest
ellipse level is used (one ellipse per panel). Ellipse fill and border colors
are controlled via \code{ellipse_fill} and \code{ellipse_color} in
\code{\link{style}}.\cr

TIME CHARTS

See \url{https://web.pdx.edu/~gerbing/lessR/examples/Time.html} for additional
examples and explanation.


\subsection{Run Chart}{

When \code{x} is the special variable \code{.Index}, XY() produces a run chart
of \code{y}. The \code{y}-values are plotted against their index positions
(1, 2, …), and run-chart diagnostics such as center lines and runs analysis can
be displayed via the corresponding arguments.
}

\subsection{Time Series Defined with Dates}{

If \code{x} is of type \code{Date} or an R time-series object, XY() produces a
time series plot for each specified variable. Time-series data can be supplied
in \dQuote{long} format (a single column of values plus a date column) or in
\dQuote{wide} format (multiple time-series columns sharing a date index). XY()
will attempt to convert character string date values (e.g.,
\code{"08/18/1952"}) to \code{Date} via \code{as.Date()}, using default date
formats when possible.

XY() makes a reasonable attempt to decode common date formats, but some formats
(e.g., those with month names rather than numbers) may require an explicit
format string via \code{ts_format}. If the default conversion fails or is
ambiguous, specify the correct format using examples such as those in the
table below. Note that \code{ts_format} can always be provided for explicit
specification of the date format.\cr

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

XY() also converts character-string dates such as \code{"2024 Aug"} and
\code{"2024 Q1"}, interpreting three-letter month abbreviations and quarter
codes \code{Q1}--\code{Q4}.

\strong{Time unit and aggregation.}
The \code{ts_unit} argument controls the time unit used for plotting and, when
specified, can also be used to aggregate the series to a coarser time unit. By
default, XY() attempts to infer a regular time unit from the observed spacing
of the dates (see \sQuote{Detecting the Regularity of Dates}). You may specify
\code{ts_unit} to override the inferred unit, including treating irregular
dates as a regular unit for aggregation (for example, \code{ts_unit="weeks"}).
The special value \code{"days7"} retains daily observations but evaluates
seasonality on a 7-day cycle.

When aggregation is requested, the date index is aggregated with
\code{endpoints()} and \code{period.apply()} from the \code{xts} package. The
aggregation function is specified with \code{ts_agg}, default \code{"sum"},
with \code{"mean"} as a common alternative.
}

\subsection{Detecting the Regularity of Dates (ts_unit)}{

For time series operations, \pkg{lessR} attempts to infer the regularity of a
date variable from the observed sequence of dates. The inferred unit is
returned internally as \code{tu_exist} and is one of \code{"days"},
\code{"weeks"}, \code{"years"}, \code{"quarters"}, \code{"months"}, or
\code{"unknown"}.

The decision rules are:

\enumerate{

\item \strong{Pre-processing.}
Dates are sorted and reduced to unique values. When a grouping variable is
present, regularity is evaluated from the first level only (assuming dates are
already time-sorted within that level). Day gaps are computed as positive
differences between successive dates.

\item \strong{Trivial / short series.}
If there are fewer than two dates, or if no positive gaps are available, the
unit is set to \code{"days"}.

\item \strong{Fine units (gap-based).}
Let \code{gap} be the median of the positive day gaps (median is robust to
occasional missing dates).

\itemize{
  \item If \code{gap == 1}, then \code{tu_exist = "days"}.
  \item If \code{gap == 7}, then the series is a \emph{candidate} weekly series.
  The gap pattern is further examined to distinguish a regular weekly series
  (primarily 7-day gaps, with only occasional longer gaps) from an irregular
  reporting schedule (for example, many 14-day gaps). If judged sufficiently
  weekly, then \code{tu_exist = "weeks"}; otherwise \code{tu_exist = "unknown"}.
}

\item \strong{Coarse units (calendar-based).}
If the unit is still \code{"unknown"} after the fine-unit checks, the dates are
examined for calendar structure. Let \code{year}, \code{month}, and \code{day}
denote the corresponding calendar fields.

\itemize{

  \item \strong{Years.}
  If there is at most one observation per year and all observations share the
  same month and day-of-month (for example, January 1 each year), then
  \code{tu_exist = "years"}. Missing years are permitted.

  \item \strong{Quarters.}
  If there is at most one observation per calendar quarter (unique
  year--quarter combinations) \emph{and} the median day gap between
  observations falls in the range 60--122 days, then
  the existing time unit is deemed as "quarters"}. 
  The reporting month and day-of-month may
  vary across quarters, and missing quarters are permitted. The gap range
  excludes sparse or irregular series that happen to have at most one
  observation per quarter by chance.

  \item \strong{Months.}
If the time variable is a Date but the observations occur exactly once per calendar month with no duplicates within a month and the sequence of months is regular (missing months become explicit NAs if present), then lessR treats the series as monthly and converts the index accordingly.


  \item \strong{Missing periods.}
  When a regular unit is accepted (\code{"weeks"}, \code{"years"},
  \code{"quarters"}, or \code{"months"}), a flag \code{has_missing_periods} is set
  to \code{TRUE} if the sequence of inferred periods contains gaps (for example,
  a 14-day gap in an otherwise weekly series, or a missing month in an otherwise
  monthly series). If the series is classified as \code{"unknown"},
  \code{has_missing_periods} is not used.
  }
}

If the user explicitly specifies \code{ts_unit}, that value is used for
aggregation and modeling, overriding the inferred unit. In particular, a
user-specified \code{ts_unit = "months"} may treat one observation per month as
monthly even if the reporting day varies across months, and a user-specified
\code{ts_unit = "weeks"} may treat dates as weekly even if the inferred
regularity is \code{"unknown"}.

\subsection{Missing Data}{

For missing data, if a date is present but the corresponding \code{y}-value is
\code{NA}, the line is broken at that point (no segment is drawn). If both the
date and value are absent (the entire row is missing), the line connects the
nearest observed dates, spanning the gap in calendar time but skipping the
missing tick label. For example, if "2021-01-07" and "2021-01-09" are present
but "2021-01-08" is absent, the plot includes points for the 7th and 9th
connected by a line; the 8th does not appear on the axis.
}

\subsection{Forecasting}{

Forecasting is activated by setting \code{ts_ahead} to a positive integer. The
trend, seasonal, and error components for exponential smoothing and regression
models follow the \code{fable} conventions and are controlled by
\code{ts_trend}, \code{ts_seasons}, and \code{ts_error}. XY() supports four
forecasting engines: \code{ETS()} and \code{TSLM()} from \code{fable} via
\code{ts_source = "fable"}, and classic \code{HoltWinters()} and regression
decomposition (\code{stl()} + \code{lm()}) via \code{ts_source = "classic"}.
Multiplicative components require positive data. Additive trend is linear,
while multiplicative trend corresponds to exponential growth or decay; additive
and multiplicative seasonality scale the seasonal pattern differently as the
series level changes.

Exponential smoothing via \code{ETS()} (the default when
\code{ts_source = "fable"}) generally provides flexible model specification
and often superior predictive performance compared to classic Holt–Winters.
For regression with seasonality, \code{TSLM()} and the \code{stl()}+\code{lm()}
approach both rely on least squares but handle seasonal structure differently.
Setting \code{ts_fitted = TRUE} provides fitted values and decomposed trend and
seasonal components for inspection.\cr
}


2-D KERNEL DENSITY

Set \code{type = "smooth"} to invoke \code{\link{smoothScatter}} and display a
2-D kernel density estimate for large data sets. The display respects the
current theme. The \code{smooth_points} argument controls how many points from
low-density regions are superimposed, \code{smooth_bins} sets the number of
bins in each direction for density estimation, and \code{smooth_power}
controls the transformation from density scale to color scale. Larger
\code{smooth_power} values reduce saturation in low-density regions. These
arguments map directly to \code{nrpoints}, \code{nbin}, and
\code{transformation} in \code{\link{smoothScatter}}. Grid lines are disabled
by default for smooth density plots, but can be re-enabled via the appropriate
styling options (e.g., \code{grid_color} in \code{\link{style}}).

Alternatively, set \code{type = "contour"} to plot contour lines of the
estimated bivariate density, with the level resolution controlled by
\code{contour_n} and \code{contour_nbins}.\cr

COLORS

A global color theme for \code{XY()} and other lessR graphics can be set with
\code{\link{style}} (e.g., \code{style(theme = "lightbronze")}). The default
theme is \code{"lightbronze"}. A grayscale theme is available via
\code{"gray"}, and other themes (e.g., \code{"sienna"}, \code{"darkred"}) are
described in \code{\link{style}}. The \code{sub_theme = "black"} option yields
a black background with partially transparent plotted colors.

Individual graphical elements (points, lines, panels, grid lines, etc.) can be
customized with additional \code{\link{style}} arguments, such as
\code{panel_fill}. For a subtle warm background, try
\code{panel_fill = "snow"}; very light grays such as
\code{"gray99"} or \code{"gray97"} provide a neutral tone.  

For many color options, the value \code{"off"} is equivalent to
\code{"transparent"}, removing the corresponding fill or borde

See \code{\link{showColors}} for a display of all named R colors and their RGB
values.\cr

ANNOTATIONS

The \code{add} argument and its related coordinates (\code{x1}, \code{y1},
\code{x2}, \code{y2}) annotate the plot with text and geometric objects. Each
object type requires a specific set of coordinates, summarized below.
\code{x}-coordinates may take the special value \code{"mean_x"} and
\code{y}-coordinates may take \code{"mean_y"}.\cr

\tabular{lll}{
Value \tab Object \tab Required Coordinates\cr
----------- \tab ------------------- \tab -----------------------\cr
\code{"text"} \tab text \tab x1, y1\cr
\code{"point"} \tab point \tab x1, y1\cr
\code{"rect"} \tab rectangle \tab x1, y1, x2, y2\cr
\code{"line"} \tab line segment \tab x1, y1, x2, y2\cr
\code{"arrow"} \tab arrow \tab x1, y1, x2, y2\cr
\code{"v_line"} \tab vertical line  \tab x1\cr
\code{"h_line"} \tab horizontal line  \tab y1\cr
\code{"means"} \tab horizontal and vertical lines at the means \tab \cr
----------- \tab ------------------- \tab -----------------------\cr
}

The value of \code{add} specifies the object type. For a single object, provide
one value and its required coordinates. For multiple placements of the same
object, supply coordinate vectors. For multiple different objects, supply a
vector of \code{add} values and, for each coordinate argument (\code{x1},
\code{y1}, \code{x2}, \code{y2}), a vector whose elements correspond to the
sequence of objects in \code{add}.

Styling options such as \code{add_color}, \code{add_fill}, and transparency
may also be given as vectors, allowing different objects to have different
colors or other properties.\cr

PDF OUTPUT

To obtain pdf output, set \code{pdf_file} to a file name, optionally with
\code{width} and \code{height} to control device size. The pdf is written to
the current working directory, which can be explicitly set with
\code{\link{setwd}}.\cr

ADDITIONAL OPTIONS

Many commonly used graphical parameters from the base \proglang{R} function
\code{\link{plot}} are passed through by XY(), including:

\describe{
  \item{cex.main, col.lab, font.sub, etc.}{Settings for main/sub-titles and axis
        annotation; see \code{\link{title}} and \code{\link{par}}.}
  \item{main}{Main title of the graph; see \code{\link{title}}.}
  \item{xlim}{Limits of the \code{x}-axis, expressed as \code{c(x1, x2)}. Note
        that \code{x1 > x2} is allowed and produces a reversed axis.}
  \item{ylim}{Limits of the \code{y}-axis.}
}

ONLY VARIABLES ARE REFERENCED

A referenced variable in a lessR plotting function such as \code{XY} must be a
variable name (or vector of variable names), not an arbitrary expression. The
variable must exist either in the referenced data frame (e.g., the default
\code{d}) or in the user's workspace (global environment). Expressions are not
evaluated in place. For example:\cr

\code{    > XY(rnorm(50), rnorm(50))   # does NOT work}\cr

Instead, create named objects and reference them directly:\cr

\preformatted{    > X <- rnorm(50)   # create vector X in user workspace
    > Y <- rnorm(50)   # create vector Y in user workspace
    > XY(X, Y)         # directly reference X and Y}
}


\value{
The output may be assigned to an \code{R} object; otherwise it is printed directly
to the console. Each component appears only when the corresponding analytic option
is activated. For example, outlier identification must be enabled (e.g., via
\code{MD_cut}) for \code{out_outliers} to be included in the output.

To save the results, assign the output to an object, such as  
\code{p <- XY(Years, Salary)}.  
Use \code{names(p)} to view the available components, and access any one by
prefixing with \code{p$}, for example \code{p$out_stats}.  
Output can be viewed interactively at the console or inserted into R~Markdown
documents for reproducible reporting.

READABLE OUTPUT

\code{out_stats}: Correlational analysis.\cr
\code{out_outliers}: Mahalanobis Distance values for detected outliers.\cr
\code{out_frcst}: Forecasted values.\cr
\code{out_fitted}: Fitted values for the observed data.\cr
\code{out_coefs}: Linear and seasonal coefficients from forecasting models.\cr
\code{out_smooth}: Smoothing parameters from exponential smoothing models.\cr
\code{out_bubble}: Bubble-plot settings, including \code{radius} and \code{power}.\cr
\code{out_reg}: Regression statistics produced when \code{fit} is specified.\cr
\code{out_parm}: Parameter settings for a VBS plot of a continuous variable.\cr
\code{out_pivot}: Pivot table for VBS plots based on any combination of
\code{by} and \code{facet} variables.\cr
\code{out_by}: Pivot table for VBS plots aggregated by the \code{by} variable.\cr
\code{out_facet1}: Pivot table for VBS plots aggregated by the first facet
variable.\cr
\code{out_facet2}: Pivot table for VBS plots aggregated by the second facet
variable.\cr

STATISTICS

\code{outliers}: Row numbers corresponding to detected outliers.\cr
}


\references{
Brys, G., Hubert, M., & Struyf, A. (2004). A robust measure of skewness. Journal of Computational and Graphical Statistics, 13(4), 996-1017.

Murdoch, D, and  Chow, E. D. (2013).  \code{ellipse} function from the \code{ellipse} package.

Gerbing, D. W. (2023). R Data Analysis without Programming, 2nd edition, Chapter 10, NY: Routledge.

Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 5, NY: CRC Press.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871.

Hyndman, R. J., & Athanasopoulos, G. (2021). \emph{Forecasting: Principles and Practice} (3rd ed.). Melbourne, Australia: OTexts. Retrieved from https://otexts.com/fpp3/

Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R, Springer. http://lmdvr.r-forge.r-project.org/

Sievert, C. (2020). \emph{Interactive Web-Based Data Visualization with R, plotly, and shiny}. Chapman and Hall/CRC. URL: \url{https://plotly.com/r/}
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{\code{\link{X}}, \code{\link{Chart}}, \code{\link{style}}.
}


\examples{
# read the data
d <- rd("Employee", quiet=TRUE)
d <- d[.(random(0.6)),]  # less computationally intensive
dd=d

#---------------------------------------------------
# traditional scatterplot with two numeric variables
#---------------------------------------------------

# scatterplot with all defaults
XY(Years, Salary)
# or use Plot in place of XY, the older method

XY(Years, Salary, by=Gender, size=2, fit="lm",
     fill=c(M="olivedrab3", W="gold1"),
     color=c(M="darkgreen", W="gold4"))

# maximum information, minimum input: scatterplot +
#  means, outliers, ellipse, least-squares lines with and w/o outliers
XY(Years, Salary, enhance=TRUE)

# extend x and y axes
XY(Years, Salary, scale_x=c(-10, 35, 10), scale_y=c(0,200000,10))

XY(Years, Salary, add="Hi", x1=c(12, 16, 18), y1=c(80000, 100000, 60000))

d <- factors(Gender, levels=c("M", "W"))
XY(Years, Salary, facet=Gender)
d <- dd

\donttest{

# just males employed more than 5 years
XY(Years, Salary, filter=(Gender=="M" & Years > 5))

# plot 0.95 data ellipse with the points identified that represent
#   outliers defined by a Mahalanobis Distance larger than 6
# save outliers into R object out
d[1, "Salary"] <- 200000
out <- XY(Years, Salary, ellipse=0.95, MD_cut=6)

# new shape and point size, no grid or background color
# then put style back to default
style(panel_fill="powderblue", grid_color="powderblue")
XY(Years, Salary, size=2, shape="diamond")
style()

# translucent data ellipses without points or edges
#  show the idealized joint distribution for bivariate normality
style(ellipse_color="off")
XY(Years, Salary, size=0, ellipse=seq(.1,.9,.10))
style()

# bubble plot with size determined by the value of Pre
# display the value for the bubbles with values of  min, median and max
XY(Years, Salary, size=Pre, size_cut=3)

# variables in a data frame not the default d
# plot 0.6 and 0.9 data ellipses with partially transparent points
# change color theme to gold with black background
style("gold", sub_theme="black")
XY(eruptions, waiting, transparency=.5, ellipse=seq(.6,.9), data=faithful)

# scatterplot with two x-variables, plotted against Salary
# define a new style, then back to default
style(window_fill=rgb(247,242,230, maxColorValue=255),
  panel_fill="off", panel_color="off", pt_fill="black", transparency=0,
  lab_color="black", axis_text_color="black",
  axis_y_color="off", grid_x_color="off", grid_y_color="black",
  grid_lty="dotted", grid_lwd=1)
XY(c(Pre, Post), Salary)
style()

# increase span (smoothing) from default of .7 to 1.25
# span is a loess parameter, which generates a caution that can be
#   ignored that it is not a graphical parameter -- we know that
# display confidence intervals about best-fit line at
#   0.95 confidence level
XY(Years, Salary, fit="loess", span=1.25)

# 2-D kernel density (more useful for larger sample sizes)
XY(Years, Salary, type="smooth")
}

#------------------------------------------------------
# scatterplot matrix from a vector of numeric variables
#------------------------------------------------------

# with least squares fit line
XY(c(Salary, Years, Pre), c(Salary, Years, Pre), fit="lm")


#--------------------------------------------------------------
# Trellis graphics and by for groups with two numeric variables
#--------------------------------------------------------------

# Trellis plot with condition on 1-variable
# optionally re-order default alphabetical R ordering by converting
#   to a factor with lessR factors (which also does multiple variables)
# always save to the full data frame with factors
d <- factors(Gender, levels=c("M", "W"))
XY(Years, Salary, facet=Gender)
d <- Read("Employee", quiet=TRUE)

\donttest{

# all three by (categorical) variables
XY(Years, Salary, facet=c(Dept, Gender), by=Plan, n_axis_y_skip=1)

# vary both shape and color with a least-squares fit line for each group
style(color=c("darkgreen", "brown"))
XY(Years, Salary, facet=Gender, fit="lm", shape=c("W","M"), size=.8)
style("gray")

# compare the men and women Salary according to Years worked
#   with an ellipse for each group
XY(Years, Salary, by=Gender, ellipse=.50)
}



# time charts
#------------
# run chart, with default fill area
XY(.Index, Salary, ts_area_fill="on")

# two run charts in same panel
# or could do a multivariate time series
XY(.Index, c(Pre, Post))

# Trellis graphics run chart with custom line width, no points
XY(.Index, Salary, facet=Gender, line_width=3, size=0)

# daily time series plot
# create the daily time series from R built-in data set airquality
oz.ts <- ts(airquality$Ozone, start=c(1973, 121), frequency=365)
XY(oz.ts)

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
XY(date, x1:x3, data=df)
style()

# aggregate monthly data to plot by quarter
n.q <- 42
month <- seq(as.Date("2013/1/1"), length=n.q, by="months")
x <- rnorm(n.q, 100, 15)
XY(month, x, ts_unit="quarters")


# trigger a time series with a Date variable specified first
# stock prices for three companies by month:  Apple, IBM, Intel
d <- rd("StockPrice")
# only plot Apple
XY(Month, Price, filter=(Company=="Apple"))
# Trellis plots, one for each company
XY(Month, Price, facet=Company, n_col=1)
# all three plots on the same panel, three shades of blue
XY(Month, Price, by=Company, color="blues")
# exponential smoothing forecast for next 12 months, 
#   aggregate monthly data by mean over quarters
XY(Month, Price, ts_ahead=12, ts_unit="quarters")

#------------------------------------------
# analysis of a single categorical variable
#------------------------------------------
d <- rd("Employee")

# default 1-D bubble plot
# frequency plot, replaces bar chart
XY(Dept)

\donttest{

# plot of frequencies for each category (level), replaces bar chart
XY(Dept, stat_x="count")


#----------------------------------------------------
# scatterplot of numeric against categorical variable
#----------------------------------------------------

# generate a chart with the plotted mean of each level
# rotate x-axis labels and then offset from the axis
style(rotate_x=45, offset=1)
XY(Dept, Salary)
style()


#-------------------
# Cleveland dot plot
#-------------------

# standard scatterplot
XY(Salary, row_names, segments_y=FALSE)

# Cleveland dot plot with two x-variables
XY(c(Pre, Post), row_names)


#------------
# annotations
#------------

# add text at the one location specified by x1 and x2
XY(Years, Salary, add="Hi There", x1=12, y1=80000)
# add text at three different specified locations
XY(Years, Salary, add="Hi", x1=c(12, 16, 18), y1=c(80000, 100000, 60000))

# add three different text blocks at three different specified locations
XY(Years, Salary, add=c("Hi", "Bye", "Wow"), x1=c(12, 16, 18),
  y1=c(80000, 100000, 60000))

# add an 0.95 data ellipse and horizontal and vertical lines through the
#  respective means
XY(Years, Salary, ellipse=0.95, add=c("v_line", "h_line"),
  x1="mean_x", y1="mean_y")
# can be done also with the following short-hand
XY(Years, Salary, ellipse=0.95, add="means")

# a rectangle requires two points, four coordinates, <x1,y1> and <x2,y2>
style(add_trans=.8, add_fill="gold", add_color="gold4", add_lwd=0.5)
XY(Years, Salary, add="rect", x1=12, y1=80000, x2=16, y2=115000)

# the first object, a rectangle, requires all four coordinates
# the vertical line at x=2 requires only an x1 coordinate, listed 2nd
XY(Years, Salary, add=c("rect", "v_line"), x1=c(10, 2),
  y1=80000, x2=12, y2=115000)

# two different rectangles with different locations, fill colors and translucence
style(add_fill=c("gold3", "green"), add_trans=c(.8,.4))
XY(Years, Salary, add=c("rect", "rect"),
  x1=c(10, 2), y1=c(60000, 45000), x2=c(12, 75000), y2=c(80000, 55000))
}

#----------------------------------------------------
# analysis of two categorical variables (Likert data)
#----------------------------------------------------

d <- Read("Mach4", quiet=TRUE)  # Likert data, 0 to 5
XY(m06, m07)
\donttest{



#---------------
# function curve
#---------------

x <- seq(10,50,by=2)
y1 <- sqrt(x)
y2 <- x**.33
# x is sorted with equal intervals so run chart by default
XY(x, y1)

# multiple plots from variable vectors need to have the variables
#  in a data frame
d <- data.frame(x, y1, y2)
# if variables are in the user workspace and in a data frame
#   with the same names, the user workspace versions are used,
#   which do not work with vectors of variables, so remove
rm(x); rm(y1); rm(y2)
XY(x, c(y1, y2))
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ color }
\keyword{ grouping variable }


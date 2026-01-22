\name{Chart}
\alias{Chart}
\alias{BarChart}
\alias{PieChart}
\alias{pc}

\title{Charts for One or Two Categorical Variables}

\description{

\code{lessR} introduces the concept of a \emph{data view} visualization function, in which the choice of visualization function directly reflects the structure of the data and the analyst's intent for understanding the data. The function \code{Chart()} visualizes the distribution of a categorical variable along with related statistics from aggregated data of a numerical variable, either counts or a statistic such as the mean of another numerical variable. Choose the type of visualization according to the value of the parameter \code{type}. 
\itemize{
\item Bar chart with \code{type = "bar"}, the default value
\item Radar chart with \code{type = "radar"}
\item Bubble chart with \code{type = "bubble"}
\item Dot chart with \code{type = "dot"}
\item Pie chart with \code{type = "pie"}
\item Starburst chart with \code{type = "pie"} and another categorical variable(s) with \code{by}
\item Treemap chart with \code{type = "treemap"}
\item Icicle chart with \code{type = "icicle"}
}

Stratify, that is, divide the distribution into groups with each group plotted separately, with parameters \code{by}, which plots the groups on the same panel, or \code{facet}, which plots the groups on different panels (not applicable to bubble charts). With this conceptualization, a starburst chart is a pie chart with nested layers, For the hierarchical charts -- pie and starburst, treemap, and icicle -- the \code{by} stratification parameter can be a vector, defining multiple levels. 

When using RStudio, plots are directed to the \code{Plots} window for the bar chart, and also the \code{Viewer} window for Plotly interactive plots. The remaining plots are all Plotly visualizations.

Unless \code{by} is a vector of at least length two, the chart is constructed from the one- or two-dimensional table that pairs each level or joint level of the categorical variables with the corresponding numerical value of \code{y}. Usually, this table is a summary (pivot) table calculated as a data aggregation from the original data table of measurements. A one-dimensional example is the average salary of the employees in each department. Corresponding two-dimensional example is the average salary of men and women in each department. Enter the original, raw data from which \code{Chart} calculates the summary table, or enter the summary table directly as the input data.

\code{Chart} also displays the foundational summary table, such as frequency table for one or two variables. If a frequency table, also displayed are Cramer's V association, and the corresponding chi-square inferential analysis. For two variables, the frequencies include the joint and marginal frequencies.

To activate Trellis graphics or facets, a multi-panel display, specify a \code{facet} variable in place of \code{by} for the second categorical variable.

For bar charts, if the provided object to analyze is a set of multiple variables, including the name of an entire data frame, then a bar chart is calculated for \emph{each} non-numeric variable in the data frame. For the default bar chart, a standard bar chart is presented simultaneously with the interactive version because there are some features in the standard chart not yet available in the interactive version. 
}

\usage{
Chart(

        # ------------------------------------------
        # Data from which to construct the bar chart
        x=NULL, by=NULL, y=NULL, data=d, filter=NULL,

        # -----------------------------------
        # Chart type, defaults to a bar chart
        type=c("bar", "radar", "bubble", "dot", "pie", "icicle", "treemap"),
        hole=0.65,  # pie chart
        radius=0.35, power=0.5,  # bubble chart

        # --------------------------
        # Chart from aggregated data
        stat=c("mean", "sum", "sd", "deviation", "min", "median", "max"),
        stat_x=c("count", "proportion"),

        # --------------------------------------------------------------
        # Facet plot, stratify on different panels
        facet=NULL, n_row=NULL, n_col=NULL, aspect="fill",
        
        # -----------------------------------------------------
        # Bar chart parameters: Layout and ordering of the bars
        horiz=FALSE, sort=c("0", "-", "+"),
        beside=FALSE, stack100=FALSE,
        gap=NULL, scale_y=NULL, one_plot=NULL,

        # ----------------------------------------------------------------
        # Analogy of physical Marks on paper to create the bars and labels
        theme=getOption("theme"),
        fill=NULL,
        color=getOption("bar_color_discrete"),
        transparency=getOption("trans_bar_fill"),
        fill_split=NULL, fill_scaled=FALSE, fill_chroma=75,

        labels=c("\%", "input", "prop", "off"),
        labels_position=c("in", "out"),
        labels_color="white",
        labels_size=0.75,
        labels_decimals=NULL,
        labels_cut=NULL,

        # ------------------------------------------------------------------
        # Labels for axes, values, and legend if x and by variables, margins
        xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
        lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),
        pad_y_min=0, pad_y_max=0,
    
        rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
        break_x=NULL, offset=getOption("offset"),
        axis_fmt=c("K", ",", ".", ""), axis_x_pre="", axis_y_pre="",
        label_max=100,

        legend_title=NULL, legend_position="right_margin",
        legend_labels=NULL, legend_horiz=FALSE,
        legend_size=NULL, legend_abbrev=10, legend_adjust=0,

        # ----------------------------------------------------
        # Draw one or more objects, text, or geometric figures
        # Only applies to the standard bar chart
        add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

        # --------------------------------------------------------------------
        # Output: text or chart turned off, to PDF file, number decimal digits
        quiet=getOption("quiet"), do_plot=TRUE, 
        use_plotly=getOption("lessR.use_plotly"),
        pdf_file=NULL, width=6.5, height=6, 
        digits_d=NULL, out_size=80, 

        # --------------------------------------
        # Deprecated, removed in future versions
        n_cat=getOption("n_cat"), value_labels=NULL,
        rows=NULL, facet1=NULL,

        # -------------
        # Miscellaneous
        eval_df=NULL, fun_call=NULL, \dots)
}


\arguments{
  \item{x}{Primary categorical \bold{variable} to analyze. For bar charts, \code{x} can be
        a single variable (in a data frame or as a vector in the user's workspace),
        a vector of variables specified with \code{\link{c}}, or an entire data frame.
        If not specified, defaults to all non-numeric variables in the data frame
        given by \code{data} (or \code{d} by default).
        To improve label legibility, category labels are automatically wrapped:
        unless \code{break_x = FALSE}, spaces in labels are replaced with line breaks.
        To keep two short words on the same line, replace the intervening space
        with a tilde; the tilde is displayed as a blank in the axis label.}
  \item{by}{Optional second categorical variable for stratification.
        Creates a two-way display (e.g., stacked or grouped bars, nested pies,
        multi-variable bubbles), with subgroups shown within each level of \code{x}.
        The same stratification applies within panels when \code{facet} is used.}
  \item{y}{Numeric variable whose values determine bar heights, bubble sizes,
        or other aggregated measures across categories.
        If \code{y} is supplied for raw data, a summary statistic must be specified
        via \code{stat}. If \code{y} is omitted, counts (or proportions) are
        computed from the data and used as the default response.}
  \item{data}{Optional data frame that contains the variables of interest.
        May be raw data, from which summaries are computed, or a pre-aggregated
        summary table with one categorical column and one numeric column giving
        the heights/sizes of the plotted objects.}
  \item{filter}{Logical expression or vector of row indices that defines a subset
        of rows in \code{data} to analyze.
        Use logical operators such as \code{\&}, \code{|}, and \code{!} and
        relational operators such as \code{==}, \code{!=}, and \code{>}.}

  \item{type}{Chart family to produce. Default is \code{"bar"}.
        Alternatives include \code{"pie"}, \code{"treemap"}, \code{"bubble"},
        and \code{"radar"}. A hierarchical pie chart (with a \code{by} vector)
        is rendered as a sunburst chart.}
  \item{hole}{For pie and sunburst charts, proportion of the radius occupied by
        the inner hole (a doughnut chart). Set to \code{0} or \code{FALSE}
        for a full pie.}
  \item{radius}{For bubble charts, scaling factor for bubble radius (in pixels)
        controlling the size of the largest displayed bubble.}
  \item{power}{For bubble charts, controls the relative scaling of bubbles.
        The default \code{0.5} scales radii so that bubble \emph{areas} are
        proportional to the underlying values. A value of \code{1} scales
        radii directly to the values, increasing visual differences in size.}

  \item{stat}{Summary statistic applied to \code{y} within groups defined
       by \code{x} and optional \code{by}. Typical values include
       \code{"sum"}, \code{"mean"}, \code{"sd"}, \code{"dev"} (mean deviations),
       \code{"min"}, \code{"median"}, and \code{"max"}.
       The resulting summary table (pivot table) defines the plotted heights
       or sizes.}
  \item{stat_x}{When \code{y} is not supplied, specifies whether to plot the
        \code{"count"} (default) of each group or its \code{"proportion"}.}

  \item{facet}{Optional categorical variable that activates \bold{Trellis graphics}
        (facets) using the \code{lattice} framework.
        A separate chart is drawn for each level of \code{facet}, in contrast
        to \code{by}, which overlays subgroups on the same panel.}
  \item{n_row}{Number of rows in the facet layout. If specified, \code{n_col}
        is determined automatically and should not be set simultaneously.}
  \item{n_col}{Number of columns in the facet layout. If specified, \code{n_row}
        is determined automatically and should not be set simultaneously.
        When \code{n_col = 1}, facet strips are placed to the left of panels
        instead of above.}
  \item{aspect}{Lattice aspect ratio for facet panels, defined as height/width.
        Default \code{"fill"} expands panels to occupy available space.
        Set to \code{1} for square panels or \code{"xy"} to bank lines to an
        effective slope of 45 degrees.}

  \item{horiz}{Orientation of bars in a bar chart. Defaults to \code{FALSE}
        (vertical bars) unless \code{one_plot = TRUE}, in which case horizontal
        bars are often more readable.}
  \item{sort}{Sorting strategy for bar categories.
        Default \code{"0"} retains the original order.
        Use \code{"-"} for descending and \code{"+"} for ascending order of
        frequencies (for one-way charts) or column sums (with \code{by}).
        Not applicable to facet plots. When \code{one_plot = TRUE}, the default
        is \code{"+"}.}
  \item{beside}{For a two-way bar chart, if \code{TRUE} plots the levels of
        the second variable as adjacent bars (grouped bars) rather than stacked
        segments.}
  \item{stack100}{Produces a 100\% stacked bar chart when a \code{by} variable
        is present, equivalent to setting \code{stat_x = "proportion"}
        with \code{by}.}
  \item{gap}{Controls the spacing between bars; passed to the \code{space}
        argument of \code{\link{barplot}}. Default is \code{0.2}, except for
        two-variable plots with \code{beside = TRUE}, where the default is
        \code{c(0.1, 1)}.}
  \item{scale_y}{Optional numeric vector of length three defining the y-axis
        (numeric axis) scale: minimum, maximum, and number of intervals.
        Applies to bar and similar charts.}
  \item{one_plot}{For multiple \code{x} variables, selects whether to draw
       a separate bar chart for each variable or combine all variables
       into a single multi-item chart. By default, if variables share a
       common response scale (e.g., Likert items), \code{one_plot} is set to
       \code{TRUE}; otherwise it defaults to \code{FALSE}.}

  \item{theme}{Color \bold{theme} for this analysis. Use \code{\link{style}}
        to set persistent defaults across analyses.}
  \item{fill}{Fill color(s) for bars, pie slices, tiles, or bubbles.
        Default is the qualitative \code{"hues"} palette under the
        \code{"colors"} theme, or an ordered sequential palette (e.g., \code{"blues"})
        for ordinal categories. For other themes, default fill is taken from
        the corresponding gradient (e.g., \code{"reds"} for \code{"darkred"}).
        May also be any vector of colors (e.g., from \code{\link{getColors}})
        or predefined palettes including color-blind–safe options such as
        \code{"viridis"}. When \code{fill} is set to the name of \code{y}
        (or \code{(count)} for tabulated counts), values of \code{y} are
        mapped to a color scale. Not used when \code{fill_split} is active.}
  \item{color}{Border color of plotted objects (bars, slices, bubbles, tiles).
        May be a vector to vary borders by category. Default is
        \code{bar_color_discrete} from \code{\link{style}}.}
  \item{transparency}{Transparency of filled areas, from \code{0} (opaque) to
        \code{1} (fully transparent). Default is \code{trans_bar_fill} from
        \code{\link{style}}.}
  \item{fill_split}{For bar charts, splits bars into two fill colors relative
        to a numeric threshold. Bars with \code{y <= fill_split} are drawn in
        the first fill color; larger values use the second. Alternatively, supply
        a length-2 vector of colors.}
  \item{fill_scaled}{For bar charts without a \code{by} variable, scales the
        lightness of the fill color according to height (the value of \code{y}).
        Larger values yield darker bars. When \code{fill} is a single color,
        a sequential scale is generated; when \code{fill} is two colors, a
        diverging scale is used.}
  \item{fill_chroma}{Chroma (saturation) for \code{fill_scaled} bars.
        Full saturation is 100; lower values approach grayscale.
        Has no effect for the \code{"gray"} theme, which is already achromatic.}

  \item{labels}{Adds numeric labels to bars or pie slices.
        Default \code{"\%"} displays percentages,
        \code{"prop"} shows proportions,
        and \code{"input"} shows the underlying numeric values (counts or
        supplied \code{y}). If \code{y} is omitted, the input values are
        the tabulated counts.}
  \item{labels_position}{Position of labels for pies/sunbursts.
        Default is \code{"in"} (inside slices); use \code{"out"} to place
        labels outside.}
  \item{labels_color}{Color(s) of the plotted labels. May be a vector; if fewer
        colors are given than categories, colors are recycled.}
  \item{labels_size}{Character expansion factor for label text.
        Default is \code{0.95}, or \code{0.9} of that value when
        \code{beside = TRUE} and \code{labels_position = "in"} (to account
        for narrower bars).}
  \item{labels_decimals}{Number of decimal places displayed in labels.
        Defaults to 0 for integer-valued \code{y} and 2 for \code{"prop"}.}
  \item{labels_cut}{Minimum relative size required to show a label.
        When \code{labels_position = "out"}, the default is \code{0.028} for
        simple charts, and \code{0.040} when a \code{by} variable is present
        or multiple \code{x} variables are combined.}

  \item{xlab}{\bold{Axis label} for the \code{x}-axis. If omitted, the label
       is taken from the variable label (if present) or the variable name.
       If \code{xy_ticks = FALSE}, no x-axis label is drawn.
       When no \code{y} is specified, \code{xlab} defaults to \code{"Index"}
       unless explicitly set.}
  \item{ylab}{Axis label for the \code{y}-axis. If omitted, the label is taken
       from the variable label (if present) or the variable name.
       If \code{xy_ticks = FALSE}, no y-axis label is drawn.}
  \item{main}{Title of the chart. Size and color may be controlled via
       \code{main_cex} and \code{main_color} in \code{\link{style}}.}
  \item{sub}{Subtitle placed below \code{xlab}. Not yet implemented.}
  \item{lab_adjust}{Two-element numeric vector (x-label, y-label) giving
       approximate inch offsets for axis labels. Positive values move labels
       away from the plotting region. Not applicable to facet (Trellis) plots.}
  \item{margin_adjust}{Four-element numeric vector (top, right, bottom, left)
       that adjusts plot margins in inches. Positive values expand the
       corresponding margin. Not applicable to facet plots.}
  \item{pad_y_min}{Proportion of padding added at the lower end of the
       \code{y}-axis (0--1).}
  \item{pad_y_max}{Proportion of padding added at the upper end of the
       \code{y}-axis (0--1).}

  \item{rotate_x}{For bar charts, rotation (in degrees) of category labels
        on the \code{x}-axis, typically used to accommodate long labels in
        combination with \code{offset}. When \code{rotate_x = 90}, labels are
        vertical and an alternative placement algorithm is used, so
        \code{offset} is usually unnecessary.}
  \item{rotate_y}{Applies to BPFM (bubble plot frequency matrix), a sequence
        of stacked bubble charts. Controls rotation of labels along the
        vertical axis.}
  \item{break_x}{For bar charts, controls automatic line-breaking of category
        labels. When \code{TRUE}, spaces are converted to new lines and tildes
        to blanks (keeping words joined by a tilde on the same line).
        Defaults to \code{TRUE} for vertical bars with \code{rotate_x = 0},
        and \code{FALSE} otherwise.}
  \item{offset}{For bar charts, controls the spacing between axis labels and
        the axis itself. Default is \code{0.5}. Larger values (e.g., \code{1.0})
        create additional room for rotated or long labels.}
  \item{axis_fmt}{Numeric format for axis labels. Default \code{"K"} shows
        thousands as \code{"K"} (e.g., \code{100000} as \code{100K}).
        Alternatives include \code{","} (comma separators with decimal point),
        \code{"."} (period separators), or \code{""} to disable formatting.}
  \item{axis_x_pre}{Prefix for labels on the \code{x}-axis, such as \code{"$"}.}
  \item{axis_y_pre}{Prefix for labels on the \code{y}-axis, such as \code{"$"}.}
  \item{label_max}{For bar charts, improves console readability of text output
        by setting a target maximum label length. Longer labels are abbreviated
        in the printed frequency distribution. The limit is not strict when
        necessary to preserve uniqueness.}

  \item{legend_title}{Title of the \bold{legend}. Usually set automatically
        from variable names, but must be supplied explicitly when plotting
        raw count matrices without variable metadata.}
  \item{legend_position}{Legend placement when plotting two variables.
        Default is in the right margin. Standard positions such as
        \code{"topleft"}, \code{"top"}, and \code{"topright"} are also
        available; see \code{\link{legend}}.}
  \item{legend_labels}{Legend labels when plotting two variables.
        Defaults to the levels of the second (or \code{by}) variable.}
  \item{legend_horiz}{If \code{TRUE}, draws the legend horizontally;
        default is vertical.}
  \item{legend_size}{Character expansion factor for legend text.}
  \item{legend_abbrev}{If specified, truncates legend title and labels to at most
        the given number of characters (subject to preserving uniqueness).}
  \item{legend_adjust}{Horizontal shift of the legend in two-way bar charts.
        Positive values move the legend to the right from its default position.}

  \item{add}{For bar charts, \bold{overlays} additional objects (text or
       geometric figures) on the plot.
       The first argument \code{"text"} writes arbitrary text; geometric options
       include \code{"rect"}, \code{"line"}, \code{"arrow"},
       \code{"v_line"} (vertical line), and \code{"h_line"} (horizontal line).
       The value \code{"means"} is shorthand for vertical and horizontal
       lines at the respective means. Does not apply to facet plots.
       Use \code{\link{style}} parameters such as \code{add_fill} and
       \code{add_color} to control appearance.}
  \item{x1}{First x-coordinate (in standardized \code{-1} to \code{1} units)
       for each added object.}
  \item{y1}{First y-coordinate for each added object.}
  \item{x2}{Second x-coordinate for each added object.
        Used for \code{"rect"}, \code{"line"}, and \code{"arrow"}.}
  \item{y2}{Second y-coordinate for each added object.
        Used for \code{"rect"}, \code{"line"}, and \code{"arrow"}.}

  \item{quiet}{If \code{TRUE}, suppresses text output to the console.
        The default can be changed via \code{\link{style}}.}
  \item{do_plot}{If \code{TRUE} (default), produces the chart. Set to
        \code{FALSE} to compute and return results without plotting.}
  \item{use_plotly}{If \code{TRUE} (default), produces a Plotly-based
        interactive chart in the RStudio \code{Viewer} window in addition
        to the static plot in the \code{Plots} window. Some advanced options
        apply only to the static chart.}
  \item{pdf_file}{If specified, directs graphics output to a PDF file with
        this name.}
  \item{width}{Width of the plot window (or PDF device) in inches.
        Default is \code{4.5}.}
  \item{height}{Height of the plot window (or PDF device) in inches.
        Default is \code{4.5}.}
  \item{digits_d}{Number of decimal digits used for displayed numeric
        summaries. Defaults to at least 2 or one more than the maximum number
        of digits in the response variable, whichever is larger.}
  \item{out_size}{Target maximum line width (in characters) for console
       frequency tables of a single variable. Longer lines trigger a vertical
       layout for improved readability.}

  \item{n_cat}{For analyses of all variables in a data frame, sets the maximum
       number of unique values for a numeric variable to be treated as
       categorical rather than continuous. Default is \code{0}.
       \strong{Deprecated}: It is preferable to convert such variables explicitly
       to factors.}
  \item{value_labels}{For factors, defaults to factor levels; for character
        variables, defaults to the character values.
        May be used to override axis labels on the \code{x}-axis.
        If the variable is a factor and \code{value_labels} is \code{NULL},
        levels are used with embedded spaces replaced by line breaks.
        If \code{x} and \code{y} share the same scale, labels may also be
        used on the \code{y}-axis. Label size is controlled via
        \code{axis_cex} and \code{axis_x_cex} in \code{\link{style}}.}
  \item{rows}{\strong{Deprecated}. Old name for \code{filter}.}
  \item{facet1}{\strong{Deprecated}. Old parameter name, replaced by
        \code{facet}.}

  \item{eval_df}{Controls whether the function checks for existence of
        \code{data} and referenced variables. Defaults to \code{TRUE}, except
        when \pkg{shiny} is loaded, in which case it is set to \code{FALSE} so
        that Shiny applications run without conflict. Set to \code{FALSE}
        when using the pipe operator \code{\%>\%}.}
  \item{fun_call}{Function call object used internally (e.g., by \pkg{knitr})
        to reconstruct the original call.}

  \item{\dots}{Additional graphical parameters passed to base \code{\link{barplot}},
      \code{\link{legend}}, and \code{\link{par}}.
      Common options include \code{cex.main} (title size),
      \code{col.main} (title color), line types such as
      \code{"dotted"} or \code{"dotdash"},
      and subtitle options \code{sub} and \code{col.sub}.
      Axis label orientation can be adjusted with \code{las = 3},
      and bar spacing with \code{space} in one-variable bar charts.}
}


\details{
\strong{OVERVIEW}

\code{Chart()} visualizes numerical values associated with one or two categorical variables, each with a relatively small number of levels. By default, colors for bars, background, and grid lines are taken from the active \code{\link{style}} theme, but all can be customized. Base computations use standard R functions such as \code{\link{barplot}}, \code{\link{chisq.test}}, and, for two variables, \code{\link{legend}}. For horizontal bar charts (\code{horiz = TRUE}), category labels are drawn horizontally and the left margin is automatically extended to accommodate both the labels and the axis title.\cr

\strong{DATA}

Conceptually, the chart is built from a summary table in which each row consists of a level of the categorical variable \code{x} paired with a numerical value \code{y}, with as many rows as there are levels of \code{x}. You may:
\itemize{
\item supply \code{x} and \code{y} directly as a pre-aggregated summary table, or
\item supply \code{x} (and optionally \code{y}) at the observation level and let \code{Chart()} aggregate over the levels of \code{x} (and \code{by}) using \code{stat}.
}
A second categorical variable \code{by} can be used to form a two-way table.

The \code{filter} parameter subsets rows (cases) of the input data frame according to a logical expression or a set of integers that specify the row numbers to retain. Use the standard R logical operators described in \code{\link{Logic}}, such as \code{\&} (and), \code{|} (or), and \code{!} (not), and the standard relational operators described in \code{\link{Comparison}}, such as \code{==} (equality), \code{!=} (not equal), and \code{>} (greater than). Alternatively, specify a vector of integers that correspond to row numbers. See the Examples.

The input can be factors, numeric values, characters, or a matrix. You can:
\itemize{
\item enter raw data and let \code{Chart()} compute frequencies or summaries, or
\item enter a pre-tabulated summary table of counts or statistics.
}
When \code{y} is not supplied, the numerical values are simply the counts of each level of \code{x} (and of each combination of \code{x} and \code{by}).\cr

\strong{TWO DATA MODES FOR PLOTLY OUTPUTS}

\code{Chart()} supports two conceptual modes for aggregated values used in the plots and tables:
\itemize{
\item \strong{Count mode} (default): if \code{y} is \code{NULL}, the chart uses counts of \code{x} (and \code{by}, when supplied).
\item \strong{Summary mode}: if \code{y} is numeric, the chart aggregates \code{y} over the categories of \code{x} (and \code{by}) using \code{stat}.
}

From full data with repeated values of \code{x} (and \code{by}), you can reduce to a summary table using one of the following transformations:

\tabular{ll}{
Transformation \tab Meaning\cr
-------------- \tab -------------------\cr
\code{"sum"} \tab sum\cr
\code{"mean"} \tab mean\cr
\code{"sd"} \tab standard deviation\cr
\code{"dev"} \tab mean deviation\cr
\code{"min"} \tab minimum\cr
\code{"median"} \tab median\cr
\code{"max"} \tab maximum\cr
------------- \tab -------------------\cr
}

All numeric values (both in console tables and Plotly hovers) are formatted according to \code{digits_d}.

Before plotting, \code{Chart()} constructs a 1-D table (\code{x}) or 2-D table (\code{by} × \code{x}) of either counts (when \code{y = NULL}) or aggregated \code{y} (when \code{y} is supplied). For count mode, \code{Chart()} prints the frequency table and a chi-square test of equal probabilities. For summary mode, it prints the aggregated table (no chi-square test is computed for numeric summaries). These tables serve as a concise audit of the data supplied to the visualization.\cr

\strong{NON-HIERARCHICAL PLOTLY CHARTS}

For non-hierarchical charts (\code{type = "bar"}, \code{"radar"}, or \code{"bubble"}):
\itemize{
\item with \code{y = NULL}, the geometry encodes counts;
\item with \code{y} supplied, the geometry encodes the chosen \code{stat} of \code{y} per category (and per group when \code{by} is supplied).
}
In bubble charts, bubble size is proportional to the aggregated value. Use \code{radius} (in pixels) and \code{power} to control size mapping (area proportional to the value when \code{power = 0.5}).\cr

\strong{HIERARCHICAL PLOTLY CHARTS}

Hierarchical charts include pies with a \code{by} variable (sunburst charts) and \code{type = "treemap"} or \code{"icicle"}. For these charts, \code{Chart()} constructs a path table from \code{x} and \code{by}, where \code{by} may be:
\itemize{
\item a single factor (one additional level), or
\item a data frame of multiple factors, where each column represents a deeper level in the hierarchy.
}
It then aggregates \code{y} (or counts, if \code{y = NULL}) along the path:

\itemize{
\item Node values are computed by applying \code{stat} within each node to its child records.
\item For \strong{additive} statistics (\code{"sum"} and counts), parent node values equal the sum of their children. Children are sized proportionally to their parent (\code{branchvalues = "total"}), so hover percentages of parent/root are well defined.
\item For \strong{non-additive} statistics (\code{"mean"}, \code{"median"}, \code{"min"}, \code{"max"}, \code{"sd"}), parent values are computed at the parent level using the same \code{stat} and are not sums of children. In this case, “\% of parent/root” is not shown in hovers because these proportions are not meaningful for non-additive summaries.
}

All numeric values shown in hovers are formatted using \code{digits_d}. For hierarchical charts:
\itemize{
\item additive modes show the aggregated value and, when appropriate, the \% of parent and \% of root;
\item non-additive modes show the aggregated value only.
}

Titles for console output and interactive plots reflect the mode:
\itemize{
\item Count mode: e.g., “Count of \code{x}” (optionally “by \code{by}”).
\item Summary mode: e.g., “\code{stat} of \code{y} by \code{x}” (and “by \code{by}” when grouped).
\item Hierarchical: analogous titles using the same \code{stat} and variable names.
}

In all cases, \code{Chart()} preserves factor level order when building 1-D and 2-D tables and prints tables with informative dimnames. The console chi-square test is computed only for count mode (1-D or 2-D).\cr

\strong{VECTOR OF x-VALUES}

A vector of categorical \code{x}-variables (character or factor) generalizes to a matrix of one-dimensional plots, depending on the value of \code{type}:
\itemize{
\item for \code{type = "bar"}, a stacked bar chart (stack of one-dimensional bar plots),
\item for \code{type = "bubble"}, a stacked bubble chart, referred to as a \emph{bubble plot frequency matrix} (BPFM).\cr
}

\strong{COLORS}

For a one-variable plot, the default bar colors are taken from the current theme via the \code{bar_fill_discrete} argument of \code{\link{style}}, which by default uses the qualitative HCL palette \code{"hues"}. Alternatively, set the bar colors explicitly with the \code{fill} parameter, using:
\itemize{
\item a single color,
\item a palette name, or
\item a vector of colors, e.g., from \code{\link{getColors}}.
}

Pre-defined sequential and divergent HCL ranges are available through \code{\link{getColors}}. The qualitative sequence \code{"hues"} provides equally spaced HCL colors (same chroma and luminance). Sequential and divergent ranges are available at 30-degree increments around the HCL color wheel, including \code{"reds"}, \code{"rusts"}, \code{"browns"}, \code{"olives"}, \code{"greens"}, \code{"emeralds"}, \code{"turquoises"}, \code{"aquas"}, \code{"blues"}, \code{"purples"}, \code{"violets"}, \code{"magentas"}, and \code{"grays"}.

Define a \emph{divergent color scale} by providing a vector of two such ranges to \code{fill}, e.g., \code{c("purples", "rusts")}. These are especially useful for multiple bar charts with a common response scale (e.g., Likert items). Alternatively, specify colors manually, such as \code{c("coral3", "seagreen3")} for a two-level \code{by} variable.

For finer control, call \code{\link{getColors}} explicitly and pass its result to \code{fill}, adjusting chroma (\code{c}) and luminance (\code{l}), or defining a custom hue (\code{h}). See \code{\link{getColors}} for details.

The values of another variable can be mapped to bar fill by setting \code{fill} equal to that variable’s name, typically \code{y} when supplied. When \code{y} is tabulated, refer to it as \code{(count)}. Larger values produce darker bars.

Additional pre-specified palettes include \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Distinct palettes include \code{"distinct"} (maximally separated hues), the viridis family (\code{"viridis"}, \code{"cividis"}, \code{"magma"}, \code{"inferno"}, \code{"plasma"}), and color-blind friendly options such as \code{"Okabe-Ito"}. Wes Anderson–inspired palettes such as \code{"Moonrise1"}, \code{"Royal1"}, \code{"GrandBudapest1"}, \code{"Darjeeling1"}, and \code{"BottleRocket1"} are also available (with variants using \code{2} or \code{3} in the name where defined).\cr

\strong{LEGEND}

When two variables are plotted, a legend is produced with entries for each level of the second or \code{by} variable. By default, the legend is placed in the right margin. This position can be changed with \code{legend_position}, which accepts \code{"right_margin"} and any valid position accepted by the standard R \code{\link{legend}} function.

The legend title can be abbreviated with \code{legend_abbrev}, which specifies the maximum number of characters. The legend is vertical by default, but can be drawn horizontally with \code{legend_horiz}.\cr

\strong{LONG CATEGORY NAMES}

Category labels are often long. Adjust their display with \code{rotate_x} and \code{rotate_y}, in conjunction with \code{offset}, which moves labels away from the axis to compensate for rotation. These settings can be made persistent with \code{\link{style}}. To reset to defaults, call \code{style()} again.

Spacing codes for category names:
\enumerate{
\item Any space in a category name is converted to a new line in the plotted label.
\item To keep words on the same line, replace the space with a tilde \code{~}; the tilde is rendered as a space without a line break.
}

For console output, you can limit label length with \code{label_max}. Longer names are abbreviated to the specified number of characters, with a mapping table provided to show the correspondence between abbreviated and full names. For one-variable frequency distributions, \code{out_size} controls the maximum line width before the distribution is printed vertically instead of horizontally.\cr

\strong{MULTIPLE BAR CHARTS ON THE SAME PANEL (PLOT)}

For multiple \code{x}-variables, set \code{one_plot = TRUE} to overlay individual bar charts on a single panel. This is especially useful when all items share a common response scale (e.g., Likert items). By default, \code{Chart()} produces a single-panel display when a common response scale is detected.

The algorithm for detecting a common response scale identifies the variable with the largest set of responses, then checks that all other variables’ responses are contained within that set. Some items may not exhibit all possible responses (e.g., no one chooses “Strongly Disagree”), but as long as at least one variable contains the full response set, the scales are treated as common.

Regardless of this automatic detection, you can explicitly set \code{one_plot} to either \code{TRUE} or \code{FALSE}. Explicitly setting \code{one_plot} bypasses the commonality check and saves computation.\cr

\strong{ENTER NUMERIC VARIABLE DIRECTLY}

Instead of computing counts from raw data, you can enter a numeric variable directly as \code{y}, together with a categorical \code{x} (and possibly a categorical \code{by}). In this case, the chart uses the supplied numeric values as-is (or aggregates them according to \code{stat}). Alternatively, you can read a pre-tabulated table of counts into R as a matrix or data frame and pass it to \code{Chart()}.\cr

\strong{STATISTICS}

In addition to the Plotly and static charts, descriptive and optional inferential statistics are reported. For count mode, a frequency table (one variable) or joint frequency table (two variables) is displayed, followed by Cramér’s V and the chi-square test of independence (or equal probabilities) by default. For summary mode, the aggregated table is printed without a chi-square test, as the test is not appropriate for numeric summaries.\cr

\strong{VARIABLE LABELS}

If variable labels are stored in the data frame (e.g., via \code{\link{Read}} or \code{\link{VariableLabels}}), they are used by default as axis labels and in text output. For a single variable, the \code{x}-axis label defaults to the variable label unless \code{xlab} is explicitly supplied. For two variables, the plot title is derived from both variable labels unless overridden by \code{main}. Variable labels are also shown in the printed tables.\cr

\strong{PDF OUTPUT}

To write graphics to a PDF file, use \code{pdf_file}, optionally with \code{width} and \code{height}. Files are written to the current working directory, which you can explicitly set with \code{\link{setwd}}.\cr

\strong{ONLY VARIABLES ARE REFERENCED}

Arguments that denote variables in \code{Chart()} (and other \code{lessR} functions) must be names of existing variables, either in the referenced data frame (e.g., the default \code{d}) or in the user’s workspace (global environment). Expressions are not evaluated directly. For example:

\code{    > Chart(cut(rnorm(50)))   # does NOT work}

Instead, assign the expression to a variable and reference that variable:

\preformatted{    > Y <- cut(rnorm(50))   # create vector Y in user workspace
    > Chart(Y)                            # directly reference Y}
}


\value{
For interactive visualizations, \code{Chart()} returns a Plotly
\code{htmlwidget} object (class \code{plotly}) that can be printed for
interactive viewing or saved as a self-contained HTML file.

For standard (non-interactive) charts, the output can optionally be
saved as an \R object. Otherwise, it appears only in the console (unless
\code{quiet = TRUE}). Two types of components are provided:
\emph{readable text output} and \emph{numerical statistics}.

The readable output consists of character strings such as frequency or
summary tables suitable for display. The numerical components are
statistics amenable to further analysis. This design supports
reproducible reporting in R Markdown documents by referencing the name
of each output component directly, using the syntax
\code{object$component}.

Each component appears only when relevant to the current analysis. For
example, cell proportions (\code{out_prop}) are included only for
two-way tables.

Example: save the output of a standard chart to an object with any valid
R name, such as \code{b <- Chart(Dept)}. View the available output
elements with \code{names(b)}, and access a specific component by
prefixing with the object name, such as \code{b$out_chi} to display the
chi-square test results. These objects can be displayed directly in the
console or within R Markdown for integrated text and analysis.

\strong{Bar charts only: tabulated numerical variable \code{y}}

When \code{Chart()} is used as a bar chart with a tabulated numerical
variable (counts or proportions), the object may contain:

\subsection{Readable output}{
\describe{
  \item{\code{out_title}}{Title of the analysis.}
  \item{\code{out_lbl}}{Variable label.}
  \item{\code{out_counts}}{Frequency or two-way frequency distribution.}
  \item{\code{out_chi}}{Chi-square test of equal probabilities
        (one variable) or independence (two variables).}
  \item{\emph{One variable} \code{out_miss}}{Number of missing values.}
  \item{\emph{Two variables} \code{out_prop}}{Cell proportions.}
  \item{\emph{Two variables} \code{out_row}}{Row-wise cell proportions.}
  \item{\emph{Two variables} \code{out_col}}{Column-wise cell proportions.}
}
}

\subsection{Statistics}{
\describe{
  \item{\code{n_dim}}{Number of dimensions, 1 or 2.}
  \item{\code{p_value}}{p-value for the null hypothesis of equal
        proportions (one variable) or independence (two variables).}
  \item{\code{freq}}{Data frame of the frequency distribution.}
  \item{\emph{One variable} \code{values}}{y-values read directly.}
  \item{\emph{One variable} \code{prop}}{Frequency distribution of
        proportions.}
  \item{\emph{One variable} \code{n_miss}}{Number of missing values.}
}
}

\strong{Numerical variable \code{y} read from data}

When \code{Chart()} reads a numeric variable \code{y} directly from the
data and summarizes it across one or two categorical variables, the
returned object can include:

\describe{
  \item{\code{out_y}}{Values of \code{y} used in the analysis.}
  \item{\code{n_dim}}{Number of dimensions, 1 or 2.}
}
}

\references{
Gerbing, D. W. (2023). \emph{R Data Analysis without Programming: Explanation and Interpretation}, 2nd edition, Chapter 4, NY: Routledge.

Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 3, NY: CRC Press.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266, https://www.tandfonline.com/doi/abs/10.1080/26939169.2021.1999871. 

Sievert, C. (2020). \emph{Interactive Web-Based Data Visualization with R, plotly, and shiny}. Chapman and Hall/CRC. URL: \url{https://plotly.com/r/}
}

\author{
David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})
}

\seealso{\code{\link{X}}, \code{\link{XY}},
\code{\link{getColors}}, \code{\link{barplot}}, \code{\link{table}}, \code{\link{legend}}, \code{\link{savePlotly}}.
}


\examples{

# get the data
d <- Read("Employee")

# --------------------------------------------------------
# bar chart from tabulating the data for a single variable
# --------------------------------------------------------

# for each level of Dept, display the frequencies
# -----------------------------------------------
# bar chart, standard and plotly
  Chart(Dept)  # bar chart by default

  # radar chart, plotly only
  Chart(Dept, type="radar") 

  # bubble chart, plotly only
  Chart(Dept, type="bubble") 

  # pie chart, plotly only
  Chart(Dept, type="pie") 

  # treemap chart, plotly only
  Chart(Dept, type="treemap") 

#  the values output by BarChart into the myOutput list
myOutput <- Chart(Dept)
# display the saved output
myOutput

# just males with salaries larger than 75,000 USD
Chart(Dept, filter=(Gender=="M" & Salary > 85000))

# rotate and offset the axis labels, sort categories by frequencies
Chart(Dept, rotate_x=45, offset=1, sort="-")

# set bars to a single color of blue with some transparency
Chart(Dept, fill="blue", transparency=0.3)
# progressive (sequential) color scale of blues
Chart(Dept, fill="blues")

# viridis palate
Chart(Dept, fill="viridis")

# change the theme just for this analysis, as opposed to style()
Chart(Dept, theme="darkgreen")

# set bar color to hcl custom hues with chroma and luminance
#   at the values provided by the default hcl colors from
#   the getColors function, which defaults to h=240 and h=60
#   for the first two colors on the qualitative scale
Chart(Gender, fill=c(hcl(h=180,c=100,l=55), hcl(h=0,c=100,l=55)))

# or set to unique colors via color names
Chart(Gender, fill=c("palegreen3","tan"))

# darken the colors with an explicit call to getColors,
#   do a lower value of luminance, set to l=25
Chart(Dept, fill=getColors(l=25), transparency=0.4)

# column proportions instead of frequencies
Chart(Gender, stat_x="proportion")

# map value of tabulated count to bar fill
Chart(Dept, fill=(count))

# data with many values of categorical variable Make and large labels
myd <- Read("Cars93")
# perpendicular labels
Chart(Make, rotate_x=90, data=myd)
# manage size of horizontal value labels
Chart(Make, horiz=TRUE, label_max=4, data=myd)

# read y variable, Salary
# display bars for values of count <= 0 in a different color
#  than values above
Chart(Dept, y=Salary, stat="dev", sort="+", fill_split=0)

# scale the luminosity of the bars with the sequential scale
Chart(Dept, y=Salary, stat="deviation", sort="+",
      fill_scaled=TRUE, fill="green")

# scale the luminosity of the bars with a divergent scale
Chart(Dept, y=Salary, stat="deviation", sort="+", fill_scaled=TRUE,
         fill=c("red", "blue"))

# ----------------------------------------------------
# bar chart from tabulating the data for two variables
# ----------------------------------------------------

# at each level of Dept, show the frequencies of the Gender levels
  # bar chart, standard and plotly
  Chart(Dept, by=Gender)  # bar chart by default

  # radar chart, plotly only
  Chart(Dept, by=Gender, type="radar") 

  # bubble chart, plotly only
  Chart(Dept, by=Gender, type="bubble") 

  # pie chart, plotly only
  Chart(Dept, by=Gender, type="pie") 

  # treemap chart, plotly only
  Chart(Dept, by=Gender, type="treemap") 
# --------------------------------------

# Trellis (facet) plot, bar chart only
Chart(Dept, facet=Gender)

# at each level of Dept, show the row proportions of the Gender levels
#   i.e., 100% stacked bar graph
Chart(Dept, by=Gender, stack100=TRUE)

# at each level of Gender, show the frequencies of the Dept levels
# do not display percentages directly on the bars
Chart(Gender, by=JobSat, fill="reds", labels="off")

# specify two fill colors for Gender
Chart(Dept, by=Gender, fill=c("deepskyblue", "black"))

# display bars beside each other instead of stacked, Female and Male
# the levels of Dept are included within each respective bar
# plot horizontally, display the value for each bar at the
#   top of each bar
Chart(Gender, by=Dept, beside=TRUE, horiz=TRUE, labels_position="out")

# horizontal bar chart of two variables, put legend on the top
Chart(Gender, by=Dept, horiz=TRUE, legend_position="top")

# for more info on base R graphic options, enter:  help(par)
# for lessR options, enter:  style(show=TRUE)
# here fill is set in the style function instead of BarChart
#   along with the others
style(fill=c("coral3","seagreen3"), lab_color="wheat4", lab_cex=1.2,
      panel_fill="wheat1", main_color="wheat4")
Chart(Dept, by=Gender,
         legend_position="topleft", legend_labels=c("Girls", "Boys"),
         xlab="Dept Level", main="Gender for Different Dept Levels",
         value_labels=c("None", "Some", "Much", "Ouch!"))
style()


# -------------------------------------------------------------------------
# bar chart from a statistic aggregated across 1 or 2 categorical variables
# -------------------------------------------------------------------------
Chart(Dept, y=Salary, stat="mean")

Chart(Dept, by=Gender, y=Salary, stat="mean")

# -----------------------------------------------------------------
# multiple bar charts tabulated from data across multiple variables
# -----------------------------------------------------------------

# bar charts for all non-numeric variables in the data frame called d
#   and all numeric variables with a small number of values, < n_cat
# BarChart(one_plot=FALSE)

d <- rd("Mach4", quiet=TRUE)

# stacked bar charts for 20 6-pt Likert scale items
# default scale is divergent from "browns" to "blues"
Chart(m01:m20, horiz=TRUE, labels="off", sort="+")

# stacked bubble charts for 20 6-pt Likert scale items
Chart(m01:m20, type="bubble")


\donttest{

# custom scale with explicit call to getColors, HCL chroma at 50
clrs <- getColors("greens", "purples", c=50)
Chart(m01:m20, horiz=TRUE, labels="off", sort="+", fill=clrs)

# custom divergent scale with pre-defined color palettes
#  with implicit call to getColors
Chart(m01:m20, horiz=TRUE, labels="off", fill=c("aquas", "rusts"))


# ----------------------------
# can enter many types of data
# ----------------------------

# generate and enter integer data
X1 <- sample(1:4, size=100, replace=TRUE)
X2 <- sample(1:4, size=100, replace=TRUE)
Chart(X1)
Chart(X1, by=X2)

# generate and enter type double data
X1 <- sample(c(1,2,3,4), size=100, replace=TRUE)
X2 <- sample(c(1,2,3,4), size=100, replace=TRUE)
Chart(X1)
Chart(X1, by=X2)

# generate and enter character string data
# that is, without first converting to a factor
Travel <- sample(c("Bike", "Bus", "Car", "Motorcycle"), size=25, replace=TRUE)
Chart(Travel, horiz=TRUE)


# ----------------------------
# bar chart directly from data
# ----------------------------

# include a y-variable, here Salary, in the data table to read directly
d <- read.csv(text="
Dept, Salary
ACCT,51792.78
ADMN,71277.12
FINC,59010.68
MKTG,60257.13
SALE,68830.06", header=TRUE)
Chart(Dept, y=Salary)

# specify two variables for a two variable bar chart
# also specify a y-variable to provide the counts directly
# when reading y values directly, must be a summary table,
#   one row of data for each combination of levels with
#   a numerical value of y
# use lessR pivot function to get summary table, cannot process missing data
#   so set na_show_group to FALSE
d <- Read("Employee")
a <- pivot(d, mean, Salary, c(Dept,Gender), na_group_show=FALSE)
Chart(Dept, y=Salary_mean, by=Gender, data=a)
# do so just with BarChart, display bars in grayscale
# How does average salary vary by gender across the various departments?
Chart(Dept, y=Salary, by=Gender, stat="mean", data=d, fill="grays")


# -----------
# annotations
# -----------

d <- rd("Employee")

# Place a message in the center of the plot
# \n indicates a new line
Chart(Dept, add="Employees by\nDepartment", x1=3, y1=10)

# Use style to change some parameter values
style(add_trans=.8, add_fill="gold", add_color="gold4", add_lwd=0.5)
# Add a rectangle around the message centered at <3,10>
Chart(Dept, add=c("rect", "Employees by\nDepartment"),
                     x1=c(2,3), y1=c(11, 10), x2=4, y2=9)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ chart }
\keyword{ bar chart }
\keyword{ pie chart }
\keyword{ sunburst }
\keyword{ treemap }
\keyword{ radar chart }
\keyword{ bubble chart }
\keyword{ color }

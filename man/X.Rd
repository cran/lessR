\name{X}
\alias{X}
\alias{Histogram}
\alias{hs}
\alias{Density}
\alias{BoxPlot}
\alias{bx}
\alias{ViolinPlot}
\alias{vp}

\title{Univariate and Grouped Displays for a Continuous Variable}

\description{

\code{lessR} introduces the concept of a \emph{data view} visualization function, in which the choice of visualization function directly reflects the structure of the data and the analyst's intent for understanding the data. The function \code{X()} visualizes the distribution of a continuous variable along with
associated summary statistics. The specific representation is selected with the
\code{type} argument:

\itemize{
  \item Histogram with \code{type = "histogram"} (default)
  \item Density plot with \code{type = "density"}
  \item Box plot with \code{type = "box"}
  \item Violin plot with \code{type = "violin"}
  \item Strip (one-dimensional scatter) plot with \code{type = "strip"}
  \item Integrated violin-box-strip (VBS) plot with \code{type = "vbs"}
}

Stratification divides the distribution into groups.
Use \code{by} to overlay the groups within a single panel, or \code{facet} to
display each group, or combination of groups, in separate panels.

When running in RStudio, static displays appear in the
\code{Plots} window, while histograms and density plots also appear as
interactive Plotly visualizations in the \code{Viewer} window.
}

\usage{
X(

    # ----------------------------------------------
    # Data from which to construct the visualization
    x=NULL, by=NULL, facet=NULL, data=d, filter=NULL,

    # ------------------------------------------
    # Type of plot for the continuous variable x
    type = c("histogram", "freq_poly", "density",
             "violin", "box", "strip", "bs", "vbs"), # violin, box, strip
    stat=c("count", "proportion", "density"),

    n_row=NULL, n_col=NULL, aspect="fill",

    # -----------------------------------------------------------------
    # Analogy of physical Marks on paper that create the bars and labels
    theme=getOption("theme"),
    fill=getOption("bar_fill_cont"),
    color=getOption("bar_color_cont"),
    transparency=getOption("trans_bar_fill"),

    # -----------------------------------------------------
    # Form of the histogram - bin the continuous variable x
    counts=FALSE, 
    bin_start=NULL, bin_width=NULL, bin_end=NULL, breaks="Sturges",
    cumulate=c("off", "on", "both"), reg="snow2",  # Cumulative histogram

    # -----------------------------------------------------
    # Density (smooth curve) plot
    show_histogram=TRUE,
    bandwidth=NULL, kind=c("general", "normal", "both"),
    fill_normal=NULL, fill_hist=getOption("se_fill"), 
    color_normal="gray20", line_width=NULL,
    x_pt=NULL, y_axis=FALSE,
    rug=FALSE, color_rug="black", size_rug=0.5,

    # -------------------------------------------------
    # Integrated violin/box/strip plot
    vbs_plot="vbs", vbs_ratio=0.9, bw=NULL, bw_iter=10,
    violin_fill=getOption("violin_fill"),
    box_fill=getOption("box_fill"),
    pt_size=NULL,
    vbs_pt_fill="black",
    vbs_mean=FALSE, fences=FALSE, n_min_pivot=1,
    k=1.5, box_adj=FALSE, a=-4, b=3,

    ID="row.name", ID_size=0.60,
    MD_cut=0, out_cut=0, out_shape="circle", out_size=1,

    # -----------------------------------------------------------------
    # Labels for axes, values, and legend if x and by variables, margins
    xlab=NULL, ylab=NULL, main=NULL, sub=NULL,
    lab_adjust=c(0,0), margin_adjust=c(0,0,0,0),

    rotate_x=getOption("rotate_x"), rotate_y=getOption("rotate_y"),
    offset=getOption("offset"),
    scale_x=NULL,
    axis_fmt=c("K", ",", ".", ""), axis_x_pre="", axis_y_pre="",

    # ----------------------------------------------------------------------
    # Draw one or more objects, text, or geometric figures, on the histogram
    add=NULL, x1=NULL, y1=NULL, x2=NULL, y2=NULL,

    # ---------------------------------------
    # Output: turn off, chart to PDF file, decimal digits, markdown file
    quiet=getOption("quiet"), do_plot=TRUE,
    use_plotly=getOption("lessR.use_plotly"),
    pdf_file=NULL, width=6.5, height=6, digits_d=NULL, Rmd=NULL,

    # --------------------------------------
    # Deprecated, removed in future versions
    n_cat=getOption("n_cat"), 
    rows=NULL, facet1=NULL, facet2=NULL,

    # ---------------------------------
    # Other
    eval_df=NULL, fun_call=NULL, \dots)
}

\arguments{
  \item{x}{Variable(s) to analyze. May be a single numeric variable, a vector
        in the workspace, multiple variables combined with \code{\link{c}},
        or an entire data frame. If omitted, defaults to all numeric variables
        in the data frame \code{d}.}
  \item{by}{A categorical grouping variable. When supplied, overlapping
        histograms or other distributional displays are drawn on the same
        coordinate system.}
  \item{facet}{A categorical conditioning variable, or a vector of two such
        variables, that activates Trellis (lattice) graphics to produce
        separate panels for each level or combination of levels.}
  \item{data}{Optional data frame that contains the variables of interest.
        Default is \code{d}.}
  \item{filter}{Logical expression or integer index vector identifying the rows
        to retain for analysis.}\cr

  \item{type}{Type of univariate display. Default is \code{"histogram"}.
        Additional options include:
        \code{"freq_poly"} (frequency polygon),
        \code{"density"} (kernel density curve),
        \code{"violin"},
        \code{"strip"} (one-dimensional scatter plot),
        and \code{"vbs"} (integrated violin-box-strip display).
        When using \code{"vbs"}, the \code{vbs_plot} parameter may specify a
        subset such as \code{"bs"} for a box-strip display.}
  \item{stat}{Specifies the vertical axis content.
        Default is \code{"count"}. Use \code{"proportion"} for relative
        frequencies, or \code{"density"} for density scaling in histogram
        mode.}\cr

  \item{n_row}{Optional number of rows for multi-panel displays created
        by \code{facet}.}
  \item{n_col}{Optional number of columns for multi-panel displays.
        If \code{n_col = 1}, facet strips appear at the left rather than top.}
  \item{aspect}{Aspect ratio of lattice panels (height/width). Default
        \code{"fill"} expands panels to available space. Use \code{1} for
        square panels or \code{"xy"} for banking to 45 degrees.}
  \item{theme}{Color theme. Use \code{\link{style}} to set persistent defaults.}
  \item{fill}{Fill color for bars or density regions. May be a single color,
        named palette (\code{"grays"}, \code{"hcl"}, \code{"blues"},
        \code{"reds"}, \code{"greens"}), or custom vector.}
  \item{color}{Border color for bars or density curves. May be a vector.
        Default follows \code{\link{style}}.}
  \item{transparency}{Transparency of filled areas. Default from
        \code{\link{style}}.}\cr

  \item{counts}{If \code{TRUE}, display counts for each bin on the histogram.}
  \item{bin_start}{Optional starting value of the first bin.}
  \item{bin_width}{Optional bin width, with or without a specified bin start.}
  \item{bin_end}{Optional value that must fall inside the final bin.}
  \item{breaks}{Method used to compute bins or an explicit set of breakpoints.
        Default is \code{"Sturges"}; options include \code{"Scott"} and
        \code{"FD"}.}\cr

  \item{cumulate}{Produces a cumulative histogram when set to \code{"on"}.
        Use \code{"both"} to superimpose the standard histogram.}
  \item{reg}{Color of the regular histogram when \code{cumulate="both"}.}
  \item{show_histogram}{When plotting a density curve, optionally draw a
        histogram behind it.}
  \item{bandwidth}{Bandwidth for kernel density estimation. Default begins with
        \code{bw.nrd0} but may be iteratively increased for smoother curves.}
  \item{kind}{Determines whether to display the general density curve, the
        normal density curve, or both.}
  \item{fill_normal}{Fill color used for the normal density curve.}
  \item{fill_hist}{Fill color for histograms under density curves.}
  \item{color_normal}{Border color for the normal density curve.}
  \item{line_width}{Line width of density curves.}
  \item{x_pt}{Value on the x-axis from which to illustrate a unit interval
        of density area. Applies when \code{kind = "general"}.}
  \item{y_axis}{If \code{TRUE}, display the density axis.}
  \item{rug}{If \code{TRUE}, draw a rug plot beneath the density curve.}
  \item{color_rug}{Color of rug ticks.}
  \item{size_rug}{Line width of rug ticks.}\cr

  \item{vbs_plot}{Controls which components of the violin-box-strip display
        appear. Characters \code{"v"}, \code{"b"}, and \code{"s"} indicate
        violin, box, and strip components. Upper/lower case permitted.
        Adjust colors with \code{\link{style}} parameters such as
        \code{violin_fill}, \code{box_fill}, \code{out_fill}, etc.}
  \item{vbs_ratio}{Relative height of the violin (and box) component.}
  \item{bw}{Bandwidth for violins; larger values yield smoother shapes.}
  \item{bw_iter}{Number of iterations for modifying the bandwidth to produce
        smoother violins.}
  \item{violin_fill}{Fill color for violins.}
  \item{box_fill}{Fill color for box plots.}
  \item{pt_size}{Size of points in strip plots.}
  \item{vbs_pt_fill}{Point color in the strip component of the VBS display.
        Default is black.}
  \item{vbs_mean}{If \code{TRUE}, show the mean in box plots.}
  \item{fences}{If \code{TRUE}, draw Tukey inner fences.}
  \item{n_min_pivot}{Minimum sample size required for a group to appear in a
        VBS pivot table (default 1).}
  \item{k}{IQR multiplier for determining whisker length.}
  \item{box_adj}{If \code{TRUE}, adjust box plot for skewness using medcouple.}
  \item{a, b}{Scaling factors for adjusted boxplot whiskers.}\cr

  \item{ID}{Variable supplying labels for selected points (outlier
        identification). Defaults to row names.}
  \item{ID_size}{Point label text size.}
  \item{MD_cut}{Mahalanobis distance cutoff for outliers in 2-variable plots.}
  \item{out_cut}{Number or proportion of extreme points to label. Interpreted
        differently for univariate vs. bivariate displays.}
  \item{out_shape}{Plotting symbol for outliers.}
  \item{out_size}{Point size for labeled outliers.}\cr

  \item{xlab}{X-axis label. Defaults to variable name or label.}
  \item{ylab}{Y-axis label. Defaults to "Frequency" or "Proportion".}
  \item{main}{Main title for the plot.}
  \item{sub}{Subtitle (not yet implemented).}
  \item{lab_adjust}{Two-element vector adjusting axis-label positions.}
  \item{margin_adjust}{Four-element vector adjusting plot margins.}\cr

  \item{rotate_x}{Rotation (in degrees) of x-axis tick labels.}
  \item{rotate_y}{Rotation (in degrees) of y-axis tick labels.}
  \item{offset}{Distance between tick labels and axis.}
  \item{scale_x}{Vector specifying start, end, and number of intervals for the
        x-axis.}
  \item{axis_fmt}{Formatting of axis numbers (\code{"K"}, \code{","},
        \code{"."}, or \code{""}).}
  \item{axis_x_pre}{Prefix for x-axis labels (e.g., \code{"$"}).}
  \item{axis_y_pre}{Prefix for y-axis labels.}\cr

  \item{add}{Add text or geometric objects (rectangles, lines, arrows,
        horizontal/vertical lines, etc.) to the plot.}
  \item{x1}{First x-coordinate for added objects.}
  \item{y1}{First y-coordinate.}
  \item{x2}{Second x-coordinate (for applicable object types).}
  \item{y2}{Second y-coordinate.}\cr

  \item{quiet}{If \code{TRUE}, suppress textual output.}
  \item{do_plot}{If \code{TRUE}, generate the plot.}
  \item{use_plotly}{If \code{TRUE} (default), draw a Plotly interactive version
        in addition to the static plot.}
  \item{pdf_file}{Optional file name to direct static graphics output to PDF.}
  \item{width}{Width of plot window (inches).}
  \item{height}{Height of plot window (inches).}
  \item{digits_d}{Digits of precision for printed statistics.}
  \item{Rmd}{Name of an R Markdown file to generate containing the analysis.}\cr

  \item{n_cat}{Maximum number of unique integer values to treat as categorical.
        \strong{Deprecated}.}
  \item{rows}{\strong{Deprecated}; use \code{filter}.}
  \item{facet1}{\strong{Deprecated}; use \code{facet}.}
  \item{facet2}{\strong{Deprecated}; use a two-element vector in \code{facet}.}\cr

  \item{eval_df}{If \code{TRUE}, checks that variables exist in the supplied
        data frame. Must be \code{FALSE} in Shiny or pipe workflows.}
  \item{fun_call}{Function call used internally for knitr compatibility.}\cr

  \item{\dots}{Additional graphics parameters passed to \code{\link{hist}},
        \code{\link{par}}, and related functions (e.g., \code{xlim},
        \code{ylim}, \code{cex.main}, \code{col.main}, \code{cex},
        \code{col.lab}).}
}

\details{
OVERVIEW

\code{X()} implements the univariate, distributional view for numeric variables.
Internally it still relies on the base \proglang{R} \code{\link{hist}} function
for histogram binning when \code{type = "histogram"}, but extends that core
infrastructure to a larger family of displays including frequency polygons,
kernel density estimates, violin plots, strip plots, and the integrated
violin-box-strip (VBS) display. For histogram-based displays, the
\code{freq} option of \code{\link{hist}} is always set to \code{FALSE}, so
counts, proportions, or densities are controlled through the lessR
arguments (\code{stat} and \code{type}) rather than directly
through \code{\link{hist}}.

For plotting densities, the recommended approach is to use
\code{type = "density"}, which computes and displays a kernel density estimate.
The older usage of \code{stat = "density"} is maintained for backward
compatibility but is not recommended for new code.\cr

VARIABLES AND TRELLIS PLOTS

At a minimum there is one primary numeric variable, \code{x}, which results in a
single univariate display (histogram, density, VBS, etc.). Facet graphics
(also called Trellis graphics), from Deepayan Sarkar's \pkg{lattice} package,
may be requested by supplying one or two categorical conditioning variables
to the \code{facet} argument. A single \code{facet} variable produces one panel
per level of that variable; a two-element vector in \code{facet} produces a
panel for each combination of levels of the two facet variables.

In each panel, the same numeric primary variable \code{x} is displayed,
conditioning on the facet levels. The combination of \code{x} with
\code{facet} thus yields a grid of small multiples that share scales and
styling, facilitating comparisons across subgroups without changing the
underlying analysis.\cr

BOXPLOTS AND THE VBS DISPLAY

For a single variable, the preferred summary display is often the integrated
violin-box-strip (VBS) plot, requested with \code{type = "vbs"}. Only the
violin or only the box portion can be obtained either via the corresponding
aliases or by setting \code{vbs_plot} to \code{"v"} or \code{"b"}. To view
a boxplot of a continuous variable across the levels of a categorical variable,
either as part of the full VBS plot or alone, there are two primary styles:\cr
1. \code{X(x, by = g, type = "box")}\cr
2. \code{X(x, facet = f, type = "box")}

Both styles convey the same information about numeric-by-category distributions.
The difference is visual: with \code{by}, multiple boxplots appear in a single
panel using the qualitative color palette from \code{\link{getColors}}, with
all hues chosen to be comparable in perceived saturation and brightness. With
\code{facet}, a separate panel is drawn for each group, typically using a
single hue. The same default qualitative colors are used throughout lessR,
including in \code{\link{Chart}}.\cr

DATA

The data may be supplied as a vector from the global environment (user
workspace), as one or more variables within a data frame, or as an entire
data frame. The default input data frame is \code{d}. A different source
data frame may be specified with the \code{data} argument. When multiple
variables are listed, only those of numeric type are analyzed by \code{X()}.

Variables in a data frame are referenced directly by their names; there is no
need to use \code{d$name}, \code{\link{with}}, or \code{\link{attach}}. If a
name exists both as a vector in the global environment and as a variable in
the specified data frame, the vector in the global environment takes
precedence.

To obtain a histogram (or other univariate display) for each numeric variable
in the default data frame \code{d}, call \code{X()} without specifying \code{x}.
For a different data frame, set the \code{data} argument accordingly. To
restrict analysis to a subset of variables, specify them with \code{:} or
\code{\link{c}}, such as \code{m01:m03} or \code{c(m01, m02, m03)}.

The \code{filter} parameter subsets rows (cases) of the input data frame
according to either (a) a logical expression or (b) a vector of integer row
indices. Logical expressions use standard \proglang{R} logical operators as
described in \code{\link{Logic}} (e.g., \code{\&} for "and", \code{|} for "or",
\code{!} for "not") and relational operators as described in
\code{\link{Comparison}} (e.g., \code{==} for equality, \code{!=} for
inequality, \code{>} for "greater than"). A vector of integers can be created
with standard \proglang{R} syntax; see the Examples section for illustrations.\cr

COLORS

Colors for individual elements of the plot can be controlled directly through
arguments such as \code{fill} (for areas) and \code{color} (for borders), or
indirectly through the global style system. The \code{\link{style}} function
allows selection of a color theme for the entire analysis. The default theme
is \code{"lightbronze"}, with additional themes such as \code{"gray"} for
grayscale and several color-emphasis themes (e.g., \code{"red"},
\code{"green"}).

For black backgrounds and partially transparent colors, use
\code{style(sub_theme = "black")}. For all color options, including
\code{fill}, a setting of \code{"off"} is equivalent to \code{"transparent"}.

For \code{fill}, you may specify a single color, a vector of colors, or a color
range. Besides standard color names, lessR provides multiple hue-balanced
palettes, including \code{"hues"}, as well as pre-specified ranges
\code{"blues"}, \code{"reds"}, and \code{"greens"}. Standard R palettes
\code{"rainbow"}, \code{"terrain"}, and \code{"heat"} are also available.
Custom ranges can be generated via \code{\link{getColors}}.\cr

HISTOGRAMS AND RELATED OUTPUT

When \code{type = "histogram"}, the output includes a frequency histogram with
lessR's default background and grid styling, along with optional relative
frequency and/or cumulative histograms. Summary statistics and a table of bin
information (bin boundaries, midpoints, counts, proportions, cumulative
counts, and cumulative proportions) are provided as part of the returned
object.

Bin construction can follow the default Sturges rule or be customized via
\code{bin_width}, \code{bin_start}, and \code{bin_end}, or via the
\code{breaks} argument (which can accept numeric vectors or named rules such
as \code{"Scott"} or \code{"FD"} as in \code{\link{hist}}). lessR adds more
informative error checking and guidance when user-specified bins do not fully
span the data or otherwise lead to problematic binning.

If multiple variables are supplied (including a complete data frame), \code{X()}
analyzes each numeric variable in turn. The related \code{\link{CountAll}}
function performs a similar role across mixed types, producing bar charts for
categorical variables and histograms for numeric variables. Faceting via
\code{facet} can be combined with \code{X()} to create trellised displays
across conditioning variables.\cr

VARIABLE LABELS

If variable labels exist (e.g., created by \code{\link{Read}}), they are used
by default for the horizontal axis label and in the text output. Variable
names are still available, but labels provide more descriptive and
publication-ready annotations.\cr

ONLY VARIABLES ARE REFERENCED

In lessR functions, the primary argument \code{x} (and any other variable 
arguments) must be specified as variable names, not expressions. The variables 
must exist either in the referenced data frame (such as the default \code{d}) 
or in the global environment. Direct expressions are not evaluated. For example:

\code{    > X(rnorm(50))   # does NOT work}

Instead, assign the expression to a named object and then reference that name:

\preformatted{
    > Y <- rnorm(50)  # create vector Y in user workspace
    > X(Y)            # directly reference Y
}

ERROR DETECTION

A common error for beginning users of \code{\link{hist}} is to specify a
sequence of break points that does not fully span the data, often constructed
with \code{\link{seq}}. Base \proglang{R} then produces a cryptic error. In
contrast, \code{X()} (and the \code{Histogram()} alias) checks for this
problem before calling \code{\link{hist}} and, if detected, issues a more
informative message along with guidance on how to correct the specification.

In addition, full control of the binning is available via \code{bin_width}
and \code{bin_start}. If \code{bin_start} is specified without \code{bin_width},
the bin width is determined by the default Sturges rule.\cr

PDF OUTPUT

To obtain PDF output directly, use the \code{pdf_file} argument, optionally
with \code{width} and \code{height} to control the device size in inches. The
resulting files are written to the current working directory, which can be set
explicitly via \code{\link{setwd}}. This mechanism facilitates high-quality,
publication-ready graphics saved directly from \code{X()}.\cr
}

\value{
The output may be assigned to an \code{R} object; otherwise it is printed directly
to the console. The returned object contains two types of components:

\itemize{
  \item Readable output: character strings such as tables and summaries,
        formatted for display.
  \item Statistics: numerical values suitable for further computation or
        inclusion in dynamic documents.
}

These components are designed to support R Markdown workflows. Any element of the
saved object can be inserted into an R Markdown document by referencing its name,
preceded by the object name and the \code{$} operator (see \code{examples}).  
Only elements relevant to the requested analysis are returned.  
For example, bin proportions appear only when a histogram is requested.

To save the results, assign the output to an object, such as \code{h <- Histogram(Salary)}.
Use \code{names(h)} to view the available components, and display any one by typing, for
example, \code{h$out_freq}. Output can be viewed interactively at the console or embedded in
R Markdown documents along with narrative interpretation.

READABLE OUTPUT\cr
\code{out_suggest}: Suggestions for other similar analyses\cr
\code{out_summary}: Summary statistics\cr
\code{out_freq}: Frequency distribution\cr
\code{out_outliers}: Outlier analysis\cr

STATISTICS\cr
\code{bin_width}: Bin width\cr
\code{n_bins}: Number of bins\cr
\code{breaks}: Breaks of the bins\cr
\code{mids}: Bin midpoints\cr
\code{counts}: Bin counts\cr
\code{prop}: Bin proportions\cr 
\code{cumulate}: Bin cumulative counts\cr 
\code{cprop}: Bin cumulative proportions\cr
}

\references{
Gerbing, D. W. (2023). \emph{R Data Analysis without Programming: Explanation and Interpretation}, 2nd edition, Chapter 5, NY: Routledge.

Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 4, NY: CRC Press.

Gerbing, D. W. (2021). Enhancement of the Command-Line Environment for use in the Introductory Statistics Course and Beyond, \emph{Journal of Statistics and Data Science Education}, 29(3), 251-266.

Hubert, M. and Vandervieren, E. (2008). An adjusted boxplot for skewed distributions, Computational Statistics and Data Analysis 52, 51865201.

Sarkar, D. (2008). \emph{Lattice: Multivariate Data Visualization with R}. Springer.

Sievert, C. (2020). \emph{Interactive Web-Based Data Visualization with R, plotly, and shiny}. Chapman and Hall/CRC. URL: \url{https://plotly.com/r/}
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{\code{\link{XY}}, \code{\link{Chart}}, \code{\link{getColors}}, \code{\link{style}}.
}

\examples{
# get the data
d <- rd("Employee")

# make sure default style is active
style()

# --------------------
# different histograms
# --------------------

# histogram with all defaults
X(Salary, type="histogram")
# with deprecated alias
Histogram(Salary)

# output saved for later analysis into object h
h <- hs(Salary)
# view full text output
h
# view just the outlier analysis
h$out_outliers
# list the names of all the components
names(h)

# histogram with no borders for the bars
X(Salary, color="off")

# just males employed more than 5 years
X(Salary, filter=(Gender=="M" & Years > 5))

# histogram with red bars, black background, and black border
style(panel_fill="black", fill="red", panel_color="black")
X(Salary)
# or use a lessR pre-defined sequential color palette
#   with some transparency
X(Salary, fill="rusts", color="brown", transparency=.1)

# histogram with purple color theme, translucent gold bars
style("purple", sub_theme="black")
X(Salary)
# back to default color theme
style()

# histogram with specified bin width
# can also use bin_start
X(Salary, bin_width=12000)

# histogram with rotated axis values, offset more from axis
# suppress text output
style(rotate_x=45, offset=1)
X(Salary, quiet=TRUE)
style()

# histogram with specified bin width
X(Salary, bin_width=20000, xlab="My Variable")

# histogram with bins calculated with the Scott method and values displayed
X(Salary, breaks="Scott", counts=TRUE, quiet=TRUE)

# histogram with the number of suggested bins, with proportions
X(Salary, breaks=15, stat="proportion")

# histogram with non-default values for x- and y-axes
d[2,4] <- 45000
X(Salary, scale_x=c(20000,160000,8))

# ----------------
# Trellis graphics
# ----------------
Histogram(Salary, facet=Dept)

# ---------------------
# cumulative histograms
# ---------------------

# cumulative histogram with superimposed regular histogram, all defaults
X(Salary, cumulate="both")

# cumulative histogram plus regular histogram
X(Salary, cumulate="both", reg="mistyrose")

# -------------
# density plots
# -------------

# default density plot
X(Salary, type="density")

# normal curve and general density curves superimposed over histogram
# all defaults
X(Salary, type="density", kind="both")

# display only the general estimated density
#  so do not display the estimated normal curve
# specify the bandwidth for the general density curve
X(Salary, type="density", bandwidth=8000)

# display only the general estimated density and a corresponding
#  interval of unit width around x_pt
X(Salary, type="density", x_pt=40000)

# densities for all specified numeric variables in a list of variables
# e.g., use the combine or c function to specify a list of variables
X(c(Years, Salary), type="density")

# -------------------------------------------------
# histograms for data frames and multiple variables
# -------------------------------------------------

# create data frame, d, to mimic reading data with Read function
# d contains both numeric and non-numeric data
d <- data.frame(rnorm(50), rnorm(50), rnorm(50), rep(c("A","B"),25))
names(d) <- c("X","Y","Z","C")

# although data not attached, access the variable directly by its name
X(X)

# histograms for all numeric variables in data frame called d
# d is the default name, so does not need to be specified with data
X()

# histogram with specified options, including red axis labels
style(fill="palegreen1", panel_fill="ivory", axis_color="red") 
X(counts=TRUE)
style()  # reset

# histograms for all specified numeric variables
# use the combine or c function to specify a list of variables
X(c(X,Y))

# integrated violin/box/scatter plot (VBS)
d <- Read("Employee")
X(Salary, type="vbs")

X(Years, by=Gender, pt_size=1.25,
     fill=c("olivedrab3", "gold1"),
     color=c("darkgreen", "gold4"), type="vbs")

\donttest{

# by variable, different colors for different values of the variable
# two panels
X(Salary, facet=Dept)

# large sample size
x <- rnorm(10000)
X(x)

# custom colors for outliers, which might not appear in this subset data
style(out_fill="hotpink", out2_fill="purple")
X(Salary, type="vbs")
style()

# no violin plot or scatterplot, just a boxplot
X(Salary, type="box")

# -----------
# annotations
# -----------

d <- rd("Employee")

# Place a message in the top-right of the graph
# Use \n to indicate a new line
X(Salary, add="Salaries in our Company", x1=100000, y1=7)

# Use style to change some parameter values
style(add_trans=.8, add_fill="gold", add_color="gold4",
      add_lwd=0.5, add_cex=1.1)
# Add a rectangle around the message centered at <100000,7>
X(Salary, add=c("rect", "Salaries in our Company"),
      x1=c(82000, 100000), y1=c(7.7, 7), x2=118000, y2=6.2)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ histogram }
\keyword{ color }

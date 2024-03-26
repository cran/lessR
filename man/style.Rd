\name{style}
\alias{style}
\alias{set}

\title{Set the Default Color Theme and Other System Settings}

\description{
Deprecated Names: \code{set}, \code{theme}

The color and style attributes of each plot can be set as a general theme, or individually set from the following list of attributes. For convenience, groups of these attributes are specified to define color themes, plus style sub-themes that apply to any theme, with default values:  \code{theme="colors"} and \code{sub_theme="default"}. To reset to the default theme: \code{style()}. 
}

\usage{
style(
  theme=c("colors", "lightbronze", "dodgerblue", "darkred", "gray",
      "gold", "darkgreen", "blue", "red", "rose", "slatered", "green",
      "purple", "sienna", "brown", "orange", "white", "light"),
  sub_theme=c("default", "black", "wsj"),
  set=NULL, get=FALSE, reset=TRUE,

  window_fill=getOption("window_fill"),
  panel_fill=getOption("panel_fill"),
  panel_color=getOption("panel_color"),
  panel_lwd=getOption("panel_lwd"),
  panel_lty=getOption("panel_lty"),

  fill=NULL,
  bar_fill=getOption("bar_fill"),
  bar_fill_discrete=getOption("bar_fill_discrete"),
  bar_fill_cont=getOption("bar_fill_cont"),
  trans=NULL,
  trans_bar_fill=getOption("trans_bar_fill"),
  color=NULL,
  bar_color=getOption("bar_color"),
  bar_color_cont=getOption("bar_color_cont"),
  bar_color_discrete=getOption("bar_color_discrete"),
  
  values=getOption("values"),
  values_color=getOption("values_color"), 
  values_size=getOption("values_size"),
  values_digits=getOption("values_digits"),
  values_position=getOption("values_position"),
		 
  pt_fill=getOption("pt_fill"),
  trans_pt_fill=getOption("trans_pt_fill"),
  pt_color=getOption("pt_color"),
  se_fill=getOption("se_fill"),
  ellipse_fill=getOption("ellipse_fill"),
  ellipse_color=getOption("ellipse_color"),
  ellipse_lwd=getOption("ellipse_lwd"),
  fit_color=getOption("fit_color"),
  fit_lwd=getOption("fit_lwd"),
  bubble_text_color=getOption("bubble_text_color"),
  segment_color=getOption("segment_color"),
  ID_color=getOption("ID_color"),
  out_fill=getOption("out_fill"),
  out_color=getOption("out_color"),
  out2_fill=getOption("out2_fill"),
  out2_color=getOption("out2_color"),

  violin_fill=getOption("violin_fill"),
  violin_color=getOption("violin_color"),
  box_fill=getOption("box_fill"),
  box_color=getOption("box_color"),

  axis_color=getOption("axis_color"),
  axis_x_color=getOption("axis_x_color"),
  axis_y_color=getOption("axis_y_color"),
  axis_lwd=getOption("axis_lwd"),
  axis_x_lwd=getOption("axis_x_lwd"),
  axis_y_lwd=getOption("axis_y_lwd"),
  axis_lty=getOption("axis_lty"),
  axis_x_lty=getOption("axis_x_lty"),
  axis_y_lty=getOption("axis_y_lty"),
  axis_cex=getOption("axis_cex"),
  axis_x_cex=getOption("axis_x_cex"),
  axis_y_cex=getOption("axis_y_cex"),
  axis_text_color=getOption("axis_text_color"),
  axis_x_text_color=getOption("axis_x_text_color"),
  axis_y_text_color=getOption("axis_y_text_color"),
  rotate_x=getOption("rotate_x"),
  rotate_y=getOption("rotate_y"),
  offset=getOption("offset"),

  lab_color=getOption("lab_color"),
  lab_x_color=getOption("lab_x_color"),
  lab_y_color=getOption("lab_y_color"),
  lab_cex=getOption("lab_cex"),
  lab_x_cex=getOption("lab_x_cex"),
  lab_y_cex=getOption("lab_y_cex"),
  main_color=getOption("main_color"),
  main_cex=getOption("main_cex"),

  grid_color=getOption("grid_color"),
  grid_x_color=getOption("grid_x_color"),
  grid_y_color=getOption("grid_y_color"),
  grid_lwd=getOption("grid_lwd"),
  grid_x_lwd=getOption("grid_x_lwd"),
  grid_y_lwd=getOption("grid_y_lwd"),
  grid_lty=getOption("grid_lty"),
  grid_x_lty=getOption("grid_x_lty"),
  grid_y_lty=getOption("grid_y_lty"),

  strip_fill=getOption("strip_fill"),
  strip_color=getOption("strip_color"),
  strip_text_color=getOption("strip_text_color"),

  add_fill=getOption("add_fill"),
  add_trans=getOption("add_trans"),
  add_color=getOption("add_color"),
  add_cex=getOption("add_cex"),
  add_lwd=getOption("add_lwd"),
  add_lty=getOption("add_lty"),

  n_cat=getOption("n_cat"), suggest=getOption("suggest"),
  notes=getOption("notes"),
  quiet=getOption("quiet"), brief=getOption("brief"),

  results=getOption("results"), explain=getOption("explain"),
  interpret=getOption("interpret"), document=getOption("document"), 
  code=getOption("code"),

  width=120, show=FALSE, \dots)

set(\dots)
}


\arguments{
  \item{theme}{The specified color scheme. If specified, re-sets all style
       attributes to the values consistent with that theme.}
  \item{sub_theme}{Further modification of the main themes.}
  \item{set}{A list of parameter values, a theme, that was previously
       saved, and now is read back to become the current set of parameter
       values_ See the examples.}
  \item{get}{Save the current list of parameter values, a theme, into
       an R object.}
  \item{reset}{Change one or more settings or the entire theme.}\cr

  \item{window_fill}{Fill color of the entire device window.}
  \item{panel_fill}{Color of the plot background.}
  \item{panel_color}{Color of border around the plot background, the box, that
        encloses the plot, with a default of \code{"black"}.}
  \item{panel_lwd}{Line width of the box around the plot.}
  \item{panel_lty}{Line type of the box around the plot. Acceptable values are 
        \code{"blank"}, \code{"solid"}, \code{"dashed"}, \code{"dotted"},
        \code{"dotdash"}, and \code{"longdash"}.}\cr

  \item{fill}{Color of a filled region -- bars, points and bubbles -- 
        depending on the
        objected plotted. Can explicitly choose \code{"grays"}
        or \code{"hues"} , or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}}
  \item{bar_fill}{Color of a filled bar, bubble or box.}
  \item{bar_fill_discrete}{Color of a filled bar chart bar or pie chart slice.}
  \item{bar_fill_cont}{Color of a filled histogram bar.}
  \item{trans}{Transparency of a filled bar, rectangular region, or points from 0
       (none) to 1 (complete).}
  \item{trans_bar_fill}{The transparency of a filled bar or rectangular region,
       such as a histogram bar or the box in a box plot. Value from 0 to 1,
       opaque to transparent.}
  \item{color}{Color of a line segment such as the border of bar or point.
        Can explicitly choose \code{"grays"}
        or \code{"hues"}, or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}}
  \item{bar_color}{Color of the border of a filled region such as a histogram
       bar.}
  \item{bar_color_discrete}{Color of the border of a filled region for
       values on a qualitative scale.}
  \item{bar_color_cont}{Color of the border of a filled region for
       values on a quantitative scale, such as a histogram bar.}\cr

  \item{values}{If not the default value of \code{"off"}, adds the
        numerical results to the plot according to \code{"\%"}, 
        \code{"prop"} or \code{"input"}, that is, percentages,
         proportions, or the value from which the slices are plotted,
         such as tabulated counts if \code{y} is not specified,
         or the value of \code{y} if the plotted values are provided. If
         any other \code{values} parameter is specified, default is set
         to \code{"\%"}.}
  \item{values_color}{Color of the plotted text. Could be a vector to specify
        a unique color for each value. If fewer colors are specified than
        the number of categories, the colors are recycled.}
  \item{values_size}{Character expansion factor, the size, of the plotted text, 
        for which the default value is 0.95.}
  \item{values_digits}{Number of decimal digits for which to display the values_
        Default is 0, round to the nearest integer, for \code{"\%"}
        and 2 for \code{"prop"}.}
  \item{values_position}{Position of the plotted text. Default is inside the pie,
        or, if \code{"label"}, as part of the label for each value
        outside of the pie.}\cr

  \item{pt_fill}{Color of a filled plotted point.}
  \item{trans_pt_fill}{The transparency of the inner region of a plotted point.
       Value from 0 to 1, opaque to transparent.}
  \item{pt_color}{Color of a line or outline of a filled region, such as the
       border of a plotted point.}
  \item{se_fill}{Color of the fill for the standard error plot about a
       fit line in a scatter plot.}
  \item{ellipse_fill}{Color of the fill for an ellipse in a scatter plot.}
  \item{ellipse_color}{Color of the border for an ellipse in a scatter plot.}
  \item{ellipse_lwd}{Line width of the border for an ellipse in a scatter plot.}
  \item{fit_color}{Color of the fit line in a scatter plot.}
  \item{fit_lwd}{Width of fit line. By default is 2 for Windows and 1.5 for Mac.}
  \item{bubble_text_color}{Color of the displayed text regarding the size of a bubble,
        either a tabulated frequency for categorical variables, or the value of a
        third variable according to \code{size}.}
  \item{segment_color}{Color of connecting line segments when there are also plotted
        points, such as in a frequency polygon. Default color is \code{color}.}
  \item{ID_color}{Color of the text to display the ID labels.}
  \item{out_fill}{For a scatterplot, color of the border of potential outliers, which,
       for the unadjusted boxplot, are default values
       1.5 IQR's beyond the lower or upper quartile.}
  \item{out_color}{For a scatterplot, color of potential outliers.}
  \item{out2_fill}{For a scatterplot, color of extreme outliers, which,
       for the unadjusted boxplot, are default values
       3 IQR's beyond the lower or upper quartile.}
  \item{out2_color}{For a scatterplot, color of the border of extreme outliers.}\cr

  \item{violin_fill}{Fill color for a \bold{violin plot.}}
  \item{violin_color}{Border color for the violin in a violin plot.}
  \item{box_fill}{Fill color for a box plot.}
  \item{box_color}{Border color of a box in a box plot.}\cr

  \item{axis_color}{Color of the axes.}
  \item{axis_x_color}{Color of the \code{x}-axis_}
  \item{axis_y_color}{Color of the \code{y}-axis_}
  \item{axis_lwd}{Line width of axes.}
  \item{axis_x_lwd}{Line width of horizontal axis_}
  \item{axis_y_lwd}{Line width of vertical axis_}
  \item{axis_lty}{Line type of axes, either "solid", "dashed",
       "dotted", "dotdash", "longdash", "twodash", or "blank".}
  \item{axis_x_lty}{Line type of horizontal axis_}
  \item{axis_y_lty}{Line type of vertical axis_}
  \item{axis_cex}{Scale magnification factor, which by defaults displays the axis
        values to be smaller than the axis labels. Provides the functionality of,
        and can be replaced by, the standard R \code{cex.axis}.}
  \item{axis_x_cex}{Scale magnification factor for the \code{x}-axis_}
  \item{axis_y_cex}{Scale magnification factor for the \code{y}-axis_}
  \item{axis_text_color}{Color of the font used to label the axis values_}
  \item{axis_x_text_color}{Color of the font used to label the \code{x}-axis values_}
  \item{axis_y_text_color}{Color of the font used to label the \code{y}-axis values_}
  \item{rotate_x}{Degrees that the \code{x}-axis values are rotated, usually to
        accommodate longer values, typically used in conjunction with \code{offset}.}
  \item{rotate_y}{Degrees that the \code{y}-axis values are rotated.}
  \item{offset}{The spacing between the axis values and the axis_ Default
        is 0.5. Larger values such as 1.0 are used to create space for the label when
        longer axis value names are rotated.}\cr

  \item{lab_color}{Color of the axis labels.}
  \item{lab_x_color}{Color of the axis labels on the horizontal axis_}
  \item{lab_y_color}{Color of the axis labels on the vertical axis_}
  \item{lab_cex}{Size of labels for x and y axes.}
  \item{lab_x_cex}{Size of labels for x.}
  \item{lab_y_cex}{Size of labels for y.}
  \item{main_color}{Color of the title.}
  \item{main_cex}{Size of the title font.}\cr

  \item{grid_color}{Color of the grid lines.}
  \item{grid_x_color}{Color of the grid lines for the \code{x}-axis_}
  \item{grid_y_color}{Color of the grid lines for the \code{y}-axis_}
  \item{grid_lwd}{Width of grid lines.}
  \item{grid_x_lwd}{Width of vertical grid lines, inherits from \code{grid_lwd}.}
  \item{grid_y_lwd}{Width of horizontal grid lines, inherits from \code{grid_lwd}.}
  \item{grid_lty}{Line type for grid lines: "solid", "dashed",
        "dotted", "dotdash", "longdash", or "twodash", or "blank".}
  \item{grid_x_lty}{Line-type of vertical grid lines, inherits from \code{grid_lty}.}
  \item{grid_y_lty}{Line-type of horizontal grid lines, inherits from \code{grid_lty}.}\cr

  \item{strip_fill}{Fill color for the \bold{strip that labels each panel} in
   a Trellis plot.}
  \item{strip_color}{Border color for the strip that labels each panel in
   a Trellis plot.}
  \item{strip_text_color}{Color of the label in each strip of a Trellis plot.}\cr

  \item{add_fill}{Interior fill color of added object.
        Can explicitly choose \code{"grays"}
        or \code{"hues"}, or pre-specified R color schemes 
        \code{"rainbow"}, \code{"terrain"}, and \code{"heat"}. Can also
        provide pre-defined color ranges \code{"blues"}, \code{"reds"}
        and \code{"greens"}, as well as
        custom colors, such as generated by \code{\link{getColors}}}
  \item{add_trans}{Transparency level of color or fill, which ever is
       applicable from 0 (opaque) to 1 (transparent).}
  \item{add_color}{Color of borders and lines of added object.}
  \item{add_cex}{Text expansion factor, relative to 1. As with the following
   properties, can be a vector for multiple placement or objects.}
  \item{add_lwd}{Line width of added object.}
  \item{add_lty}{Line type of added object. See \code{panel_lty} for types.}\cr

  \item{n_cat}{Number of categories that specifies the largest number of unique
        equally-spaced values of variable of a numeric data type for which the
        variable will be analyzed as categorical. Default value is 0.
        \emph{[deprecated]}: Best to convert a categorical integer variable
        to a factor.}
  \item{suggest}{If \code{TRUE}, then provide suggestions for alternative analyses.}
  \item{notes}{If \code{TRUE}, then provide notes.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
        with \code{\link{style}} function.}
  \item{brief}{If set to \code{TRUE}, reduced text output.}\cr

  \item{results}{For the R markdown file generated by the Rmd option, 
        show the results.}
  \item{explain}{For the R markdown file generated by the Rmd option, 
        explain the results.}
  \item{interpret}{For the R markdown file generated by the Rmd option, 
        interpret the results.}
  \item{document}{For the R markdown file generated by the Rmd option,
        documents the code that generated the results.} 
  \item{code}{For the R markdown file generated by the Rmd option, 
        shows the code that generated the results.}\cr

  \item{width}{Maximum width of each line displayed at the console, just accesses
        the standard R options function for width.}
  \item{show}{Option for showing all settings.}
  \item{\dots}{Parameter values_}
}


\details{
OVERVIEW\cr
Sets the default color palette via the R \code{\link{options}} statement, as well as the transparency of plotted bars and points and other non-color characteristics such as the color of the grid lines. For convenience, groups of attributes are organized into themes and sub-themes. When the \code{theme} is specified, \emph{all} options are reset to their default values_ All other modifications, with individual parameters or grouped parameters as a sub-theme, are cumulative. For example, one sub-theme can be followed by another, as well as the specifications of individual attributes. Calling the function with no arguments sets to the default style.

Available themes:\cr
"lightbronze" [default]\cr
"dodgerblue" [default \code{lessR} 3.6.0 and earlier]\cr
"darkred"\cr
"gray"\cr
"gold"\cr
"darkgreen"\cr
"blue"\cr
"red"\cr
"rose"\cr
"green"\cr
"purple"\cr
"sienna"\cr
"brown"\cr
"orange"\cr
"white"\cr
"light"\cr

The \code{"gray"} color theme is based on the colors used in Hadley Wickham's \code{ggplot2} package. The \code{"lightbronze"} theme, especially with the \code{wsj} sub-theme, is based on Jeffrey Arnold's \code{wsj} theme from his ggthemes package.

SUB-THEMES\cr
\code{"black"}: Black background of the entire device window with translucent fill colors from the current theme. 
\code{"wsj"}: Similar to the \code{wsj} theme from the \code{ggthemes} package, especially with the theme of \code{"lightbronze"}. The \code{y}-axis is removed with though the value labels retained, the vertical grid is removed, and the horizontal grid is dotted and thicker than the default.
}


\value{
The current settings can optionally be saved into an \code{R} object, and then read back at a later time with the \code{set} function.\cr
}


\references{
Arnold, Jeffrey B., (2017), ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 3.4.0. https://CRAN.R-project.org/package=ggthemes

Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 10, NY: CRC Press.

Wickham, Hadley, (2009), ggplot2: Elegant Graphics for Data Analysis, 2nd edition, Springer.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{options}}.
}

\examples{

# some data
d <- rd("Employee", quiet=TRUE)

# gold colors embedded in a black background
style("gold", sub_theme="black")
Plot(Years, Salary, size=0, ellipse=seq(.1,.9,.1))

# three ways to do gray scale
style(window_fill="white")
# 1. gray scale with a light gray background
style("gray")
# 2. gray scale with a dark, almost black, background
style("gray", sub_theme="black")
# 3. mostly black and white
style("white")

# reset style to the default "colors"
style()

# set bar fill to qualitative hcl colors
# here also turn off bar borders and set to a mild transparency
Histogram(Salary, fill="greens", color="off")
# same as
# style(bar_fill_cont="greens", bar_color="off")
# Histogram(Salary)

# set bar fill to 6 blue colors
# for continuous band explicitly call getColors and specify n
#   to obtain the full spectrum, such as for analysis of Likert
#   scale responses with six possible responses per item
style(bar_fill=getColors("blues", n=6))

# adjust Trellis strip to a dark background
style(strip_fill="gray60", strip_color="gray20",
      strip_text_color=rgb(247,242,230, maxColorValue=255))
Plot(Years, Salary, by1=Gender)

# define a custom style beyond just colors
style(panel_fill="off", panel_color="off",
      window_fill=rgb(247,242,230, maxColorValue=255),
      pt_fill="black", trans=0,
      lab_color="black", axis_text_color="black",
      axis_y_color="off",
      grid_x_color="off", grid_y_color="black",  grid_lty="dotted", grid_lwd=1)
hs(Salary)

# save the current theme settings into an R object without changes
# unless set to FALSE, get is always TRUE, for all calls to style
mystyle <- style(get=TRUE)
# ...  bunch of changes
# then recall older settings to current theme setting
style(set=mystyle)

# create a gray-scale with a sub-theme of wsj
# save, and then at a later session read back in
grayWSJ <- style("gray", sub_theme="wsj")
# Write(grayWSJ, "grayWSJ", format="R")
# ...
#mystyle <- Read("grayWSJ.rda")  # read grayWSJ.rda
#style(set=mystyle)



# all numeric variables with 8 or less unique values and equally spaced
#  intervals are analyzed as categorical variables
style(n_cat=8)}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sets }
\keyword{ csv }







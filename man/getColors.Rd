\name{getColors}
\alias{getColors}

\title{Hue, Chroma, Luminance (HCL) Color Wheel or Specified Colors}

\description{

Generates colors and the corresponding color rectangle of bars of different color or a color wheel with slices of different colors for a sequence of qualitative HCL colors by default. To avoid bias in the comparison of differently colored regions of a visualization, the HCL colors by default are generated with fixed values of chroma (saturation) and luminance (brightness) for a range of hues, by default ordered so that adjacent colors are as separated as possible. Also generates a sequence of HCL colors according to any chosen hue value in which chroma and luminance can be varied by implicit calls to Zeileis's et al. \code{sequential_hcl} function from Ihaka's et al. \code{colorspace} package, with pre-defined values such as \code{"blues"}. Also processes any arbitrarily specified set of colors that are specified, or generated from a custom range according to a beginning and ending specified color.

In terms of workflow, use the function by itself to select a set of colors from the resulting color rectangle/wheel. The function outputs the colors so that the function call can serve as an argument to parameters in other functions that require a sequence of one or more colors as input, in which case the visualization of the color wheel or rectangle is not generated. So after the colors are selected, pass to an argument for a visualization function such as for the \code{fill} parameter.
}

\usage{
  getColors(clr=NULL, end.clr=NULL, shape=c("rectangle", "wheel"),
         n=5, h=0, h2=NULL, c=70, l=55, fixup=TRUE,
         in.order=FALSE, radius=0.9, main=NULL,
         labels=NULL, labels.cex=0.8, border="lightgray", lty="solid",
         output=NULL, quiet=getOption("quiet"), \ldots)
}

\arguments{
  \item{clr}{Optional specified colors to plot. If list of
  specified colors, then the following parameters are not relevant. Can also
  be pre-defined color codes that trigger a range of colors from light to
  dark, such as \code{"blues"}.}
  \item{end.clr}{If specified, then generate a color continuum that begins at
   \code{clr} and ends at \code{end.clr}.}
   \item{shape}{xx.}\cr

  \item{n}{Number of colors to display.}\cr

  \item{h}{Beginning HCL hue, 0 to 360.}
  \item{h2}{Ending HCL hue, 0 to 360. Defaults to a value close to 360.
        Specify only if a limited range of colors is desired.
        Requires \code{in.order} to be \code{FALSE}.}
  \item{c}{Value of chroma, with default of 65.}
  \item{l}{Value of luminance, with standard default of 65.
        If a progressive color sequence based on a single
        hue is generated, default values are to start at \code{l}=90 and end at
        \code{l}=45.}
  \item{fixup}{R parameter name. If \code{TRUE}, then HCL values outside of
        the displayable RGB color space are transformed to fit into that space
        so as to display.}\cr

  \item{in.order}{If \code{TRUE}, orders the colors in order of their
        HCL hue values. Otherwise maximizes the difference
    		between adjacent colors hues to prepare for inclusion in
    		visualizations with qualitative, discrete color scales.}
  \item{radius}{Size of wheel. Not applicable to the rectangular shape.}
  \item{main}{Title.}\cr

  \item{labels}{If \code{TRUE}, then displayed. For HCL qualitative scale,
       default is \code{TRUE}, otherwise \code{FALSE}}.
  \item{labels.cex}{Character expansion factor of labels relative to 1.}
  \item{border}{Color of the borders of each slice. Set to \code{"off"} or
       \code{"transparent"} to turn off.}
  \item{lty}{Line type of the border.}\cr

  \item{output}{Default to evaluate if function call at top level, so produce
       text and graphics output, or embedded in another function call,
       so do not produce that output. If set to \code{"on"}, then 
       do output. If set to \code{"off"} then do not do output.}
  \item{quiet}{If set to \code{TRUE}, no list of colors. Can change system default
       with \code{\link{style}} function.}
  \item{\ldots}{Other parameter values.}
}


\details{

I. HCL COLORS\cr
Generate a range of colors according to the parameter \code{clr} in the form of a character string vector of their names, and also as a color wheel if not called from another function. The default value (for all but grayscale or white color themes) of \code{clr} is \code{"colors"}, which generates a palette of the specified number, \code{n}, of discrete HCL colors at the same chroma and luminance, respective default values of 65 and 100. With constant chroma and luminance the HCL color space can provide a palette of colors that have the same gray-scale intensities if desaturated. That means no brightness bias for viewing different colors that represent different areas, such as in a bar chart of two variables, or a pie chart. The primary qualification is that the HCL color space is not in a one-to-one correspondence with the RGB color space of computer monitors, so some HCL colors are approximated (with the default setting of the \code{fixup} parameter set to \code{TRUE}).

For \code{"colors"}, the default, the hue values and associated colors are expressed as HEX and RGB values. The first 12 generated discrete colors, with their associated HCL hues at the default values of chroma (65) and luminance (100), are  green (120), brown (60), blue (240), red (0), purple (270), turquoise (180),  olive (90), rust (30), mulberry (330), aqua (210), medium orchard (300), and marine green (150).

To have the generated colors be in the sequential order of hues, set \code{in.order} to \code{FALSE}. For about up to five colors adjacent values are still reasonably well distinguished even if in sequential order of hue number in the hcl space. The colors are generated to maintain maximum hue separation regardless of the number of colors, so will differ somewhat from the list of colors when re-ordered as described in the previous paragraph.\cr

II. COLOR SEQUENCE\cr
A second possibility is to generate a sequence of colors according to the value of \code{n} from a given start color to an ending color. To specify a custom range, set \code{clr} as the value of the first color, and then \code{end.clr} as the value of the last color in the color range. The colors in the sequence may or may not be of the same hue.

Or, access implicit calls Zeileis (2009) \code{\link{sequential_hcl}} function from the \code{colorspace} package to access pre-defined color ranges including \code{"grays"}, which is the default if the color theme is \code{"gray"} or  \code{"white"}. Other predefined sequences are shown in the following table. Also can invoke the standard R color ranges of \code{"heat"}, \code{"terrain"}, and \code{"rainbow"}. Can specify any value of hue with \code{h}. Can also provide custom values of chroma (c) and luminance (l), with either one a range of values defined as a vector of two values. Default values are \code{c=70} and \code{l=c(90,45)}. That is, the color sequence is generated according to the given hue, \code{h}, with a chroma of 70 and luminance varying from 90 to the darker 45.

The values set by the predefined sequences follow.\cr


\tabular{lll}{
 colors \tab param \tab value\cr
----------- \tab ----- \tab -----\cr
\code{"reds"} \tab h \tab 0\cr
\code{"rusts"} \tab h \tab 30\cr
\code{"yellows"} \tab h \tab 60\cr
\code{"olives"} \tab h \tab 100\cr
\code{"greens"} \tab h \tab 120\cr
\code{"turquoises"} \tab h \tab 180\cr
\code{"aquas"} \tab h \tab 200\cr
\code{"blues"} \tab h \tab 240\cr
\code{"purples"} \tab h \tab 280\cr
\code{"grays"} \tab c \tab 0\cr
----------- \tab ----- \tab -----\cr
}

The predefined color name can be provided as the first argument of the function call, that is, the value of \code{clr}, or the corresponding value of \code{h} (or \code{c} for gray scale) can be specified. The specifications are equivalent.\cr


III. SPECIFIED COLORS\cr
The third possibility is to generate a color wheel from a specified set of color values. Set the value of \code{clr} according to the vector of these values. Specify the values with R color names (see the lessR function \code{\link{showColors}}), RGB values according to the \code{rgb} function or from related R color space functions such as \code{hcl}, or as hexadecimal codes.\cr

FUNCTION USAGE\cr
Use the function on its own, in which case the color rectangle/wheel visualization is generated as are the color values. The vector of color values may be saved in its own R object as the output of the function call. Or, use the function to set colors for other parameter values in other function calls. See the examples.
}

\value{
Colors are invisibly returned as a character string vector.
}

\seealso{
\code{\link{hcl}}, \code{\link{showColors}}
}


\examples{
# default color wheel of 5 HCL colors with adjacent colors max separated
getColors()

# deep rich colors for HCL qualitative scale
getColors(c=90, l=50)

# generate hcl blue sequence with c=65 and vary l
getColors("blues", labels=FALSE)

# generate yellow hcl sequence with varying chroma
getColors("yellows", c=c(20,90), l=65)  # default of h=0

# generate custom hue color sequence with varying brightness
getColors(h=50, c=65, l=c(40,80), n=10)

# a standard R color sequence
getColors("heat")

# manual specification of colors
# individual colors
getColors(c("black", "blue", "red"))
# custom range of colors
getColors(clr="aliceblue", end.clr="blue")

# just generate character vector of colors
clr <- getColors(output="off")  
clr


# -----
# plots
mydata <- rd("Employee", format="lessR")

# default quantitative scale
bc(Dept, fill=getColors())
# or with implicit call to getColors
bc(Dept, fill="colors")
# or an implicit call with the blues
bc(Dept, fill="blues")

# even though we have a bar graph, also want the
#  graph of the colors as well as the text listing of the colors
bc(Dept, fill=getColors("blues", output="on"))

# custom hue with different chroma levels (saturations)
BarChart(Dept, fill=getColors(h=230, c=c(20,90), l=65))

# custom hue with different luminance levels (brightness)
Histogram(Salary, fill=getColors(h=230, c=65, l=c(90,30)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hcl }
\keyword{ color }


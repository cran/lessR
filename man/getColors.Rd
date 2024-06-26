\name{getColors}
\alias{getColors}

\title{Hue, Chroma, Luminance (HCL) Color Wheel or Specified Colors}

\description{

Generates color vectors, including HCL colors for qualitative and sequential color scales, and displays these internally generated as well as manually specified arbitrary colors. To avoid bias in comparing differently colored regions of a visualization, generates HCL colors by default with fixed values of chroma (saturation) and luminance (brightness) for a range of hues, by default ordered so that adjacent colors are as separated as possible. Also generates a sequence of HCL colors according to any chosen hue value in which implicit calls can vary chroma and luminance to Zeileis's et al. \code{sequential_hcl} function from Ihaka's et al. \code{colorspace} package, and also with pre-defined values such as \code{"blues"}. The function also processes any arbitrarily specified set of colors or colors generated from a custom range according to a beginning and ending specified color. The function also includes color palettes from the \code{viridis} and \code{wesanderson} packages.

In terms of workflow, use the function to select a set of colors from the resulting color rectangle/wheel. The function outputs the colors so that the function call can serve as an argument to parameters in other functions that require a sequence of one or more colors as input. The visualization of the color wheel or rectangle is not generated in this situation. After selecting the colors, pass to an argument for a visualization function such as for the \code{fill} parameter.
}

\usage{
  getColors(pal=NULL, end_pal=NULL,
         n=12, h=0, h2=NULL, c=NULL, l=NULL, transparency=0,
         in_order=NULL, fixup=TRUE, power=NULL,
         shape=c("rectangle", "wheel"), radius=0.9, border="lightgray",
         main=NULL, labels=NULL, labels_cex=0.8, lty="solid",
         output=NULL, quiet=getOption("quiet"), \dots)
}

\arguments{
  \item{pal}{Palette of specified colors to plot. If
  specified colors, then the following parameters are not relevant. Can also
  be pre-defined color sequences that trigger a sequence of colors from light to
  dark, such as \code{"blues"}, or \code{"distinct"} to maximize color
  separation.}
  \item{end_pal}{If specified, then generate a color continuum that begins at
   \code{pal} and ends at \code{end_pal}.}\cr

  \item{n}{Number of colors to display.}
  \item{h}{Beginning HCL hue, 0 to 360.}
  \item{h2}{Ending HCL hue, 0 to 360. Defaults to a value close to
       360. Requires \code{in_order} to be \code{FALSE}.}
  \item{c}{Value of HCL chroma (saturation). Respective default values
       for qualitative, sequential, and divergent scales are
       65, c(35,75), and 50.}
  \item{l}{Value of HCL luminance (brightness).
       Respective default values for qualitative, sequential, and
       divergent scales are 60, c(80,25), and c(40,70).}
  \item{transparency}{Transparency factor of the area of each slice from 0,
        no transparency to 1, full transparency.}
  \item{in_order}{If \code{TRUE}, orders the colors in order of their
        HCL hue values, the default for a "wheel". Otherwise maximizes
        the difference between adjacent colors hues to prepare for
        inclusion in visualizations with qualitative, discrete color scales.}
  \item{fixup}{R parameter name. If \code{TRUE}, then HCL values outside of
        the displayable RGB color space are transformed to fit into that space
        so as to display.}
  \item{power}{Power for generating a sequential or divergent HCL scale
       (via colorspace package) for potentially non-linear changes
       in chroma and luminance across the scale. Default for sequential is 1
       and for divergent 0.75.}\cr
   
  \item{shape}{Default is a \code{"rectangle"}, or specify a \code{"wheel"}.}
  \item{radius}{Size of wheel. Not applicable to the rectangular shape.}
  \item{border}{Color of the borders of each slice. Set to \code{"off"} or
       \code{"transparent"} to turn off.}\cr 
  
  \item{main}{Title. Unlike other \code{lessR} functions, there is a default
             title, turned off by explicitly setting to \code{NULL} or
             \code{""}.}
  \item{labels}{If \code{TRUE}, then displayed. For HCL qualitative scale,
       default is \code{TRUE}, otherwise \code{FALSE}}.
  \item{labels_cex}{Character expansion factor of labels relative to 1.}
  \item{lty}{Line type of the border.}\cr

  \item{output}{Default to produce text and graphics output when called
       directly from the console but not when called from a visualization
       function or a direct call in R Markdown, which requires
       \code{output=TRUE}.}
  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default with
       \code{\link{style}} function.}
  \item{\dots}{Other parameter values.}
}


\details{

I. HCL COLORS\cr
Generate a palette of colors according to the parameter \code{pal} in the form of a character string vector of their names, and also as a color wheel if not called from another function. The default value (for all but grayscale or white color themes) of \code{pal} is \code{"hues"}, which generates a qualitative palette of the specified number, \code{n}, of discrete HCL colors at the same chroma and luminance, respective default values of 65 and 60. With constant chroma and luminance the HCL color space provides a palette of colors with the same gray-scale intensities if desaturated. That means no brightness bias for viewing different colors that represent different areas, such as in a bar chart of two variables, or a pie chart. The primary constraint is that the HCL color space is not in a one-to-one correspondence with the RGB color space of computer monitors, so some HCL colors are approximated (with the default setting of the \code{fixup} parameter set to \code{TRUE}).

For \code{"hues"}, the default, the hue values and associated colors are expressed as HEX and RGB values. The first 12 generated discrete colors are  blue (240), brown (60), green (120), red (0), purple (275), turquoise (180), rust (30), olive (90), aqua (210), mulberry (330), emerald (150), and violet (300).

To have the generated colors be in the sequential order of hues, set \code{in_order} to \code{TRUE}, the default when \code{shape} is set to \code{"wheel"}. For about up to five or six colors adjacent values are still reasonably well distinguished even if in sequential order of hue number in the hcl space.\cr

II. COLOR SEQUENCE\cr
A second possibility generates a sequence of colors according to the value of \code{n} from a given start color to an ending color. To specify a custom range, set \code{pal} as the value of the first color, and then \code{end_pal} as the value of the last color in the color range. The colors in the sequence may or may not be of the same hue.

Or, access implicit calls Zeileis (2009) \code{sequential_hcl} and \code{diverge_hcl} functions from the \code{colorspace} package to access pre-defined color ranges including \code{"grays"}, which is the default if the color theme is \code{"gray"} or  \code{"white"}. Other predefined sequences are shown in the following table. Also can invoke the standard R color ranges of \code{"heat"}, \code{"terrain"}, and \code{"rainbow"}, or, preferably, their \code{colorspace} equivalents: \code{"rainbow_hcl"}, \code{"heat_hcl"}, and \code{"terrain_hcl"}. 

Can specify any value of hue with \code{h}. Can also provide custom values of chroma (c) and luminance (l), with either one a range of values defined as a vector of two values_ Default values are \code{c=100} and \code{l=c(75,35)}. That is, the color sequence is generated according to the given hue, \code{h}, with a chroma of 100 and luminance varying from 75 to the darker 45.

The predefined sequences consist of the following hues and color names, defined in 30 degree increments around the HCL color wheel. Visualize the color wheel with then discrete colors below with the lessR function \code{\link{getColors}}, specifically the function call \code{getColors(shape="wheel")}. Visualize sequential color scales for each of the colors below with the lessR function \code{\link{showPalettes}}.\cr

\tabular{lll}{
 colors \tab param \tab value\cr
----------- \tab ----- \tab -----\cr
\code{"reds"} \tab h \tab 0\cr
\code{"rusts"} \tab h \tab 30\cr
\code{"browns"} \tab h \tab 60\cr
\code{"olives"} \tab h \tab 90\cr
\code{"greens"} \tab h \tab 120\cr
\code{"emeralds"} \tab h \tab 150\cr
\code{"turquoises"} \tab h \tab 180\cr
\code{"aquas"} \tab h \tab 210\cr
\code{"blues"} \tab h \tab 240\cr
\code{"purples"} \tab h \tab 270\cr
\code{"violets"} \tab h \tab 300\cr
\code{"magentas"} \tab h \tab 330\cr
\code{"grays"} \tab c \tab 0\cr
----------- \tab ----- \tab -----\cr
}

The predefined color name can be provided as the first argument of the function call, that is, the value of \code{pal}, or the corresponding value of \code{h} (or \code{c} for gray scale) can be specified. The specifications are equivalent. To specify a divergent color scale, provide both the value of \code{pal} as the beginning value and the value of \code{end_pal} as the last value, such that both values are one of the pre-specified color ranges. In either situation, of sequential or divergent color scales, custom values of \code{c} and \code{l} can be provided.\cr


III. SPECIFIED COLORS\cr
The third possibility is to generate a color wheel from a specified set of color values. Set the value of \code{pal} according to the vector of these values. Specify the values with R color names (see the lessR function \code{\link{showColors}}), RGB values according to the \code{rgb} function or from related R color space functions such as \code{hcl}, or as hexadecimal codes.\cr

IV. OTHER INCLUDED COLOR PALETTES\cr
The following palettes are based on those from the viridis package: \code{"viridis"}, \code{"cividis"}, \code{"plasma"}, and \code{"spectral"}, though the palettes here are generated from the base R function \code{\link{hcl.colors}}. These palettes were developed to be more useable for varying types of color-blindness, as is the included palette \code{"Okabe-Ito"}. The Tableau default qualitative color palette is also included, identified by \code{"Tableau"}. 

Movie director Wes Anderson is known for is innovative color themes in his movies, which feature a combination of pastel colors and bold primary colors. The following palettes are from the wesanderson package, based the colors from his movies: "BottleRocket1", "BottleRocket2", "Rushmore1", "Rushmore", "Royal1", "Royal2", "Zissou1", "Darjeeling1", "Darjeeling2 ", "Chevalier1", "FantasticFox1", "Moonrise1", "Moonrise2", "Moonrise3", "Cavalcanti1", "GrandBudapest1", "GrandBudapest2", "IsleofDogs1", "IsleofDogs2". The generation of the corresponding palettes are with \code{type} set to \code{"continuous"} to generalize to palettes of any length. Note that this package is suggested, which means to use the package for the first time you will be prompted to install the package.

The palette \code{"distinct"} specifies a sequence of 20 colors manually chosen for the distinctiveness. The first five colors are from the qualitative sequence of \code{hcl} colors with \code{c=90} and \code{l=50}. To maximise color separation, the remaining 15 colors do not satisfy constance levels of \code{c} and \code{l}. Use such as for plotting with a \code{by} variable with up to 20 levels.

FUNCTION USAGE\cr
Use the function on its own, in which case the color rectangle/wheel visualization is generated as are the color values. The vector of color values may be saved in its own R object as the output of the function call. Or, use the function to set colors for other parameter values in other function calls. See the examples.
}

\value{
Colors are invisibly returned as a character string vector.
}

\references{
Gerbing, D. W. (2020). R Visualizations: Derive Meaning from Data, Chapter 10, NY: CRC Press.
}

\seealso{
\code{\link{hcl}}, \code{\link{palette.colors}}, \code{\link{hcl.colors}}, \code{\link{showColors}}
}


\examples{
# HCL color wheels/rectangles
#----------------------------
# set in_order to TRUE for hues ordered by their number

# color spectrum of 12 hcl colors presented in the order
#  in which they are assigned to discrete levels of a
#  categorical variable
getColors()

# color spectrum of 12 hcl colors ordered by hue from 0
#   by intervals of 360/12 = 30 degrees
getColors(in_order=TRUE)

# pastel hcl colors, set luminance to 85 from default of 50
getColors(in_order=TRUE, l=85)

# color wheel of 36 ordered hues around the wheel
getColors(n=36, shape="wheel", border="off")

# ggplot qualitative colors, here for 3 colors generated
#   in order of their hue numbers across the color wheel
#   starting at a hue of 15 degrees and luminance of 60
getColors(h=15, n=3, l=60, in_order=TRUE)


# HCL Qualitative Scale
# ---------------------
# default pre-defined 12 hcl colors that were manually reordered
#   so that adjacent colors achieve maximum separation
getColors()

# deep rich colors for HCL qualitative scale
getColors(c=90, l=45)


# HCL Sequential Scales
# ---------------------
# generate hcl blue sequence with c=60 and vary l
getColors("blues", labels=FALSE)

# generate yellow hcl sequence with varying chroma
getColors("browns", c=c(20,90), l=60)

# non-linear grayscale, more concentration of colors at the beginning
getColors("black", "white", n=24, power=0.75)

# generate custom hue color sequence close to colorbrewer Blues
# library(RColorBrewer)
# getColors(brewer.pal(6,"Blues"))
# compare, vary both l and c
getColors(h=230, n=6, l=c(96,30), c=c(5,80))

# a standard R color sequence
getColors("heat")

# from viridis
getColors("viridis", n=12)

# maximally distinct
getColors("distinct", n=20)


# HCL Divergent Scales
# --------------------
# seven colors from rust to blue
getColors("rusts", "blues", n=7)

# add a custom value of chroma, c, to make less saturated
getColors("rusts", "blues", n=7, c=45)


# Manual Specification of Colors
# ------------------------------
# individually specified colors
getColors(c("black", "blue", "red"))

# custom sequential range of colors
getColors(pal="aliceblue", end_pal="blue")


# Plots
# -----
d <- rd("Employee")

# default quantitative scale
bc(Dept, fill=getColors())
# or with implicit call to getColors
bc(Dept, fill="colors")
# or an implicit call with the blues
bc(Dept, fill="blues")
# or explicit call
bc(Dept, fill=getColors("blues"))

# custom hue with different chroma levels (saturations)
BarChart(Dept, fill=getColors(h=230, c=c(20,60), l=60))

# custom hue with different luminance levels (brightness)
# if explicitly calling getColors need to also specify n
Histogram(Salary, fill=getColors(h=230, c=60, l=c(90,30), n=10))

# use the default qualitative viridis color scale
bc(Dept, fill="viridis")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hcl }
\keyword{ color }


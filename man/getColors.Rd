\name{getColors}
\alias{getColors}

\title{Hue, Chroma, Luminance (hcl) Color Space Wheel or Specified Colors}

\description{
Generates colors and the corresponding color wheel by default for specified HCL colors at fixed values of chroma and luminance for a range of hues, by default ordered so that adjacent colors are as separated as possible. Also generates a range of colors defined by given beginning and ending colors, or according to pre-defined values such as \code{"blues"}. Also processes any specified set of colors. Outputs the colors so that the function call can serve as an argument to parameters in other functions that require a sequence of one or more colors as input, in which case the visualization of the color wheel is not generated.
}

\usage{
  getColors(clr=NULL, end.clr=NULL,
         n=5, h1=0, h2=NULL, c=100, l=65, fixup=TRUE,
         mixup=TRUE, radius=0.9, 
		 labels=TRUE, labels.cex=0.8, border="black", lty="solid",
         quiet=getOption("quiet"), \ldots)
}

\arguments{
  \item{clr}{Optional specified colors to plot. If list of
  specified colors, then the following parameters are not relevant. Can also
  be pre-defined color codes that trigger a range of colors from light to 
  dark, such as \code{"blues"}.}
  \item{end.clr}{If specified, then generate a color continuum that begins at 
   \code{clr} and ends at \code{end.clr}.}\cr

  \item{n}{Number of colors to display.}\cr

  \item{h1}{Beginning HCL hue, 0 to 360.}
  \item{h2}{Ending HCL hue, 0 to 360. Defaults to a value close to 360.
        Specify only if a limited range of colors is desired.
        Requires \code{mixup} to be \code{FALSE}.}
  \item{c}{Constant value of chroma.}
  \item{l}{Constant value of luminance.}
  \item{fixup}{R parameter name. If \code{TRUE}, then HCL values outside of
        the displayable RGB color space are transformed to fit into that space
        so as to display.}\cr

  \item{mixup}{If \code{TRUE}, will mixup the colors so that
        adjacent colors are maximally different from other. Color hues are
        exactly the same for different sizes of \code{n}
        if set to \code{TRUE} otherwise approximated to maximize the difference
		between colors.}
  \item{radius}{Size of wheel.}\cr


  \item{labels}{If \code{TRUE}, then displayed.}
  \item{labels.cex}{Character expansion factor of labels relative to 1.}
  \item{border}{Color of the borders of each slice. Set to \code{"off"} or
       \code{"transparent"} to turn off.}
  \item{lty}{Line type of the border.}\cr

  \item{quiet}{If set to \code{TRUE}, no text output. Can change system default
       with \code{\link{style}} function.}
  \item{\ldots}{Other parameter values.}
}


\details{

I. HCL COLORS\cr
Generate a range of colors according to the parameter \code{clr} in the form of a character string vector of their names, and also as a color wheel if not called from another function. The default value (for all but grayscale or white color themes) of \code{clr} is \code{"hcl"}, which generates a palette of the specified number, \code{n}, of discrete HCL colors at the same chroma and luminance, respective default values of 65 and 100. With constant chroma and luminance the HCL color space can provide a palette of colors that have the same gray-scale intensities if desaturated. That means no brightness bias for viewing different colors that represent different areas, such as in a bar chart of two variables, or a pie chart. The primary qualification is that the HCL color space is not in a one-to-one correspondence with the RGB color space of computer monitors, so some HCL colors are approximated (with the default setting of the \code{fixup} parameter set to \code{TRUE}).

For \code{"hcl"} the hue values and associated colors are expressed as HEX and RGB values. The first three generated discrete colors, with their associated HCL hues at the default values of chroma (65) and luminance (100), are blue (240), red (0), and green (120), which maintains the maximum hue separation for three values at 120 degrees. The next three generated colors are brown (60), plum (300), and turquoise (180), such that all six colors maintain the maximum color separation of 60 degrees. The last six colors are olive (90), steelblue (270), rust (30), aqua (210), violet (330) and marine green (150), which maintain the maximum color separation of 30 degrees.

To have the generated colors be in the sequential order of hues, set \code{mixup} to \code{FALSE}. For about up to five colors adjacent values are still reasonably well distinguished even if in sequential order of hue number in the hcl space. The colors are generated to maintain maximum hue separation regardless of the number of colors, so will differ somewhat from the list of colors when re-ordered as described in the previous paragraph.\cr

II. COLOR RANGE\cr
A second possibility is to generate a range of colors according to the value of \code{n} from a given start color to an ending color, which may or may not be of the same hue. To specify a custom range, set \code{clr} as the value of the first color, and then \code{end.clr} as the value of the last color in the color range.  Or, access pre-defined color ranges including \code{"grays"}, which is the default if the color theme is \code{"gray"}. Other predefined ranges include \code{"blues"}, \code{"reds"}, \code{"greens"}, and the standard R color ranges of \code{"heat"}, \code{"terrain"}, and \code{"rainbow"}.\cr

III. SPECIFIED COLORS\cr
The third possibility is to generate a color wheel from a specified set of color values. Set the value of \code{clr} according to the vector of these values. Specify the values with R color names (see the lessR function \code{\link{showColors}}), RGB values according to the \code{rgb} function or from related R color space functions such as \code{hcl}, or as hexadecimal codes.\cr

FUNCTION USEAGE\cr
Use the function on its own, in which case the color wheel visualization is generated as are the color values. The vector of color values may be saved in its own R object as the output of the function call. Or, use the function to set colors for other parameter values in other function calls. See the examples.
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

# save the vector of color values and list
mycolors <- getColors()
mycolors

# set luminance to 90 for 8 hcl colors
getColors(n=8, l=90)

# generate a color wheel with 10 slices, starting at h=20
getColors(h1=20, h2=200, n=10)

# manual entry of colors
getColors(c("black", "blue", "red"))

# manual entry of an ordered progression
# with so many colors make labels smaller
# save colors in mycolors character vector
mycolors <- getColors(n=25, clr="aliceblue", end.clr="blue", labels.cex=.8)

# call within the BarChart function to specify the HCL colors of the 5 bars
mydata <- rd("Employee", format="lessR", quiet=TRUE)
BarChart(Dept, fill=getColors(n=5))

# store color vector in an object for re-use
cc <- getColors(n=5, l=50)
BarChart(Dept, fill=cc)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hcl }
\keyword{ color }






\name{STL}
\alias{STL}

\title{Seasonal and Trend Decomposition of a Time series via Loess}

\description{
Decompose a time series into seasonal, trend and irregular components using loess, a wrapper to provide additional information to the Base R \code{stl} function and accept more general input beyond the \code{stl} required time series object input. This function also accepts a Base R time series from the global environment as input, but also accepts data in the traditional \code{x},\code{y} format where \code{x} is a variable of type \code{Date}. Moreover, the \code{Date} variable can be inferred from digital character string inputs. The time unit of the input dates can also be aggregated, such as changing monthly dates to quarterly dates.
}

\usage{
STL(x, y=NULL, data=d, filter=NULL,
    ts_format=NULL, ts_unit=NULL, ts_agg=c("sum","mean"),
    show_range=FALSE, robust=FALSE, quiet=FALSE, do_plot=TRUE)
}

\arguments{
  \item{x}{Dates for the time series within a data frame, or a time series object
        created with the Base R function \code{ts}.}
  \item{y}{Numerical variable that is planted in the time series, not used if 
        \code{x} is a time series object.}
  \item{data}{Data frame that contains the \code{x} and
        \code{y} variables. Default data frame is \code{d}.}
  \item{filter}{A logical expression that specifies a subset of rows of
        the data frame to analyze.}
  \item{ts_format}{A specified format (for R function \code{as.Date()})
    that describes the values of the date variable on the x-axis, 
    needed if the function cannot identify the correct date format to properly
    decode the given date values. For example, describe a character string
    date such as \code{"09/01/2024"} by the format \code{"\%m/\%d/\%Y"}.
    See \code{details} for more information.}
  \item{ts_unit}{Specify the time unit from which to plot a time series,
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
  \item{show_range}{Display the range for each component.}
  \item{robust}{\code{stl()} parameter for a more robust solution.}
  \item{quiet}{If \code{TRUE}, no text output to the console.}
  \item{do_plot}{If \code{FALSE}, no plot.}
}

\details{
PURPOSE\cr
Obtain and plot the seasonal, trend, and the irregular (remainder or residual) components of a time series using the Base R \code{stl} function. The corresponding plot is of four panels, one for the data and one each for the seasonal, trend, remainder components. Provide additional information comparing the relative sizes of the components in the form of the percent of variance of each component accounted for and the range of values of each component. 

Seasonality is detected over a year, such as four quarters in a year or 12 months in a year. The exception is for daily data, for which seasonality can be indicated by the time unit of \code{"days7"}, which will evaluate seasonality over the seven days of a week.

RANGE BARS\cr
By definition, the data shows the most variability compared to the three components. If the four panels were scaled on the same y-axis, then the relative magnitude of the variations in each of the components, such as assessed by the ranges of each of their values, would be more directly observable. For example, if seasonality has no  practical presence in the data, then the amplitude of the seasonal plot, the range of the seasonal component values, would be a small fraction the amplitude of the data plot, only reflecting random noise. Plotted on the same panel, the comparison would be direct. 

Instead, however, the plots of the data and each of the three components are drawn such that each component is plotted on its own panel with its own scale with the most detail possible. The purpose of the range bars is to show a relative scale for comparison across the panels. Each range bar is a magnification indicator. The larger the bar, the more expanded is the corresponding panel, which means the smaller the variation of the component relative to the range of the data. Shrinking the size of a range bar along with the corresponding panel to the same size as the range bar for the data, the smallest range bar, would show the comparison directly.

DATE FORMAT\cr
\code{STL()} makes reasonable attempt to decode a character string date value as the \code{x}-axis variable as read from a text data file such as a \code{csv} file. Some date formats are not available for conversion by default, such as date values that include the name of the month instead of its number. And, in general, there can be no guarantee that a date format is not miss inferred as they can be inherently ambiguous. If the default date conversion is not working, then manually supply the date format following one of the format examples in the following table according to the parameter \code{ts_format}.

\tabular{ll}{
Date \tab Format\cr
\"2022-09-01\" \tab \"\%Y-\%m-\%d\"\cr 
\"2022/9/1\" \tab \"\%Y/\%m/\%d\"\cr
\"2022.09.01\" \tab \"\%Y.\%m.\%d\"\cr
\"09/01/2022\" \tab \"\%m/\%d/\%Y\"\cr
\"9/1/15\" \tab \"\%m/\%d/\%y\"\cr
\"September 1, 2022\" \"\tab \%B \%d, \%Y\"\cr
\"Sep 1, 2022\" \tab \"\%b \%d, \%Y\"\cr
\"20220901\" \tab \"\%Y\%m\%d\"\cr
}



For emphasis, each range bar is displayed in a pale yellow color. 
}

\value{
An \code{stl()} object and text to the console.

Here is an example of saving the output to an R object with any valid R name, such as \code{s}: \code{s <- STL(Price)}. To see the names of the output objects for that specific analysis, enter \code{names(s)}. To display any of the objects, precede the name with \code{s$}, such as to view the saved frequency distribution with \code{s$out_freq}. Or, only list the name of the output object to get the four output components displayed as a single data frame. View the output at the R console or within a markdown document that displays your results.

\code{x.name}: Name of the date variable on the horizontal axis.
\code{trend}: Value of the \code{trend} component for each x-value.
\code{season}: Value of the \code{season} component for each x-value.
\code{error}: Value of the \code{error} component for each x-value.
}

\author{David W. Gerbing (Portland State University; \email{gerbing@pdx.edu})}

\seealso{
\code{\link{stl}}.
}

\examples{
# read the built-in data set dataStockPrice
d <- Read("StockPrice")
# extract just the data for Apple, the first 473 rows of data
d <- d[1:473,]

# manually request the STL for d
STL(Month, Price)

# enter a time series, here one that comes with Base R
# monthly average air temperatures in Nottingham, UK from 1920 to 1939
# get the time series into the global environment
my.ts <- nottem
STL(my.ts)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stl }
\keyword{ time series }


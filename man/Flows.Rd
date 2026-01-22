\name{Flows}
\alias{Flows}
\title{Sankey Flow Diagram (2- or 3-Stage) with Plotly}

\description{
Create a Sankey flow diagram between source and target categories with weighted links that visualizes movement of volume between categorical stages. Supports either two stages (A -> B) or three stages (A -> B -> C). Flows (links) are colored by the leftmost source category and retain that color across all legs. Node fills can be neutral gray or (by default) the left/source nodes can inherit the flow colors while intermediate and right nodes remain gray. In RStudio, the visualization renders in the \code{Viewer} pane.}

\usage{
Flows(
  value,
  stage1,
  stage2,
  stage3 = NULL,
  data = d,
  # appearance
  title = NULL,
  fill = NULL,
  link_alpha = 0.55,
  nodes_gray = FALSE,
  neutral_gray = "gray80",
  border_gray  = "gray60",
  lift_y = 0,
  labels_size = 1.00,
  digits_d = 0
)
}

\arguments{
  \item{stage1}{Column name for the left (source) stage. May be given unquoted (recommended) or as a character string.}
  \item{stage2}{Column name for the middle (or right, in 2-stage) stage. Unquoted or character string.}
  \item{stage3}{Optional column name for the right stage (3-stage). If \code{NULL}, a 2-stage diagram is created. Unquoted or character string.}
  \item{value}{Column name with non-negative weights (e.g., counts, volume, tonnage) used to determine link thickness. Unquoted or character string.}
  \item{data}{A data frame containing the stages and the weight column. If \code{NULL}, the function will look for the columns in the calling environment.}

  \item{title}{Optional chart title. If \code{NULL}, a title is constructed from the provided stage names.}
  \item{fill}{Optional color range name or vector used for \emph{source} categories. If \code{NULL} (default), the range is inherited from the current theme via \code{.color_range(.get_fill(getOption("theme")), n.x)}, where \code{n.x} is the number of unique \code{stage1} categories.}
  \item{link_alpha}{Numeric from 0 to 1. Transparency for flows (links). Default \code{0.55}.}
  \item{nodes_gray}{Logical. If \code{FALSE} (default), left/source nodes are colored (matching their flows) and all other nodes are neutral gray. If \code{TRUE}, all nodes are neutral gray.}
  \item{neutral_gray}{Color for gray node fills. Defaults to \code{"gray80"}.}
  \item{border_gray}{Color for node borders. Defaults to \code{"gray60"}.}
  \item{lift_y}{Proportion of vertical shift of the displayed chart.
        Positive values lift the chart upward.}
  \item{labels_size}{Size of displayed labels, with 0.90 the default.}
  \item{digits_d}{Integer number of decimals for link values shown in the hover. Default \code{0}.}
}

\details{
\strong{Flow semantics}\cr
\code{Flows} is intended for directional flows where the width of each band represents the amount moving from one category to the next (e.g., supply chain, traffic, energy, user funnels). It is \emph{not} a general association plot for arbitrary cross-tabulations without directional meaning. For hierarchical composition without directional flow, consider \code{\link{Chart}} with parameter \code{type} set to \code{"sunburst"}, \code{"treemap"}, \code{"radar"}, or \code{"pie"} instead.

\strong{Color mapping}\cr
Flows are colored by the \emph{leftmost} source category (\code{stage1}). In a 3-stage diagram, colors are carried from \code{stage1} through \code{stage2} to \code{stage3} so each link retains its origin color. When \code{nodes_gray = FALSE} (default), only source nodes inherit the flow colors; middle and right nodes are neutral gray. When \code{nodes_gray = TRUE}, all nodes are gray. If \code{fill} is \code{NULL}, colors are drawn from the current lessR theme (\code{getOption("theme")}) via \code{.get_fill()} and \code{.color_range()}.

\strong{Layout}\cr
Nodes are arranged in fixed columns: left = \code{stage1}, middle = \code{stage2}, right = \code{stage3} (if provided). Vertical positions are spaced evenly within each column. The diagram prints automatically in interactive sessions and returns a Plotly htmlwidget (invisibly) for programmatic use.
}

\value{
An object of class \code{htmlwidget} (Plotly). The widget prints
automatically in interactive sessions; otherwise it is returned (invisibly) for
further composition or saving.
}

\examples{
\dontshow{if (interactive()) \{ }
# Example data: Source -> Port -> Destination with volumes
SupplyChain <- data.frame(
  Source      = c("China","China","China","Germany","Germany",
                  "Brazil","Brazil","USA","USA"),
  Port        = c("Los Angeles","Seattle","Houston","New York","Baltimore",
                  "Miami","Houston","Los Angeles","Miami"),
  Destination = c("Portland","Chicago","Dallas","Boston","Philadelphia",
                  "Atlanta","Dallas","San Francisco","Atlanta"),
  Volume      = c(320,180,150,220,130,200,100,170,140)
)

# 3-stage: Source -> Port -> Destination
Flows(Volume, Source, Port, Destination, data=SupplyChain)

# 2-stage: Source -> Destination, ignore Port
Flows(Volume, Source, Desination, data=SupplyChain)

# All nodes neutral gray
Flows(Volume, Source, Port, Destination, data=SupplyChain,
       nodes_gray=TRUE, link_alpha=0.6)

# Supply a specific range (overrides theme), show one decimal in hover
Flows(Volume, Source, Port, Destination, data=SupplyChain,
       fill="blues", digits_d=1)
\dontshow{\} }
}

\seealso{
\code{\link{Chart}} for other Plotly-based charts in \pkg{lessR}, including
\code{type="bar"}, \code{type="sunburst"}, \code{"treemap"}, \code{"pie"}, \code{"radar"}, and \code{"bubble"}. \code{Histogram} and \code{Plot} also create Plotly-based charts.
}

\author{
David W. Gerbing
}

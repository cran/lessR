.bcParamValid <-
function (y.miss, by.miss, facet1.miss, Trellis, sort,
          fill_split, fill_scaled, fill_chroma, theme,
          fill.miss, labels_position, stat.miss) {

  if (!by.miss  &&  !facet1.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "by  and  facet1  parameters not currently available at the same time.\n\n")
  }

  if (Trellis  &&  sort != "0") {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Sort not applicable to Trellis plots\n\n")
  }

  if (!fill_scaled  &&  !is.null(fill_split)  &&  !fill.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "fill_split assigns its own color based on the theme\n",
      "  either drop  fill_split  or drop  fill  parameter values\n\n")
  }

  if (fill_scaled  &&  !by.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "fill_scaled only applies if no  by  variable\n\n")
  }

  if (!(labels_position %in% c("in", "out"))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "labels_position  must be set to \"in\", \"out\"\n\n")
  }

  if (!stat.miss && y.miss) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "parameter  stat  is meaningless if no y-variable to transform\n\n")
  }

  if (!stat.miss && Trellis) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Only the original data work with Trellis plots,",
      " no data aggregation with parameter stat.",
      " Use  by  instead of  facet1.\n\n")
  }

}


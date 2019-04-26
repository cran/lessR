.param.old <-
function (...) {

  # check for dated parameters no longer used
  dots <- list(...)
  
  if (!is.null(dots)) if (length(dots) > 0) {

    for (i in 1:length(dots)) {

#     if (grepl("fill", names(dots)[i], fixed=TRUE)  ||
#         grepl("color_", names(dots)[i], fixed=TRUE)) {
#       cat("\n"); stop(call.=FALSE, "\n","------\n",
#         "The parameter list for this function is much shortened by moving\n",
#         "most color and related style attributes to function:  style\n\n",
#         "Example: Here set the theme to gold with a fill color of ",
#         "\"powerderblue\"\n\n",
#         "style(\"gold\", fill=\"powderblue\")\n\n",
#         "Enter   style(show=TRUE)  to see all the options\n",
#         "Enter   ?style  to view the help file\n\n")
#     }

      if (grepl("stroke", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "stroke  replaced with:  color\n",
          "e.g., ellipse_color  instead of  ellipse.stroke\n\n",
          "Also, now modify via function:  style\n\n",
          "Example: style(\"gold\", color=\"powderblue\")\n\n",
          "Enter   style(show=TRUE)  to see all the options\n",
          "Enter   ?style  to view the help file\n\n")
      }

      if (grepl("col.", names(dots)[i], fixed=TRUE)) 
        if (names(dots)[i] != "col.names"  &&
            names(dots)[i] != "col.main"  &&
            names(dots)[i] != "col.lab"  &&
            names(dots)[i] != "col.sub") {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "color options dropped the  col. prefix\n",
            "eg., fill, instead of col_fill\n\n",
          "Also, now can modify fill and related colors\n",
          "  with function:  style\n\n")
      }

      if (names(dots)[i] %in% c("tm.adj", "rm.adj", "bm.adj", "lm.adj")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use margin_adj vector: top, right, bottom, left\n",
          "e.g., margin_adj=c(.5,0,0,0) moves top margin in 0.5 in \n\n")
      }
      if (names(dots)[i] %in% c("xlab_adj", "ylab_adj")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use lab_adj vector: xlab, ylab\n",
          "e.g., lab_adj=c(.5,0) moves x-axis label in 0.5 in \n\n")
      }

      if (names(dots)[i] == "area") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  area=TRUE  renamed  fill=\"on\" or area_fill=\"on\"\n\n")
      }

      if (names(dots)[i] == "ref") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  ref  renamed  from  or  to  for Read and Write\n\n")
      }

      if (names(dots)[i] == "prop") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  prop  renamed  stack100 for two variables\n",
          "and  stat_x=\"proportion\"  for one variable\n\n")
      }

      if (names(dots)[i] == "proportion") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  proportion  renamed  stat_x=\"proportion\" \n\n")
      }

      if (names(dots)[i] == "band") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  band  renamed  rug\n\n")
      }

      if (names(dots)[i] == "gen_color") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  gen_color  renamed  color_gen\n\n")
      }

      if (names(dots)[i] == "nrm_color") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  nrm_color  renamed  colr.nrm\n\n")
      }

      if (names(dots)[i] == "legend_loc") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  legend_loc  renamed  legend_position\n\n")
      }

      if (names(dots)[i] == "values_pos") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  values_pos  renamed  values_position\n\n")
      }

      if (names(dots)[i] == "values_cex") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  values_cex  renamed  values_size\n\n")
      }

      if (names(dots)[i] == "sort_x") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  sort_x  renamed  sort\n\n")
      }
      
      if (grepl("colors", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "colors option removed because the fill parameter\n",
          "  and getColors function offer more functionality\n\n")
      }

      if (grepl("rotate", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Use the style function for rotate_x, rotate_y, offset\n\n")
      }
      if (grepl("fit.line", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "fit.line options dropped the  .line suffix\n",
          "use  fit, instead of fit.line\n\n")
      }
      if (grepl("by.group", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "by.group option is now just  by, its original meaning\n",
          "use  by1  and  by2  for 1 and 2 variable Trellis graphics\n\n")
      }
      if (names(dots)[i] %in% c("x.start","x.end","y.start","y.end")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "x.start, x.end, y.start, and y.end no longer used\n\n",
          "Instead use the standard R xlim and ylim parameters,\n",
          "such as xlim=c(0,40) to specify from 0 to 40. Same for ylim\n\n")
      }
      if (names(dots)[i] == "hist.counts") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  hist.counts  renamed  values\n\n")
      }
      if (names(dots)[i] == "line_chart") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  line_chart  renamed  run\n\n")
      }
      if (names(dots)[i] == "rotate.values") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Rotation of the axis labels can now be applied to both axes\n\n",
          "option  rotate.values  renamed  rotate_x or rotate_y\n\n")
      }
      if (names(dots)[i] == "line_width") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  line_width  renamed  lwd\n\n")
      }
      if (names(dots)[i] == "bubble_size") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble_size  renamed  radius\n\n")
      }
      if (names(dots)[i] == "bubble_scale") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble_scale  renamed  power\n\n")
      }
      if (names(dots)[i] == "bubble_text") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble_text  is now modified in function:  style\n\n")
      }
      if (names(dots)[i] == "size.out") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  size.out  renamed  out_size\n\n")
      }
      if (names(dots)[i] == "shape.out") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  shape.out  renamed  out_shape\n\n")
      }
      if (names(dots)[i] == "topic") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  topic  renamed  values\n\n")
      }
      if (names(dots)[i] == "kind") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  kind  is no longer active\n\n")
      }
      if (names(dots)[i] == "object") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  object  is no longer active\n\n",
          "use line_chart=TRUE to get a line chart\n",
          "set size=0 to remove points from the plot\n\n")
      }
      if (names(dots)[i] == "auto") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  auto  renamed  enhance\n\n")
      }
      if (names(dots)[i] == "values") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  values  renamed  stat_x  or  stat_yx\n\n")
      }
      if (names(dots)[i] == "type") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  type  renamed  stat\n\n")
      }
      if (names(dots)[i] == "smooth_trans") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  smooth_trans  renamed  smooth_exp\n\n")
      }
      if (names(dots)[i] == "knitr.file") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "knitr.file  no longer used\n",
          "Instead use  Rmd  for R Markdown file\n\n")
      }
      if (names(dots)[i] == "diag") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "diag  option no longer available\n\n")
      }
      if (names(dots)[i] == "low_color") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  low_color  renamed  low_fill\n\n")
      }
      if (names(dots)[i] == "fill_ellipse") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  fill_ellipse  renamed  ellipse_fill\n\n")
      }
      if (names(dots)[i] == "color_ellipse") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  color_ellipse  renamed  ellipse_color\n\n")
      }
      if (names(dots)[i] == "color_fit") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  color_fit  renamed  fit_color\n\n")
      }
      if (names(dots)[i] == "lwd.fit") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  lwd.fit  renamed  fit_lwd\n\n")
      }
      if (names(dots)[i] == "se.fit") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  se.fit  renamed  fit_se\n\n")
      }
      if (names(dots)[i] == "box") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  box  renamed  panel_color\n\n")
      }
      if (names(dots)[i] == "bg") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bg  renamed  panel_fill\n\n")
      }
      if (names(dots)[i] == "axes") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  axes  renamed  axis_text_color\n\n")
      }
      if (names(dots)[i] == "boxplot") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use  violin  instead of boxplot\n\n")
      }
      if (names(dots)[i] == "addtop")   # BarChart
        cat("\naddtop  is now a multiplicative factor instead of additive\n\n")

      if (names(dots)[i] == "count.levels") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use  count.labels  instead of count.levels\n\n")
      }
    }
  }
}

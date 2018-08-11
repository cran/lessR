.param.old <-
function (...) {

  # check for dated parameters no longer used
  dots <- list(...)
  
  if (!is.null(dots)) if (length(dots) > 0) {

    for (i in 1:length(dots)) {

      if (grepl("fill", names(dots)[i], fixed=TRUE)  ||
          grepl("color.", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "The parameter list for this function is much shortened by moving\n",
          "most color and related style attributes to function:  style\n\n",
          "Example: Here set the theme to gold with a fill color of ",
          "\"powerderblue\"\n\n",
          "style(\"gold\", fill=\"powderblue\")\n\n",
          "Enter   style(show=TRUE)  to see all the options\n",
          "Enter   ?style  to view the help file\n\n")
      }

      if (grepl("stroke", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "stroke  replaced with:  color\n",
          "e.g., ellipse.color  instead of  ellipse.stroke\n\n",
          "Also, now modify via function:  style\n\n",
          "Example: style(\"gold\", color=\"powderblue\")\n\n",
          "Enter   style(show=TRUE)  to see all the options\n",
          "Enter   ?style  to view the help file\n\n")
      }

      if (grepl("col.", names(dots)[i], fixed=TRUE)) 
        if (names(dots)[i] != "col.main"  &&
            names(dots)[i] != "col.lab"  &&
            names(dots)[i] != "col.sub") {
          cat("\n"); stop(call.=FALSE, "\n","------\n",
            "color options dropped the  col. prefix\n",
            "eg., fill, instead of col.fill\n\n",
          "Also, now can modify fill and related colors\n",
          "  with function:  style\n\n")
      }

      if (names(dots)[i] %in% c("tm.adj", "rm.adj", "bm.adj", "lm.adj")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use margin.adj vector: top, right, bottom, left\n",
          "e.g., margin.adj=c(.5,0,0,0) moves top margin in 0.5 in \n\n")
      }
      if (names(dots)[i] %in% c("xlab.adj", "ylab.adj")) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Now use lab.adj vector: xlab, ylab\n",
          "e.g., lab.adj=c(.5,0) moves x-axis label in 0.5 in \n\n")
      }

      if (names(dots)[i] == "sort.x") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option sort.x  renamed  sort\n\n")
      }
      
      if (grepl("colors", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "colors option removed because the fill parameter\n",
          "  and getColors function offer more functionality\n\n")
      }

      if (grepl("rotate", names(dots)[i], fixed=TRUE)) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Use the style function for rotate.x, rotate.y, offset\n\n")
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
      if (names(dots)[i] == "line.chart") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  line.chart  renamed  run\n\n")
      }
      if (names(dots)[i] == "rotate.values") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Rotation of the axis labels can now be applied to both axes\n\n",
          "option  rotate.values  renamed  rotate.x or rotate.y\n\n")
      }
      if (names(dots)[i] == "line.width") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  line.width  renamed  lwd\n\n")
      }
      if (names(dots)[i] == "bubble.size") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble.size  renamed  radius\n\n")
      }
      if (names(dots)[i] == "bubble.scale") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble.scale  renamed  power\n\n")
      }
      if (names(dots)[i] == "bubble.text") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bubble.text  is now modified in function:  style\n\n")
      }
      if (names(dots)[i] == "size.out") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  size.out  renamed  out.size\n\n")
      }
      if (names(dots)[i] == "shape.out") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  shape.out  renamed  out.shape\n\n")
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
          "use line.chart=TRUE to get a line chart\n",
          "set size=0 to remove points from the plot\n\n")
      }
      if (names(dots)[i] == "type") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  type  renamed  object\n\n")
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
      if (names(dots)[i] == "low.color") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  low.color  renamed  low.fill\n\n")
      }
      if (names(dots)[i] == "fill.ellipse") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  fill.ellipse  renamed  ellipse.fill\n\n")
      }
      if (names(dots)[i] == "color.ellipse") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  color.ellipse  renamed  ellipse.color\n\n")
      }
      if (names(dots)[i] == "color.fit") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  color.fit  renamed  fit.color\n\n")
      }
      if (names(dots)[i] == "lwd.fit") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  lwd.fit  renamed  fit.lwd\n\n")
      }
      if (names(dots)[i] == "se.fit") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  se.fit  renamed  fit.se\n\n")
      }
      if (names(dots)[i] == "box") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  box  renamed  panel.color\n\n")
      }
      if (names(dots)[i] == "bg") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  bg  renamed  panel.fill\n\n")
      }
      if (names(dots)[i] == "axes") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "option  axes  renamed  axis.text.color\n\n")
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

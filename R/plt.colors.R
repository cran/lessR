.plt.colors <-
function(object, nn_col, n.by, segments, theme, fill, fill.miss,
            color, color.miss, area_fill, area_fill.miss, trans, stack,
            n.ycol, n.y_var, ord.by.call, run, size.pt) {

  n.clrs <- max(nn_col, n.by)  # n_col goes to lattice


  ### area fill
  ### ---------

  if (segments) {

    # fill can substitute for area_fill if no points
    if (!fill.miss && area_fill.miss && all(size.pt==0)) {
      area_fill <- fill
      area_fill.miss <- FALSE
      fill.miss <- TRUE
    }

    if (area_fill[1] == "on") {  # "on" only applies to one value
      if (n.y_var == 1)
        area_fill <- getOption("violin_fill")
      else
        area_fill <- .get_fill()
    }

    if (!("transparent" %in% area_fill)) {
      if (!stack && n.ycol > 1)  {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Filling a color for the areas under multiple curves only\n",
          "  meaningful if the curves are stacked on each other,\n",
          "  so set:  stack=TRUE.\n\n")
      }
    }

    if (n.clrs > 1) {
      if (!run) {  # area_fill for multiple run plots not meaningful
        if ("transparent" %in% area_fill) {
          qual_pal <- ifelse (theme %in% c("gray", "white"), "grays", "hues")
          area_fill <- getColors(qual_pal, n=n.clrs, output=FALSE)
        }
        else
          area_fill <- .color_range(area_fill, n.clrs)  # interpret blues, etc
      }


      color <- .plt.fill(color, color.miss, ord.by.call, n.clrs, n.clrs, theme)

      if (object=="both"  &&  color.miss  &&  all(area_fill!="transparent")) {
        color <- area_fill
        color.miss <- FALSE
      }
    }  # end n.clrs > 1

    if ((trans > 0)) {  # default fill trans is 0.1
       if (area_fill[1] != "transparent")
         area_fill <- .maketrans(area_fill, (1-trans)*256)
    }

  }  # end segments  --- area_fill


  ### fill and color
  ### --------------

  # with multiple colors and either just fill or color specified,
  #   need both fill and color to be the same unless one is "transparent"
  if (n.clrs > 1) {
    if (fill.miss && !color.miss) {
      if (!("transparent" %in% color)) {
        fill <- color
        fill.miss <- FALSE
      }
    }
    if (!fill.miss && color.miss) {
      if (!("transparent" %in% fill)) {
        color <- fill
        color.miss <- FALSE
      }
    }
  }

  nmC <- c("reds", "rusts", "browns", "olives", "greens", "emeralds",
          "turquoises", "aquas", "blues", "purples", "violets",
          "magentas", "grays")
  nmR <- c("rainbow", "heat", "terrain")
  nmV<- c("viridis", "cividis", "plasma", "spectral")
  nmO<- c("Okabe-Ito")
  nmD<- c("distinct")
  nmT <- c("Tableau")
  nm <- c(nmC, nmR, nmV, nmO, nmD, nmT)

  # set pt_fill
  color_done <- FALSE
  if (fill.miss) {
    if (n.clrs == 1)
      pt_fill <- getOption("pt_fill")
    else
      pt_fill <- .color_range(.get_fill(), n.clrs)
  }
  else {
    if (!(fill[1] %in% c("hues", nm)))  # if a by var, then length(fill) > 1
      pt_fill <- fill
    else {
      pt_fill <- .color_range(fill, n.clrs)
      pt_color <- rep("transparent", n.clrs)
      color_done <- TRUE
    }
  }

  if (trans > 0)  # default fill trans is 0.1
    pt_fill <- .maketrans(pt_fill, (1-trans)*256)

  # set pt_color, could also be line color if time series
  if (!color_done) {
    if (color.miss) {
      if (n.clrs == 1)
        pt_color <- getOption("pt_color")
      else
        pt_color <- .color_range(.get_fill(), n.clrs)  # see if range
    }
    else
      pt_color <- color
  }

  return(list(pt_fill=pt_fill, pt_color=pt_color, area_fill=area_fill))

}

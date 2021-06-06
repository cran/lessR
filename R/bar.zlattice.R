.bar.lattice <- 
function(x, by1, by2, nrows, ncols, asp, prop,
         fill, color,
         trans, size.pt, xlab, ylab, main,
         rotate_x, offset,
         width, height, pdf_file,
         segments_x, breaks, c.type, quiet) {

  if (!quiet)
    cat("[Trellis graphics from Deepayan Sarkar's lattice package]\n")

  panel_fill <- getOption("panel_fill")
  panel_color <- getOption("panel_color")

  grid_x_color <- ifelse(is.null(getOption("grid_x_color")), 
    getOption("grid_color"), getOption("grid_x_color"))
  grid_y_color <- ifelse(is.null(getOption("grid_y_color")), 
    getOption("grid_color"), getOption("grid_y_color"))
 
  grid_x_lwd <- ifelse(is.null(getOption("grid_x_lwd")), 
    getOption("grid_lwd"), getOption("grid_x_lwd"))
  grid_y_lwd <- ifelse(is.null(getOption("grid_y_lwd")), 
    getOption("grid_lwd"), getOption("grid_y_lwd"))

  grid_x_lty <- ifelse(is.null(getOption("grid_x_lty")), 
    getOption("grid_lty"), getOption("grid_x_lty"))
  grid_y_lty <- ifelse(is.null(getOption("grid_y_lty")), 
    getOption("grid_lty"), getOption("grid_y_lty"))

  axis_x_color <- ifelse(is.null(getOption("axis_x_color")), 
    getOption("axis_color"), getOption("axis_x_color"))
  axis_y_color <- ifelse(is.null(getOption("axis_y_color")), 
    getOption("axis_color"), getOption("axis_y_color"))
  # axis color is panel_color unless axis_color is changed from default
  theme <- getOption("theme")
  sub_theme <- getOption("sub_theme")
  if (sub_theme != "black")  {  
     panel_color <- ifelse (axis_x_color != "gray15", axis_x_color, "#DED9CD")
     if (panel_color == "#DED9CD")
       panel_color <- ifelse (axis_y_color != "gray15", axis_y_color, "#DED9CD")
  }
  else {
     panel_color <- ifelse (axis_x_color != "gray55", axis_x_color, "gray55")
     if (panel_color == "gray55")
       panel_color <- ifelse (axis_y_color != "gray55", axis_y_color, "gray55")
  }

  axis_x_text_color <- ifelse(is.null(getOption("axis_x_text_color")), 
    getOption("axis_text_color"), getOption("axis_x_text_color"))
  axis_y_text_color <- ifelse(is.null(getOption("axis_y_text_color")), 
    getOption("axis_text_color"), getOption("axis_y_text_color"))

  axis_x_cex <- ifelse(is.null(getOption("axis_x_cex")), 
    getOption("axis_cex"), getOption("axis_x_cex"))
  adj <- .RSadj(axis_cex=axis_x_cex); axis_x_cex <- adj$axis_cex

  axis_y_cex <- ifelse(is.null(getOption("axis_y_cex")), 
    getOption("axis_cex"), getOption("axis_y_cex"))
  adj <- .RSadj(axis_cex=axis_y_cex); axis_y_cex <- adj$axis_cex

  lab_x_color <- ifelse(is.null(getOption("lab_x_color")), 
    getOption("lab_color"), getOption("lab_x_color"))
  lab_y_color <- ifelse(is.null(getOption("lab_y_color")), 
    getOption("lab_color"), getOption("lab_y_color"))


  # if applicable, open graphics window of specified dimensions
  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)
  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (!in.RStudio && !in.knitr) dev.new(width=width, height=height)

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) {
    was.null <- TRUE
    ylab <- ifelse (!prop, "Count of", "Proportion of")
  }
  else
    was.null <- FALSE

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse(is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
  lab_y_cex <- ifelse(is.null(lab_y_cex), lab_cex, lab_y_cex)
  adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex
  gl <- .getlabels(xlab, ylab, main, lab_x_cex=lab_x_cex, 
                   lab_y_cex=lab_y_cex)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl
  y.lab <- ifelse (was.null, paste(gl$yb, x.name), gl$yb)
  main.lab <- gl$mb
  sub.lab <- gl$sb
  lab_x_cex <- gl$lab_x_cex
  lab_y_cex <- gl$lab_y_cex

  # lattice does horizontal bar or dot chart, so reverse axis labels
  if (c.type %in% c("bar", "dot")) {
    tmp.lab <- x.lab
    x.lab <- y.lab
    y.lab <- tmp.lab
    tmp.cex <- lab_x_cex
    lab_x_cex <- lab_y_cex
    lab_y_cex <- tmp.cex
    if (is.null(trans)) trans <- getOption("trans_pt_fill")
  }

  # move strip to left for a single column
  strp <- TRUE;  strp.lft <- FALSE
  if (!is.null(ncols)) if (ncols == 1) {
    strp <- FALSE;  strp.lft <- TRUE
  }

  # ------------------------------------------------
  # calculate the histogram or bar chart or dot plot
  # set conditioning variables, by1 and by2

  h.type <- ifelse(prop, "percent", "count")  # histogram type
  #h.type <- "density"   # need to integrate density as a 3rd option
  if (c.type == "hist") {
    if (is.null(by2))
      p <- lattice::histogram(~ x | by1, type=h.type)
    else
      p <- lattice::histogram(~ x | by1 * by2, type=h.type)
    # see if there is a color range for fill, if so then get the colors
    # if breaks were possible, use unlist(p)$panel.args.common.breaks
    n.bar <- unlist(p)$panel.args.common.nint
  }

  else if (c.type %in% c("bar", "dot")) {
    mytab <- table(x, by1)
    if (prop) mytab <- prop.table(mytab, margin=2)
    mytabDF <- as.data.frame.table(mytab, responseName="Count")
    p <- lattice::barchart(x ~ Count | by1, data=mytabDF)
    n.bar <- nrow(mytab)
  }

  # process a potential color range (such as "blues")
  clr <- NULL
  if (is.null(fill))  fill <- getOption("bar_fill_discrete")
  clr <- .color_range(fill, n.bar)  # if range, return colors
  fill <- clr  # NOTE:  somewhere  fill  is still being used instead of clr


  # customize layout cols and rows, need only specify one
  if (!is.null(nrows) ||  !is.null(ncols)) {
    n.panels <- ifelse (is.null(by2), nlevels(by1), nlevels(by1)*nlevels(by2))
    if (is.null(ncols)) ncols <- (n.panels %/% nrows) + (n.panels %% nrows > 0)
    if (is.null(nrows)) nrows <- (n.panels %/% ncols) + (n.panels %% ncols > 0)
    p <- update(p, layout=c(ncols, nrows))
  } 

  # scale down the point size, grid line width for the multi-panel dot plots
  n.pnl <-  length(levels(by1))
  if (!is.null(by2)) n.pnl <- n.pnl + length(levels(by2))
  size.mult <- ifelse (n.pnl > 3, 0.70, 0.833)
  size.pt <- size.pt * size.mult
  if (n.pnl > 3 &&  grid_y_lwd > 0.99) grid_y_lwd <- .5 * grid_y_lwd

  # separate panels with a border even if turned off when only one plot
  panel_frame_color <- ifelse(panel_color == "transparent",
                              "gray50", panel_color)

  # even if no axis in single plot, multi-panel needs an axis to separate
  # scales, as currently configured, does not separate values from the axis
  g.x_color <- grid_x_color
  if (g.x_color ==  "transparent") g.x_color <- grid_y_color
  g.y_color <- grid_y_color
  if (g.y_color ==  "transparent") g.y_color <- grid_x_color

  a.x.text_color <- axis_x_text_color
  if (a.x.text_color ==  "transparent") a.x.text_color <-axis_y_text_color
  a.y.text_color <- axis_y_text_color
  if (a.y.text_color ==  "transparent") a.y.text_color <- axis_x_text_color

  l.x_color <- lab_x_color
  if (l.x_color == "transparent") l.x_color <-lab_y_color
  l.y_color <- lab_y_color
  if (l.y_color == "transparent") l.y_color <- lab_x_color

  # more physical space of the axis from the axis labels unless too many rows
  if (is.null(nrows)) nrows <- 1
  if (nrows < 7) { 
    pad <- 2.08 - 0.56*log(nrows)
    p <- update(p,
         par.settings=list(
           layout_heights=list(axis_xlab_padding=pad)))
  }

  top.pad <- ifelse (is.null(main), 0, 1)
  if (!is.null(by1)) top.pad <- 1
  axs.top <- ifelse (is.null(main), .5, 1)


  # specify plot attributes
  p <- update(p,
         strip=strp, strip.left=strp.lft, aspect=asp,
         par.strip_text=list(cex=axis_x_cex, col=getOption("strip_text_color")),
         xlab=list(label=x.lab, cex=lab_x_cex, col=l.x_color),
         ylab=list(label=y.lab, cex=lab_y_cex, col=l.y_color),
         main=list(label=main.lab, col=getOption("lab_color")), 
         par.settings=list(
           background=list(col=getOption("window_fill")),
           panel.background=list(col=panel_fill),
           layout_heights=list(top.padding=top.pad, axis_top=axs.top),
           axis_line=list(col=panel_frame_color,
             lty=getOption("axis_lty"), lwd=getOption("axis_lwd")), 
           strip.border=list(col=getOption("strip_color"), lwd=0.5),
           strip.background=list(col=getOption("strip_fill"))),
         scales=list(
           x = list(cex=axis_x_cex, rot=rotate_x,
                    col=a.x.text_color),
           y = list(cex=axis_y_cex, rot=getOption("rotate_y"),
                    col=a.y.text_color)),
         panel = function(x, y, ...) {
            panel.grid(h=0, v=-1, col=g.x_color,
                        lwd=grid_x_lwd, lty=grid_x_lty)
            if (c.type == "hist") {
              panel.grid(h=-1, v=0, g.y_color,
                         lwd=grid_y_lwd, lty=grid_y_lty)
              panel.histogram(x, col=fill, border=color, ...)
              #panel.dnFill(x, fill=rgb(.3,.3,.9,.2), color="darkblue",
                           #ref=TRUE, origin=0, ...)
              #panel.mathdensity(dmath = dnorm, col.line = "grey60",
                           #args = list(mean=mean(x),sd=sd(x)), ...)
            }
            if (c.type == "dot") {
              if (segments_x) {
                panel.points(x, y, pch=21, cex=size.pt,
                   col=getOption("pt_fill"), fill=getOption("pt_fill"), ...)
                panel.segments(0, y, x, y, col=getOption("pt_fill"), ...)
              }
              else {
                panel.dotplot(x, y, type="p", col=color, fill=fill,
                   col.line=fill, ...)  # called from Plot 
              }
            }
            else if (c.type == "bar")
              panel.barchart(x, y, col=fill, border=color, ...)
          }
        )
        

  # display
# if (is.null(pdf_file)) pdf_file <- FALSE
  if (!is.null(pdf_file)) {
    cat("\n>>> pdf_file does not work for Trellis graphics, manually save\n\n")
#   pdf(pdf_file, width=width, height=height)
#   print(p)
#   dev.off()
# }
# else {
  }
  print(p)

  # text output
  if (!quiet) {

    if (c.type == "hist"  &&  is.null(by2)) {
      stuff <- .ss.numeric(x, by1, digits_d=getOption("digits_d"), 
                           brief=TRUE, y.name=getOption("by1name"))
      txsts <- stuff$tx
      class(txsts) <- "out"
      output <- list(out_stats=txsts)
      class(output) <- "out_all"
      print(output)
    }

    else if (c.type == "bar"  &&  is.null(by2)) {
        stats <- .ss.factor(x, by=by1, brief=TRUE, digits_d=getOption("digits_d"),
                            x.name=x.name, y.name=getOption("by1name"),
                            x.lbl=x.lbl, y.lbl=y.lbl)
        if (!is.null(stats)) {
          txttl <- stats$txttl
          txfrq <- stats$txfrq
          txXV <- stats$txXV

          class(txttl) <- "out"
          class(txXV) <- "out"
          output <- list(out_frq=txfrq, out_chi=txXV)
          class(output) <- "out_all"
          print(output)      
      }

    }
  }

  cat("\n")

}

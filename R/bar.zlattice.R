.bar.lattice <- 
function(x, by1, by2, nrows, ncols, asp, prop,
         fill, color, panel.fill, panel.color,
         trans, size.pt, xlab, ylab, main,
         lab.cex, axis.cex, rotate.x, rotate.y,
         width, height, pdf.file,
         segments.x, breaks, c.type) {


  cat("[Trellis graphics from Deepayan Sarkar's lattice package]\n\n")


  grid.x.color <- ifelse(is.null(getOption("grid.x.color")), 
    getOption("grid.color"), getOption("grid.x.color"))
  grid.y.color <- ifelse(is.null(getOption("grid.y.color")), 
    getOption("grid.color"), getOption("grid.y.color"))
 
  grid.x.lwd <- ifelse(is.null(getOption("grid.x.lwd")), 
    getOption("grid.lwd"), getOption("grid.x.lwd"))
  grid.y.lwd <- ifelse(is.null(getOption("grid.y.lwd")), 
    getOption("grid.lwd"), getOption("grid.y.lwd"))

  grid.x.lty <- ifelse(is.null(getOption("grid.x.lty")), 
    getOption("grid.lty"), getOption("grid.x.lty"))
  grid.y.lty <- ifelse(is.null(getOption("grid.y.lty")), 
    getOption("grid.lty"), getOption("grid.y.lty"))

  axis.x.color <- ifelse(is.null(getOption("axis.x.color")), 
    getOption("axis.color"), getOption("axis.x.color"))
  axis.y.color <- ifelse(is.null(getOption("axis.y.color")), 
    getOption("axis.color"), getOption("axis.y.color"))
  # axis color is panel.color unless axis.color is changed from default
  theme <- getOption("theme")
  sub.theme <- getOption("sub.theme")
  if (sub.theme != "black")  {  
     panel.color <- ifelse (axis.x.color != "gray15", axis.x.color, "#DED9CD")
     if (panel.color == "#DED9CD")
       panel.color <- ifelse (axis.y.color != "gray15", axis.y.color, "#DED9CD")
  }
  else {
     panel.color <- ifelse (axis.x.color != "gray55", axis.x.color, "gray55")
     if (panel.color == "gray55")
       panel.color <- ifelse (axis.y.color != "gray55", axis.y.color, "gray55")
  }

  axis.x.text.color <- ifelse(is.null(getOption("axis.x.text.color")), 
    getOption("axis.text.color"), getOption("axis.x.text.color"))
  axis.y.text.color <- ifelse(is.null(getOption("axis.y.text.color")), 
    getOption("axis.text.color"), getOption("axis.y.text.color"))

  axis.x.cex <- ifelse(is.null(getOption("axis.x.cex")), 
    getOption("axis.cex"), getOption("axis.x.cex"))
  adj <- .RSadj(axis.cex=axis.x.cex); axis.x.cex <- adj$axis.cex

  axis.y.cex <- ifelse(is.null(getOption("axis.y.cex")), 
    getOption("axis.cex"), getOption("axis.y.cex"))
  adj <- .RSadj(axis.cex=axis.y.cex); axis.y.cex <- adj$axis.cex

  lab.x.color <- ifelse(is.null(getOption("lab.x.color")), 
    getOption("lab.color"), getOption("lab.x.color"))
  lab.y.color <- ifelse(is.null(getOption("lab.y.color")), 
    getOption("lab.color"), getOption("lab.y.color"))


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

  # get lab.x.cex  lab.y.cex
  lab.cex <- getOption("lab.cex")
  lab.x.cex <- getOption("lab.x.cex")
  lab.y.cex <- getOption("lab.y.cex")
  lab.x.cex <- ifelse(is.null(lab.x.cex), lab.cex, lab.x.cex)
  adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  lab.y.cex <- ifelse(is.null(lab.y.cex), lab.cex, lab.y.cex)
  adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex
  gl <- .getlabels(xlab, ylab, main, lab.x.cex=lab.x.cex, 
                     lab.y.cex=lab.y.cex)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl
  y.lab <- ifelse (was.null, paste(gl$yb, x.name), gl$yb)
  main.lab <- gl$mb
  sub.lab <- gl$sb
  lab.x.cex <- gl$lab.x.cex
  lab.y.cex <- gl$lab.y.cex

  # lattice does horizontal bar or dot chart, so reverse axis labels
  if (c.type %in% c("bar", "dot")) {
    tmp.lab <- x.lab
    x.lab <- y.lab
    y.lab <- tmp.lab
    tmp.cex <- lab.x.cex
    lab.x.cex <- lab.y.cex
    lab.y.cex <- tmp.cex
    if (is.null(trans)) trans <- getOption("trans.pt.fill")
  }

  # move strip to left for a single column
  strp <- TRUE;  strp.lft <- FALSE
  if (!is.null(ncols)) if (ncols == 1) {
    strp <- FALSE;  strp.lft <- TRUE
  }

  # ---------------------------------------
  # set conditioning variables, by1 and by2

  h.type <- ifelse(prop, "percent", "count")
  #h.type <- "density"   # need to integrate density as a 3rd option
  if (c.type == "hist") {
    if (is.null(by2))
      p <- lattice::histogram(~ x | by1, type=h.type)
    else
      p <- lattice::histogram(~ x | by1 * by2, type=h.type)
  }

  else if (c.type %in% c("bar", "dot")) {
    mytab <- table(x, by1)
    mytabDF <- as.data.frame.table(mytab, responseName="Count")
    p <- lattice::barchart(x ~ Count | by1, data=mytabDF)
  }

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
  if (n.pnl > 3 &&  grid.y.lwd > 0.99) grid.y.lwd <- .5 * grid.y.lwd

  # separate panels with a border even if turned off when only one plot
  panel_frame.color <- ifelse(panel.color == "transparent",
                              "gray50", panel.color)

  # even if no axis in single plot, multi-panel needs an axis to separate
  # scales, as currently configured, does not separate values from the axis
  g.x.color <- grid.x.color
  if (g.x.color ==  "transparent") g.x.color <- grid.y.color
  g.y.color <- grid.y.color
  if (g.y.color ==  "transparent") g.y.color <- grid.x.color

  a.x.text.color <- axis.x.text.color
  if (a.x.text.color ==  "transparent") a.x.text.color <-axis.y.text.color
  a.y.text.color <- axis.y.text.color
  if (a.y.text.color ==  "transparent") a.y.text.color <- axis.x.text.color

  l.x.color <- lab.x.color
  if (l.x.color == "transparent") l.x.color <-lab.y.color
  l.y.color <- lab.y.color
  if (l.y.color == "transparent") l.y.color <- lab.x.color

  # more physical space of the axis from the axis labels unless too many rows
  if (is.null(nrows)) nrows <- 1
  if (nrows < 7) { 
    pad <- 2.08 - 0.56*log(nrows)
    p <- update(p,
         par.settings=list(
           layout.heights=list(axis.xlab.padding=pad)))
  }

  top.pad <- ifelse (is.null(main), 0, 1)
  if (!is.null(by1)) top.pad <- 1
  axs.top <- ifelse (is.null(main), .5, 1)


  # specify plot attributes
  p <- update(p,
         strip=strp, strip.left=strp.lft, aspect=asp,
         par.strip.text=list(cex=axis.x.cex, col=getOption("strip.text.color")),
         xlab=list(label=x.lab, cex=lab.x.cex, col=l.x.color),
         ylab=list(label=y.lab, cex=lab.y.cex, col=l.y.color),
         main=list(label=main.lab, col=getOption("lab.color")), 
         par.settings=list(
           background=list(col=getOption("window.fill")),
           panel.background=list(col=panel.fill),
           layout.heights=list(top.padding=top.pad, axis.top=axs.top),
           axis.line=list(col=panel_frame.color,
             lty=getOption("axis.lty"), lwd=getOption("axis.lwd")), 
           strip.border=list(col=getOption("strip.color"), lwd=0.5),
           strip.background=list(col=getOption("strip.fill"))),
         scales=list(
           x = list(cex=axis.x.cex, rot=rotate.x, col=a.x.text.color),
           y = list(cex=axis.y.cex, rot=rotate.y, col=a.y.text.color)),
         panel = function(x, y, ...) {
             panel.grid(h=0, v=-1, col=g.x.color,
                        lwd=grid.x.lwd, lty=grid.x.lty)
            if (c.type == "hist") {
             panel.grid(h=-1, v=0, g.y.color,
                        lwd=grid.y.lwd, lty=grid.y.lty)
              panel.histogram(x, col=fill, border=color, ...)
              #panel.dnFill(x, fill=rgb(.3,.3,.9,.2), color="darkblue",
                           #ref=TRUE, origin=0, ...)
              #panel.mathdensity(dmath = dnorm, col.line = "grey60",
                           #args = list(mean=mean(x),sd=sd(x)), ...)
            }
            if (c.type == "dot") {
              if (segments.x) {
                panel.points(x, y, pch=21, cex=size.pt,
                   col=getOption("pt.fill"), fill=getOption("pt.fill"), ...)
                panel.segments(0, y, x, y, col=getOption("pt.fill"), ...)
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
  if (is.null(pdf.file)) pdf.file <- FALSE  # from BarChart
  if (pdf.file) {
    pdf(pdf.file, width=width, height=height)
    print(p)
    dev.off()
  }
  else {
    print(p)
  }

}



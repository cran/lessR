.bar.lattice <- 
function(x, by1, by2, nrows, ncols, asp, prop,
         fill, stroke, bg.fill, bg.stroke, trans, size.pt, xlab, ylab, main,
         cex.lab, cex.axis, rotate.x, rotate.y, width, height, pdf.file,
         segments.x, breaks, c.type) {


  cat("[Trellis graphics with Deepayan Sarkar's lattice package]\n\n")

  # if applicable, open graphics window of specified dimensions
  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)
  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (!in.RStudio && !in.knitr) dev.new(width=width, height=height)

  # scale for regular R or RStudio
  adj <- .RSadj(radius=NULL, cex.axis)
  size.axis <- adj$size.axis
  size.lab <- adj$size.lab

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) {
    was.null <- TRUE
    ylab <- ifelse (!prop, "Count of", "Percent of")
  }
  else
    was.null <- FALSE
  gl <- .getlabels(xlab, ylab, main, sub=NULL, cex.lab=size.lab)
  x.name <- gl$xn;  x.lbl <- gl$xl;  x.lab <- gl$xb
  y.lab <- ifelse (was.null, paste(gl$yb, x.name), gl$yb)
  main.lab <- gl$mb
  sub.lab <- gl$sb
  cex.lab <- gl$cex.lab

  # lattice does horizontal bar or dot chart, so reverse axis labels
  if (c.type %in% c("bar", "dot")) {
    tmp.lab <- x.lab
    x.lab <- y.lab
    y.lab <- tmp.lab
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
  g.w <- getOption("grid.lwd")
  if (n.pnl > 3 &&  g.w > 0.99) g.w <- .5 * g.w

  # separate panels with a border even if turned off for a single plot
  panel_frame.stroke <- ifelse(bg.stroke == "transparent", "gray50", bg.stroke)

  # even if no axis in single plot, multi-panel needs an axis to separate
  # scales, as currently configured, does not separate values from the axis
  a.x.stk <- getOption("axis.x.stroke")
  if (a.x.stk ==  "transparent") a.x.stk <- getOption("axis.y.stroke")
  a.y.stk <- getOption("axis.y.stroke")
  if (a.y.stk ==  "transparent") a.y.stk <- getOption("axis.x.stroke")

  s.x.stk <- getOption("bg.stroke")
  if (s.x.stk ==  "transparent") s.x.stk <- getOption("lab.stroke")

  g.x.stk <- getOption("grid.x.stroke")
  if (g.x.stk ==  "transparent") g.x.stk <- getOption("grid.y.stroke")
  g.y.stk <- getOption("grid.y.stroke")
  if (g.y.stk ==  "transparent") g.y.stk <- getOption("grid.x.stroke")


  # specify plot attributes
  p <- update(p,
         strip=strp, strip.left=strp.lft, aspect=asp,
         par.strip.text=list(col=getOption("lab.stroke")),
         xlab=list(label=x.lab, cex=cex.lab, col=getOption("lab.stroke")),
         ylab=list(label=y.lab, cex=cex.lab, col=getOption("lab.stroke")),
         main=list(label=main.lab, col=getOption("lab.stroke")), 
         par.settings=list(
           background=list(col=getOption("device.fill")),
           panel.background=list(col=bg.fill),
           axis.line=list(col=panel_frame.stroke), 
           strip.border=list(col=s.x.stk, lwd=0.5),
           strip.background=list(col=getOption("ellipse.fill"))),
         scales=list(
           x = list(cex=cex.axis, rot=rotate.x, col=a.x.stk),
           y = list(cex=cex.axis, rot=rotate.y, col=a.y.stk)),
         panel = function(x, y, ...) {
            panel.grid(h=0, v=-1, col=g.x.stk,
                       lwd=g.w, lty=getOption("grid.lty"))
            if (c.type == "hist") {
              panel.grid(h=-1, v=0, col=g.y.stk,
                         lwd=g.w, lty=getOption("grid.lty"))
              panel.histogram(x, col=fill, border=stroke, ...)
            }
            if (c.type == "dot") {
              if (segments.x) {
                panel.points(x, y, pch=21, cex=size.pt,
                   col=getOption("pt.fill"), fill=getOption("pt.fill"), ...)
                panel.segments(0, y, x, y, col=getOption("pt.fill"), ...)
              }
              else {
                panel.dotplot(x, y, type="p", col=stroke, fill=fill,
                   col.line=fill, ...)  # called from Plot 
              }
            }
            else if (c.type == "bar")
              panel.barchart(x, y, col=fill, border=stroke, ...)
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



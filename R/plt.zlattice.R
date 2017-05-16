.plt.lattice <- 
function(x, y, by1, by2, by, object, nrows, ncols, asp,
         fill, stroke, bg.fill, bg.stroke, trans, size.pt, size.ln,
         xlab, ylab, main, shape, cex.lab, cex.axis,
         lvl, ellipse.stroke, ellipse.lwd, fit, fit.stroke, fit.lwd, area, origin,
         rotate.x, rotate.y, width, height, pdf.file, ...) {


  cat("[Trellis graphics with Deepayan Sarkar's lattice package]\n\n")

  # if applicable, open graphics window of specified dimensions
  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)
  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (!in.RStudio && !in.knitr) dev.new(width=width, height=height)

  if (size.pt == 0) object <- "line"

  size.lab <- cex.axis 
  gl <- .getlabels(xlab, ylab, main, cex.lab=size.lab)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  size.lab <- gl$cex.lab

  fill <- ifelse (is.null(trans), fill, .maketrans(fill, (1-trans)*256)) 
  col.bg <- ifelse(sum(col2rgb(bg.fill)) < 370, "transparent", bg.fill)
  if (is.null(trans)) trans <- getOption("trans.pt.fill")

  n.groups <- ifelse (is.null(by), 0, nlevels(by))

  col.stroke <- character(length=length(n.groups))
  col.fill <- character(length=length(n.groups))
  ltype <- character(length=n.groups)

  # set colors
  if (n.groups <= 1) {
    col.stroke <- stroke[1]
    col.fill <- fill[1]
  }
  else if (n.groups == 2) {
      col.stroke[1] <- stroke[1]
      col.stroke[2] <- stroke[1]
      col.fill[1] <- fill[1]
      col.fill[2] <- "transparent"
      ltype[1] <- "solid"
      if (object == "both") ltype[2] <- "dotted"
  }  
  else  {  # n.groups > 2
    col.stroke <- .col.discrete(bright=TRUE)[1:n.groups]

    for (i in 1:n.groups) {
      col.fill[i] <- .maketrans(col.stroke[i], (1-trans)*256)
      ltype[i] <- "solid"
    }
  }

  if (area != "transparent")
    if (col.fill[1] != area) col.fill[1] <- .maketrans(area, (1-trans)*256)


  legend.title <- abbreviate(getOption("byname"), 7)
  leg.cex.title <- 0.85
  if (!is.null(by)) by <- abbreviate(by, 5)

  # move strip to left for a single column
  strp <- TRUE;  strp.lft <- FALSE
  if (!is.null(ncols)) if (ncols == 1) {
    strp <- FALSE;  strp.lft <- TRUE
  }


  # ---------------------------------
  # set 1 or 2 conditioning variables
  if (is.null(by2)) {
    p <- lattice::xyplot(y ~ x | by1, groups=by, ...)
  }
  else {  # by2 is present
    p <- lattice::xyplot(y ~ x | by1 * by2, groups=by, ...)
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

  # separate panels with a border even if off for a single plot
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
           strip.background=list(col=getOption("ellipse.fill")),
           plot.line=list(col=stroke, lty="solid", lwd=1),
           plot.symbol=list(pch=shape, cex=size.pt, col=col.stroke, fill=col.fill),
           superpose.symbol=list(pch=shape, cex=size.pt,
             col=col.stroke, fill=col.fill),
           superpose.line=list(col=col.stroke, lty=ltype)),
         scales=list(
           x = list(cex=cex.axis, rot=rotate.x, col=a.x.stk),
           y = list(cex=cex.axis, rot=rotate.y, col=a.y.stk)),
         auto.key=list(
           cex=0.85,
           space="right", rows=length(unique(by)),
           border=TRUE, background=col.bg,
           title=legend.title, cex.title=leg.cex.title),
         panel = function(x, y, ...) {
             panel.grid(h=0, v=-1, col=getOption("grid.x.stroke"),
                        lwd=g.w, lty=getOption("grid.lty"))
             panel.grid(h=-1, v=0, col=getOption("grid.y.stroke"),
                        lwd=g.w, lty=getOption("grid.lty"))
             if (area != "transparent")
               panel.xyarea(x, y, origin=origin, col=col.fill)
             if (object == "point")
               tp <- "p" 
             else if (object == "both")
               tp <- "b"
             else if (object == "line")
               tp <- "l"
             panel.xyplot(x, y, type=tp, col=col.stroke, fill=col.fill,
                          lwd=size.ln, ...)
          }
        )

  # fit line
  if (fit == "loess")
    p <- p + latticeExtra::layer(panel.loess(x, y, col=P1, lwd=P2),
                   data=list(P1=fit.stroke, P2=fit.lwd))
  else if (fit == "ls")
    p <- p + latticeExtra::layer(panel.lmline(x, y, col=P1, lwd=P2),
                   data=list(P1=fit.stroke, P2=fit.lwd))

  # ellipse
  if (lvl > 0)
    p <- p + latticeExtra::layer(panel.ellipse(x, y, center.cex=0,
                     level=P2, col=P1, lwd=P3),
                   data=list(P1=ellipse.stroke, P2=lvl, P3=ellipse.lwd))

  # display
  if (!is.null(pdf.file)) {
    pdf(pdf.file, width=width, height=height)
    print(p)
    dev.off()
  }
  else {
    print(p)
  }

}

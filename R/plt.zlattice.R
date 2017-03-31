.plt.lattice <- 
function(x, y, by, by2, byg, object, nrows, ncols, asp,
         fill, stroke, bg, col.grid, trans, size.pt, size.ln,
         xlab, ylab, main, shape, cex.axis, axes,
         lvl, stroke.ellipse, lwd.ellipse, fit, stroke.fit, lwd.fit, area, origin,
         rotate.x, rotate.y, width, height, pdf.file) {


  txt <- "[Trellis graphics with Deepayan Sarkar's lattice package]\n\n"

  # open graphics window of specified dimensions
  if (options("device") != "RStudioGD")
    dev.new(width=width, height=height)

  if (size.pt == 0) object <- "line"

  size.lab <- cex.axis 
  gl <- .getlabels(xlab, ylab, main, cex.lab=size.lab)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  size.lab <- gl$cex.lab
  by.name <- getOption("byname")

  fill <- ifelse (is.null(trans), fill, .maketrans(fill, (1-trans)*256)) 
  col.bg <- ifelse(sum(col2rgb(bg)) < 370, "transparent", bg)
  if (is.null(trans)) trans <- getOption("trans.fill.pt")

  n.groups <- ifelse (is.null(byg), 0, nlevels(byg))

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


  # set Trellis parameters
  t.sym <- trellis.par.get("plot.symbol")
    t.sym$pch <- shape
    t.sym$col <- stroke
    t.sym$fill <- col.fill 
  trellis.par.set("plot.symbol", t.sym)

  t.lne <- trellis.par.get("plot.line")
    t.lne$col <- stroke
    t.lne$lty <- "solid"
    t.lne$lwd <- 1 
  trellis.par.set("plot.line", t.lne)

  t.bg <- trellis.par.get("panel.background")
    t.bg$col <- bg
  trellis.par.set("panel.background", t.bg)

  t.sbg <- trellis.par.get("strip.background")
    t.sbg$col <- getOption("fill.ellipse")
  trellis.par.set("strip.background", t.sbg)

  t.sup <- trellis.par.get("superpose.symbol")
    t.sup$pch <- shape
    t.sup$col <- col.stroke
    t.sup$fill <- col.fill
  trellis.par.set("superpose.symbol", t.sup)

  t.sln <- trellis.par.get("superpose.line")
    t.sln$col <- col.stroke
    t.sln$lty <- ltype
  trellis.par.set("superpose.line", t.sln)


  legend.title <- abbreviate(getOption("byname"), 7)
  leg.cex.title <- 0.85
  if (!is.null(byg)) byg <- abbreviate(byg, 5)

  # move strip to left for a single column
  strp <- TRUE;  strp.lft <- FALSE
  if (!is.null(ncols)) if (ncols == 1) {
    strp <- FALSE;  strp.lft <- TRUE
  }


  # ---------------------------------
  # set 1 or 2 conditioning variables
  if (is.null(by2)) {
    p <- xyplot(y ~ x | by, groups=byg)
  }
  else {  # by2 is present
    p <- xyplot(y ~ x | by * by2, groups=byg)
  }


  # customize layout cols and rows, need only specify one
  if (!is.null(nrows) ||  !is.null(ncols)) {
    n.panels <- ifelse (is.null(by2), nlevels(by), nlevels(by)*nlevels(by2))
    if (is.null(ncols)) ncols <- (n.panels %/% nrows) + (n.panels %% nrows > 0)
    if (is.null(nrows)) nrows <- (n.panels %/% ncols) + (n.panels %% ncols > 0)
    p <- update(p, layout=c(ncols, nrows))
  } 

  # specify plot attributes
  p <- update(p,
         strip=strp, strip.left=strp.lft,
         xlab=x.lab, ylab=y.lab, aspect=asp,
         auto.key=list(
           cex=0.85,
           space="right", rows=length(unique(byg)),
           border=TRUE, background=col.bg,
           title=legend.title, cex.title=leg.cex.title),
         scales=list(
           x = list(cex=cex.axis, rot=rotate.x, col=axes),
           y = list(cex=cex.axis, rot=rotate.y)),
         panel = function(x, y, ...) {
             panel.grid(h=-1, v=-1, col.line=col.grid, lwd=.5)
             if (area != "transparent")
               panel.xyarea(x, y, origin=origin, col=col.fill)
             if (object == "point")
               tp <- "p" 
             else if (object == "both")
               tp <- "b"
             else if (object == "line")
               tp <- "l"
             panel.xyplot(x, y, type=tp, lwd=size.ln, ...)
          }
        )

  # fit line
  if (fit == "loess")
    p <- p + layer(panel.loess(x, y, col=P1, lwd=P2),
                   data=list(P1=stroke.fit, P2=lwd.fit))
  else if (fit == "ls")
    p <- p + layer(panel.lmline(x, y, col=P1, lwd=P2),
                   data=list(P1=stroke.fit, P2=lwd.fit))

  # ellipse
  if (lvl > 0)
    p <- p + layer(panel.ellipse(x, y, center.cex=0, level=P2, col=P1, lwd=P3),
                   data=list(P1=stroke.ellipse, P2=lvl, P3=lwd.ellipse))

  # display
  if (!is.null(pdf.file)) {
    pdf(pdf.file, width=width, height=height)
    trellis.par.set("plot.symbol", t.sym)
    trellis.par.set("plot.line", t.lne)
    trellis.par.set("strip.background", t.sbg)
    trellis.par.set("panel.background", t.bg)
    trellis.par.set("superpose.symbol", t.sup)
    trellis.par.set("superpose.line", t.sln)
    print(p)
    dev.off()
  }
  else {
    print(p)
  }

}

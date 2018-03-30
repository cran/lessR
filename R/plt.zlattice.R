.plt.lattice <- 
function(x, y, by1, by2, by, adj.bx.ht, object, nrows, ncols, asp,
         fill, color, panel.fill, panel.color,
         trans, size.pt, size.ln,
         xlab, ylab, main, shape, lab.cex, axis.cex,
         lvl=0, ellipse.color=NULL, ellipse.lwd=NULL,
         fit="off", fit.color=NULL, fit.lwd=NULL,
         area="transparent", origin=NULL, jitter, bw, violin,
         box, vbs.size, box.adj, a, b, k.iqr, fences, vbs.mean,
         out.shape, out.size,
         out.fill, out.color, out2.fill, out2.color,
         ID, out.cut, ID.color, ID.size,
         rotate.x, rotate.y, width, height, pdf.file, c.type, ...) {


  if (size.pt == 0) object <- "line"

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

  # get lab.x.cex  lab.y.cex
  lab.cex <- getOption("lab.cex")
  lab.x.cex <- getOption("lab.x.cex")
  lab.y.cex <- getOption("lab.y.cex")
  lab.x.cex <- ifelse (is.null(lab.x.cex), lab.cex, lab.x.cex)
  adj <- .RSadj(lab.cex=lab.x.cex); lab.x.cex <- adj$lab.cex
  lab.y.cex <- ifelse (is.null(lab.y.cex), lab.cex, lab.y.cex)
  adj <- .RSadj(lab.cex=lab.y.cex); lab.y.cex <- adj$lab.cex

  is.y <- ifelse (!is.null(y), TRUE, FALSE)
  gl <- .getlabels(xlab, ylab, main, y.nm=is.y, lab.x.cex=lab.x.cex, 
                     lab.y.cex=lab.y.cex, by1.nm=TRUE)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  lab.x.cex <- gl$lab.x.cex
  lab.y.cex <- gl$lab.y.cex

  if (trans > 0) fill <- .maketrans(fill, (1-trans)*256)
  col.bg <- ifelse(sum(col2rgb(panel.fill)) < 370, "transparent", panel.fill)

  n.groups <- ifelse (is.null(by), 1, nlevels(by))

  col.color <- character(length=length(n.groups))
  col.fill <- character(length=length(n.groups))
  ltype <- character(length=n.groups)

  # set colors
  if (n.groups == 1) {
    col.fill <- fill[1]
    col.color <- color[1]
  }
  else if (n.groups == 2) {
      col.fill[1] <- fill[1]
      col.fill[2] <- "gray70"
      col.color[1] <- color[1]
      col.color[2] <- "black"
      ltype[1] <- "solid"
      if (object == "both") ltype[2] <- "dotted"
  }  
  else  {  # n.groups > 2
    col.color <- .col.discrete()[1:n.groups]
    for (i in 1:n.groups) {
      col.fill[i] <- col.color[i]
      col.fill[i] <- .maketrans(col.fill[i], (1-trans)*256)
      ltype[i] <- "solid"
    }
  }

  if (object != "point") if (area != "transparent")
    if (col.fill[1] != area) col.fill[1] <- .maketrans(area, (1-trans)*256)

  legend.title <- abbreviate(getOption("byname"), 7)
  leg.cex.title <- 0.85  # abbreviate only returns of type character
  if (!is.null(by)) by <- factor(abbreviate(by, 5), levels(by))  # retain order

  if (is.null(by1) && is.null(by2))
    n.panels <- 1
  else {
    n.panels <- ifelse (is.null(by2), nlevels(by1), nlevels(by1)*nlevels(by2))
    if (n.panels == 0) n.panels <- 1
    if (c.type == "cont") {
      if (is.null(ncols) && is.null(nrows)) {
        ncols <- ifelse (n.panels < 7, 1, 2) 
      }
    }
  }

  # customize layout cols and rows, only specify one
  # if ncols or nrows specified, compute the other
  if (n.panels > 1) {
    if (!is.null(nrows)  ||  !is.null(ncols)) {
      if (is.null(ncols)) ncols <- (n.panels %/% nrows) + (n.panels %% nrows > 0)
      if (is.null(nrows)) nrows <- (n.panels %/% ncols) + (n.panels %% ncols > 0)
    }
  }

  # move strip to left for a single column
  strp <- TRUE;  strp.lft <- FALSE
  if (!is.null(ncols)) {
    if (ncols == 1) {
      strp <- FALSE;  strp.lft <- TRUE
    }
  }
  if (is.null(by1)) strp <- FALSE


  # ---------------------------------
  if (c.type == "contcont") {  # cont - cont
    # set 1 or 2 conditioning variables
    if (is.null(by2)) {
      p <- lattice::xyplot(y ~ x | by1, groups=by, ...)
    }
    else {  # by2 is present
      p <- lattice::xyplot(y ~ x | by1 * by2, groups=by, ...)
    }
  }

  else if (c.type == "contcat") {  # cont - cat
      jitter <- .4 * jitter
      if (is.null(by1)  &&  is.null(by2)) {
        p <- lattice::bwplot(y ~ x, groups=by, ...)
      }
      else if (is.null(by2)) {
        p <- lattice::bwplot(y ~ x | by1, groups=by, ...)
      }
      else {  # by2 is present
        p <- lattice::bwplot(y ~ x | by1 * by2, groups=by, ...)
      }
  }  # end contcat

  else if (c.type == "cont") {  # cont
    # set 0, 1 or 2 conditioning variables
    if (is.null(by1)  &&  is.null(by2)) {  # 0 cond var
      p <- lattice::stripplot(~ x, groups=by, subscripts=TRUE, ...)
      y.lab <- ""
    }
    else if (is.null(by2)) {  # 1 cond var
      p <- lattice::stripplot(~ x | by1, groups=by, subscripts=TRUE, ...)
      y.lab <- getOption("by1name")
    }
    else  {  # 2 cond var
      p <- lattice::stripplot(~ x | by1 * by2, subscripts=TRUE, ...)
      y.lab <- ""
    }
  }  # end cont

  p <- update(p, layout=c(ncols, nrows))


  # scale down the point size, grid line width for the multi-panel dot plots
  n.pnl <- length(levels(by1))
  if (!is.null(by2)) n.pnl <- n.pnl + length(levels(by2))
  if (n.pnl > 3  &&  grid.x.lwd > 0.99) grid.x.lwd <- .5 * grid.x.lwd
  if (n.pnl > 3  &&  grid.y.lwd > 0.99) grid.y.lwd <- .5 * grid.y.lwd

  size.mult <- ifelse (n.pnl > 3, 0.70, 0.833)
  size.pt <- size.pt * size.mult

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

  # separate the axis from the axis labels unless too many rows
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
           strip.background=list(col=getOption("strip.fill")),
           plot.polygon=list(col=getOption("violin.color"), 
             fill=getOption("violin.fill"), lty="solid", lwd=1),
           plot.line=list(col=color, lty="solid", lwd=1),
           plot.symbol=list(pch=shape, cex=size.pt, col=col.color,
             fill=col.fill),
           superpose.symbol=list(pch=shape, cex=size.pt,
             col=col.color, fill=col.fill),
           superpose.line=list(col=col.color, lty=ltype)),
         scales=list(
           x = list(cex=axis.x.cex, rot=rotate.x, col=a.x.text.color),
           y = list(cex=axis.y.cex, rot=rotate.y, col=a.y.text.color))
  )

  if (c.type == "contcont") {
    # specify plot attributes
    p <- update(p,
         auto.key=list(
           cex=0.85,
           space="right", rows=length(unique(by)),
           border=TRUE, background=col.bg,
           title=legend.title, cex.title=leg.cex.title),

         panel = function(x, y, ...) {
             panel.grid(h=0, v=-1, col=g.x.color,
                        lwd=grid.x.lwd, lty=grid.x.lty)
             panel.grid(h=-1, v=0, g.y.color,
                        lwd=grid.y.lwd, lty=grid.y.lty)
             if (area != "transparent")
               panel.xyarea(x, y, origin=origin, col=col.fill)
             if (object == "point")
               tp <- "p" 
             else if (object == "both")
               tp <- "b"
             else if (object == "line")
               tp <- "l"
             panel.xyplot(x, y, type=tp, col=col.color, fill=col.fill,
                          lwd=size.ln, ...)
          }
        )

    # fit line
    if (fit == "loess")
      p <- p + latticeExtra::layer(panel.loess(x, y, col=P1, lwd=P2),
                     data=list(P1=fit.color, P2=fit.lwd))
    else if (fit == "ls")
      p <- p + latticeExtra::layer(panel.lmline(x, y, col=P1, lwd=P2),
                     data=list(P1=fit.color, P2=fit.lwd))

    # ellipse
    if (lvl > 0)
      p <- p + latticeExtra::layer(panel.ellipse(x, y, center.cex=0,
                       level=P2, col=P1, lwd=P3),
                     data=list(P1=ellipse.color, P2=lvl, P3=ellipse.lwd))
  }  # end contcont 

  else if (c.type %in% c("contcat", "cont")) {

    #myboxStats <- function(...) 
      #if (!box.adj)
        #return(boxplot.stats(x))
      #else
        #return(adjboxStats(x, coef=k.iqr, a=a, b=b))

    if (fences) {  # make room for the fences with horizontal lengthening
      p <- update(p,
         prepanel=function(x=x) {

           num5 <- fivenum(x, na.rm=TRUE)
           q1 <- num5[2];  q3 <- num5[4];  iqr <- q3 - q1

           m.c <- ifelse (box.adj, mc(x, na.rm=TRUE), 0)
           if (m.c >= 0) {
             fnc.lwr <- q1 - (k.iqr * exp(a*m.c) * iqr)
             fnc.upr <- q3 + (k.iqr * exp(b*m.c) * iqr)
           }
           else {  # m.c < 0
             fnc.lwr <- q1 - (k.iqr * exp(-b*m.c) * iqr)
             fnc.upr <- q3 + (k.iqr * exp(-a*m.c) * iqr)
           } 

           min.x <- min(x, fnc.lwr) 
           max.x <- max(x, fnc.upr)

           list(xlim=c(min.x, max.x))
         }
       )
    }  # end fences


    if (n.groups > 1) {  # cex refers to the text, not the points
      c.color <- character(length=length(n.groups))
      if (n.groups == 2) {
        c.color[1] <- col.fill[1]  
        c.color[2] <- "black"
      }
      else
        c.color <- col.fill
    }
    else
      c.color <- col.fill

    if (c.type %in% c("cont", "contcat")) {
      if (n.groups > 1) {
        legend.lbl.cex <- ifelse (in.RStudio, .75, .66) 
        p <- update(p, key=list(space="top", columns=n.groups,
               text=list(levels(by), cex=legend.lbl.cex), 
               points=list(pch=21, fill=col.fill, col=c.color, cex=1),
               border="gray80", background=col.bg, padding.text=2))
      }
    }

    p <- update(p,

       par.settings=list(  # col option does not work directly on panel.bwplot
         box.rectangle=list(fill=getOption("box.fill"),
                            col=getOption("box.color")),
         box.umbrella=list(col=getOption("box.color"), lty="solid")
       ),

       panel=function(x=x, box.ratio, wID=ID, ...,
                      groups=groups, subscripts=subscripts) {

          jitter.data <- ifelse (jitter > 0, TRUE, FALSE)
          size.pt <- size.pt * 1.2  # lattice adjustment

          num5 <- fivenum(x, na.rm=TRUE)
          q1 <- num5[2]
          q3 <- num5[4]
          iqr <- q3 - q1
          fnc.in <- rep(NA_real_, 2)   # inner fences
          fnc.out <- rep(NA_real_, 2)  # outer fences

          m.c <- ifelse(box.adj, mc(x, na.rm=TRUE), 0)
          if (m.c >= 0) {
            fnc.in[1] <- q1 - (k.iqr * exp(a*m.c) * iqr)
            fnc.in[2] <- q3 + (k.iqr * exp(b*m.c) * iqr)
            fnc.out[1] <- q1 - (2 * k.iqr * exp(a*m.c) * iqr)
            fnc.out[2] <- q3 + (2 * k.iqr * exp(b*m.c) * iqr)
          }
          else {  # m.c < 0
            fnc.in[1] <- q1 - (k.iqr * exp(-b*m.c) * iqr)
            fnc.in[2] <- q3 + (k.iqr * exp(-a*m.c) * iqr)
            fnc.out[1] <- q1 - (2 * k.iqr * exp(-b*m.c) * iqr)
            fnc.out[2] <- q3 + (2 * k.iqr * exp(-a*m.c) * iqr)
          } 

          panel.grid(h=0, v=-1, col=g.x.color, lwd=grid.x.lwd, lty=grid.x.lty)

          if (violin) {
            # to get a violin plot, cannot have y and by1
            vw <- ifelse (!is.null(y) && !is.null(by1), FALSE, TRUE) 
            panel.violin(x=x, ...,
                col=getOption("violin.fill"), border=getOption("violin.color"),
                varwidth=vw, box.width=vbs.size, bw=bw)
           }

          if (box) {

            n.lvl <- ifelse (is.null(by1), 1, nlevels(by1))
            n <- adj.bx.ht
            int <- ifelse (n <= 25000, 4.10 - 0.000065*n, 3.25 - 0.00003*n)
            denom <- int - 0.5*n.lvl
            if (denom < 1.5) denom <- 1.5

            if (!box.adj)
              .panel.bwplot(x=x, ..., pch="|", vbs.mean=vbs.mean, fences=fences,
                  box.ratio=vbs.size/denom, mean.color=out.fill, 
                  stats=boxplot.stats, k.iqr=k.iqr, do.out=FALSE) 
            else
              .panel.bwplot(x=x, ...,  pch="|", vbs.mean=vbs.mean, fences=fences,
                  box.ratio=vbs.size/denom, mean.color=out.fill, 
                  stats=adjboxStats, k.iqr=k.iqr, a=a, b=b, do.out=FALSE) 
          }  # end box

          # plotting a subset of x requires adjusting y, in .panel.strip
          if (c.type == "cont"  ||  c.type == "contcat") {
            if (box) {  # only display outliers

              i.out <- which(x<fnc.out[1] | x>fnc.out[2])
              if (n.groups == 1) {
                i.out.clr <- 1
                fill.out <- out2.fill
              }
              else {
                i.out.clr <- as.numeric(groups[i.out])
                fill.out <- col.fill[i.out.clr]
              }
              # plot extreme outliers
              .panel.stripplot(x=x[i.out],
                cex=out.size, col=out2.color, fill=fill.out, pch=out.shape, ...)

              i.out <- which(x>=fnc.out[1] & x<fnc.in[1] |
                              x>fnc.in[2] & x<=fnc.out[2])
              if (n.groups == 1) {
                i.out.clr <- 1
                fill.out <- out.fill
              }
              else {
                i.out.clr <- as.numeric(groups[i.out])
                fill.out <- col.fill[i.out.clr]
              }
              # plot outliers
              .panel.stripplot(x= x[i.out],
                cex=out.size, col=out.color, fill=fill.out, pch=out.shape, ...)

              # label outliers
              if (out.cut > 0) {
                wwID <- wID[subscripts]

                ind.lo <- which(x < fnc.in[1])
                x.lo <- x[ind.lo]
                ID.lo <- wwID[ind.lo]
                ord <- order(x.lo, decreasing=FALSE)
                x.lo <- x.lo[ord]
                x.lo <- na.omit(x.lo[1:min(length(x.lo),out.cut)])
                ID.lo <- ID.lo[ord] 
                ID.lo <- na.omit(ID.lo[1:min(length(ID.lo),out.cut)]) 
                
                ind.hi <- which(x > fnc.in[2])
                x.hi <- x[ind.hi]
                ID.hi <- wwID[ind.hi]
                ord <- order(x.hi, decreasing=TRUE)
                x.hi <- x.hi[ord]
                x.hi <- na.omit(x.hi[1:min(length(x.hi),out.cut)])
                ID.hi <- ID.hi[ord] 
                ID.hi <- na.omit(ID.hi[1:min(length(ID.hi),out.cut)]) 

                x.out <- c(x.lo, x.hi)
                ID.lbl <- union(ID.lo, ID.hi)  # combine factors

                panel.text(x.out, y=1.08, labels=ID.lbl,
                           col=ID.color, cex=ID.size, adj=0, srt=90)
              }
            }  # end box

            if (size.pt > 0) {  # regular points
              s.pt <- ifelse (n.groups > 1, size.pt*1.2, size.pt)
              if (box) {
                i.out <- which(x>=fnc.in[1] & x<=fnc.in[2])
                if (n.groups == 1) {
                  i.out.clr <- 1
                  fill.out <- col.fill
                }
                else {
                  i.out.clr <- as.numeric(groups[i.out])
                  fill.out <- col.fill[i.out.clr]
                }
              }  # end box
              else {
                i.out <- 1:length(x)
                fill.out <- col.fill
              }
              color.out <- fill.out
              if (n.groups == 2) color.out <- "black"
              .panel.stripplot(x=x[i.out], 
                 cex=s.pt, pch=shape, col=color.out, fill=fill.out,
                 jitter.data=jitter.data, factor=jitter, ...)
            }

          }  # end c.type == "cont"

        }  # end panel function
      )  # end update
  }


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

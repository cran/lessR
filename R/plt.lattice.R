.plt.lattice <-
function(x, y, facet1, facet2, by, adj.bx.ht, object, n_row, n_col, asp,
         fill, area_fill, color, panel_fill, panel_color,
         trans, size.pt, size.ln,
         xlab, ylab, main, shape, lab_cex, axis_cex,
         lvl=0, ellipse_color=NULL, ellipse_lwd=NULL,
         fit="off", fit_power=1, fit_color=NULL, fit_lwd=NULL, fit_se,
         plot_errors=FALSE, origin=NULL, jitter,
         violin, violin_fill, box, box_fill,
         bw, vbs_ratio, box_adj, a, b, k.iqr, fences, vbs_mean,
         out_shape, out_size,
         out_fill, out_color, out2_fill, out2_color,
         ID, out_cut, ID_color, ID_size,
         axis_fmt="K", axis_x_pre="", axis_y_pre="",
         rotate_x, rotate_y, n_axis_x_skip, n_axis_y_skip,
         width, height, pdf_file, T.type, quiet, ...) {


  if (size.pt[1] == 0) object <- "line"
  size.ln <- size.ln + 0.5  # Trellis plot lines are narrower

  if (!is.null(area_fill)) if (area_fill == "on")
    area_fill <- getOption("violin_fill")

  # if applicable, open graphics window of specified dimensions
  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)
  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)
  if (!in.RStudio && !in.knitr) dev.new(width=width, height=height)

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

  # get lab_x_cex  lab_y_cex
  lab_cex <- getOption("lab_cex")
  lab_x_cex <- getOption("lab_x_cex")
  lab_y_cex <- getOption("lab_y_cex")
  lab_x_cex <- ifelse (is.null(lab_x_cex), lab_cex, lab_x_cex)
  adj <- .RSadj(lab_cex=lab_x_cex); lab_x_cex <- adj$lab_cex
  lab_y_cex <- ifelse (is.null(lab_y_cex), lab_cex, lab_y_cex)
  adj <- .RSadj(lab_cex=lab_y_cex); lab_y_cex <- adj$lab_cex

  is.y <- ifelse (!is.null(y), TRUE, FALSE)
  gl <- .getlabels(xlab, ylab, main, y.nm=is.y, lab_x_cex=lab_x_cex,
                     lab_y_cex=lab_y_cex, facet1.nm=TRUE)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  date.var <- FALSE
  if (is.null(dim(x))) if (.is.date(x)) date.var <- TRUE
  if (date.var) x.lab <- NULL
  y.name <- gl$yn; y.lbl <- gl$yl;
  if (!is.null(gl$yb))
    y.lab <- ifelse (is.null(ylab), gl$yb, "")
  else
    y.lab <- ifelse(is.null(ylab), "", ylab)
  main.lab <- gl$mb
  sub.lab <- gl$sb

  col.bg <- ifelse(sum(col2rgb(panel_fill)) < 370, "transparent", panel_fill)

  n.groups <- ifelse (is.null(by), 1, nlevels(by))

  pt.fill <- fill
  pt.color <- color
  ltype <- character(length=n.groups)
  for (j in 1:n.groups) ltype[j] <- "solid"

  legend_title <- deparse(substitute(by, env=parent.frame()))
  leg.text.cex <- axis_cex * 1.05
  leg.title.cex <- leg.text.cex * 1.14  # abbreviate only returns of type character
# BAD if (!is.null(by)) by <- factor(abbreviate(by, 5), levels(by))

  if (is.null(facet1) && is.null(facet2))
    n.panels <- 1
  else {
    n.panels <- ifelse (is.null(facet2), nlevels(facet1),
                        nlevels(facet1)*nlevels(facet2))
    if (n.panels == 0) n.panels <- 1

    if (T.type %in% c("cont", "cont_cont")) {
      if (is.null(n_col) && is.null(n_row)) {
        n_col <- ifelse (n.panels < 5, 1, 2)
        if ((T.type == "cont")  &&  !is.null(facet2)) {
          n_col <- length(unique(na.omit(facet1)))
          n_row <- length(unique(na.omit(facet2)))
        }
      }
    }
  }

  # customize layout cols and rows, only specify one
  # if n_col or n_row specified, compute the other
  if (n.panels > 1) {
    if (!is.null(n_row)  ||  !is.null(n_col)) {
      if (is.null(n_col)) n_col <- (n.panels %/% n_row) + (n.panels %% n_row>0)
      if (is.null(n_row)) n_row <- (n.panels %/% n_col) + (n.panels %% n_col>0)
    }
  }

  # move strip to left for a single column
  strp <- TRUE;  strp.lft <- FALSE
  if (!is.null(n_col)) {
    if (n_col == 1) {
      strp <- FALSE;  strp.lft <- TRUE
    }
  }
  if (is.null(facet1)) strp <- FALSE


  # ---------------------------------
  if (!date.var) {

    # perhaps print fewer axis tick labels, perhaps re-format labels
    if (!is.null(x)) 
      x.out <- .sparse.labels(x, ax="x", n_axis_x_skip, axis_fmt, axis_x_pre)
    if (!is.null(y)) 
      y.out <- .sparse.labels(y, ax="y", n_axis_y_skip, axis_fmt, axis_y_pre)

    if (T.type == "cont_cont") {  # cont - cont
      # set 1 or 2 conditioning variables
      if (is.null(facet2)) {
        p <- lattice::xyplot(y ~ x | facet1, groups=by,
          scales=list(
            x = list(at=x.out$at, labels=x.out$labels, tck=0.8),
            y = list(at=y.out$at, labels=y.out$labels, tck=0.8)
          ), ...)
      }
      else {  # facet2 is present
        p <- lattice::xyplot(y ~ x | facet1 * facet2, groups=by,
          xlim = range(x.out$at),
          ylim = range(y.out$at),
          scales=list(
            x = list(at=x.out$at, labels=x.out$labels, tck=0.8),
            y = list(at=y.out$at, labels=y.out$labels, tck=0.8)
          ), ...)
      }  # end facet2 is present
    }

# get boxplot with vbs_plot="b" instead
# -------------------------------------
#   else if (T.type == "con_cat") {  # cont - cat
#       jitter <- .4 * jitter
#       if (is.null(facet1)  &&  is.null(facet2)) {
#         p <- lattice::bwplot(y ~ x, groups=by,
#         scales=list(
#           x = list(at=x.out$at, labels=x.out$labels, tck=0.8),
#           y = list(at=y.out$at, labels=y.out$labels, tck=0.8)
#         ), ...)
#       }
#       else if (is.null(facet2)) {
#         p <- lattice::bwplot(y ~ x | facet1, groups=by,
#         scales=list(
#           x = list(at=x.out$at, labels=x.out$labels, tck=0.8),
#           y = list(at=y.out$at, labels=y.out$labels, tck=0.8)
#         ), ...)
#       }
#       else {  # facet2 is present
#         p <- lattice::bwplot(y ~ x | facet1 * facet2, groups=by,
#         scales=list(
#           x = list(at=x.out$at, labels=x.out$labels, tck=0.8),
#           y = list(at=y.out$at, labels=y.out$labels, tck=0.8)
#         ), ...)
#       }
#   }  # end con_cat

    else if (T.type == "cont") {  # cont, VBS plots, possibly over facets
      # set 0, 1 or 2 conditioning variables
      if (is.null(facet1)  &&  is.null(facet2)) {  # 0 cond var
        p <- lattice::stripplot(~ x, groups=by, subscripts=TRUE,
          scales=list(
            x = list(at=x.out$at, labels=x.out$labels, tck=0.8)
          ), ...)
        y.lab <- ""
      }
      else if (is.null(facet2)) {  # 1 cond var
        p <- lattice::stripplot(~ x | facet1, groups=by, subscripts=TRUE,
          scales=list(
            x = list(at=x.out$at, labels=x.out$labels, tck=0.8)
          ), ...)
        y.lab <- ifelse (is.null(ylab), getOption("facet1name"), ylab)
      }
      else  {  # 2 cond var
        p <- lattice::stripplot(~ x | facet1*facet2, groups=by, subscripts=TRUE,
          scales=list(
            x = list(at=x.out$at, labels=x.out$labels, tck=0.8)
          ), ...)
        y.lab <- ""
      }
    }  # end cont
  } # end not date.var

  else {  # x-variable is a Date
    if (T.type == "cont_cont") {  # cont - cont
      # set 1 or 2 conditioning variables
      if (is.null(facet2)) {
        p <- lattice::xyplot(y ~ x | facet1, groups=by,
          scales=list(
            y=list(labels=.axis.format(pretty(y), axis_fmt, "no", axis_y_pre),
                   at=pretty(y), tck=.8)
          ), ...)
      }
      else {  # facet2 is present
        p <- lattice::xyplot(y ~ x | facet1 * facet2, groups=by,
          scales=list(
            y=list(labels=.axis.format(pretty(y), axis_fmt, "no", axis_y_pre),
                   at=pretty(y), tck=.8)
          ), ...)
      }
    }

    else if (T.type == "con_cat") {  # cont - cat
        jitter <- .4 * jitter
        if (is.null(facet1)  &&  is.null(facet2)) {
          p <- lattice::bwplot(y ~ x, groups=by,
            scales=list(
              y=list(labels=.axis.format(pretty(y), axis_fmt, "no", axis_y_pre),
                     at=pretty(y), tck=.8)
          ), ...)
        }
        else if (is.null(facet2)) {
          p <- lattice::bwplot(y ~ x | facet1, groups=by,
            scales=list(
              y=list(labels=.axis.format(pretty(y), axis_fmt, "no", axis_y_pre),
                     at=pretty(y), tck=.8)
            ), ...)
        }
        else {  # facet2 is present
          p <- lattice::bwplot(y ~ x | facet1 * facet2, groups=by,
          scales=list(
            y=list(labels=.axis.format(pretty(y), axis_fmt, "no", axis_y_pre),
                   at=pretty(y), tck=.8)
            ), ...)
        }
    }  # end con_cat

    else if (T.type == "cont") {  # cont
      # set 0, 1 or 2 conditioning variables
      if (is.null(facet1)  &&  is.null(facet2)) {  # 0 cond var
        p <- lattice::stripplot(~ x, groups=by, subscripts=TRUE,
          scales=list(
            x=list(labels=.axis.format(pretty(x), axis_fmt, axis_x_pre, "no"), 
                   at=pretty(x), tck=.8)
          ), ...)
        y.lab <- ""
      }
      else if (is.null(facet2)) {  # 1 cond var
        p <- lattice::stripplot(~ x | facet1, groups=by, subscripts=TRUE,
          scales=list(
            x=list(labels=.axis.format(pretty(x), axis_fmt, axis_x_pre, "no"), 
                   at=pretty(x), tck=.8)
          ), ...)
        y.lab <- ifelse (is.null(ylab), getOption("facet1name"), ylab)
      }
      else  {  # 2 cond var
        p <- lattice::stripplot(~ x | facet1 * facet2, groups=by,
                                subscripts=TRUE,
          scales=list(
            x=list(labels=.axis.format(pretty(x), axis_fmt, axis_x_pre, "no"), 
                   at=pretty(x), tck=.8)
          ), ...)
        y.lab <- ""
      }
    }  # end cont
  } # end is date.var

  p <- update(p, layout=c(n_col, n_row))

  if (length(levels(facet1)) == 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Need more than just one unique value for  facet1\n",
      "Unique values of facet1: ", levels(facet1),"\n\n")
  }
  if (xor(is.null(n_col), is.null(n_row))) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "If you specify  n_col  or  n_row  then \n",
      "  specify parameter  facet1  not parameter  by.\n\n")
  }


  # scale down the point size, grid line width for the multi-panel dot plots
  n.pnl <- length(levels(facet1))
  if (!is.null(facet2)) n.pnl <- n.pnl + length(levels(facet2))
  if (n.pnl > 3  &&  grid_x_lwd > 0.99) grid_x_lwd <- .5 * grid_x_lwd
  if (n.pnl > 3  &&  grid_y_lwd > 0.99) grid_y_lwd <- .5 * grid_y_lwd

  size.mult <- ifelse (n.pnl > 3, 0.70, 0.833)
  size.pt <- size.pt * size.mult

  # separate panels with a border even if turned off when only one plot
  panel_color <- getOption("panel_color")
  panel_frame_color <- ifelse(panel_color == "transparent",
                              "gray30", panel_color)

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

  # separate the axis from the axis labels unless too many rows
  if (is.null(n_row)) n_row <- 1
  if (n_row < 7) {
    pad <- 2.08 - 0.56*log(n_row)
    p <- update(p,
         par.settings=list(
           layout_heights=list(axis_xlab_padding=pad)))
  }

  top.pad <- ifelse (is.null(main), 0, 1)
  if (!is.null(facet1)) top.pad <- 1
  axs.top <- ifelse (is.null(main), 0, 1)  # old: .5, 1

  # get full list of lattice parameters: trellis.par.get()
  #print(trellis.par.get("superimpose.symbol"))
  p <- update(p,
         strip=strp, strip.left=strp.lft, aspect=asp,
         par.strip.text=list(cex=axis_x_cex, col=getOption("strip_text_color")),
         xlab=list(label=x.lab, cex=lab_x_cex, col=l.x_color),
         ylab=list(label=y.lab, cex=lab_y_cex, col=l.y_color),
         main=list(label=main.lab, col=getOption("lab_color")),
         par.settings=list(
           background=list(col=getOption("window_fill")),
           panel.background=list(col=panel_fill),
           layout.heights=list(top.padding=top.pad, axis.top=axs.top),
           axis.line=list(col=panel_frame_color,
             lty=getOption("axis_lty"), lwd=getOption("axis_lwd")),
           strip.border=list(col=getOption("strip_color"), lwd=1),
           strip.background=list(col=getOption("strip_fill")),
           strip.color=list(col=getOption("strip_color")),  # ???
           plot.polygon=list(col=getOption("violin_color"),
             fill=violin_fill, lty="solid", lwd=1),
           plot.line=list(col=pt.color, lty="solid", lwd=1),
           plot.symbol=list(pch=shape, cex=size.pt, col=pt.color,
             fill=pt.fill),
           superpose.symbol=list(pch=shape, cex=size.pt,
             col=pt.color, fill=pt.fill),
           superpose.line=list(col=pt.color, lty=ltype)),
         scales=list(
           x = list(cex=axis_x_cex, rot=rotate_x, col=a.x.text_color),
           y = list(cex=axis_y_cex, rot=rotate_y, col=a.y.text_color))
  )

  if (T.type == "cont_cont") {

    # set legend
    if (n.groups > 1) {  # lattice groups is lessR by
      if (object %in% c("point", "both")) {
        p <- update(p,
             key=list(  # defines the properties of the legend
               space="right",
               text=list(levels(by), cex=leg.text.cex),  # by is a factor
               points=list(pch=21, fill=fill, col=fill, cex=leg.title.cex),
               border="gray80", background=col.bg,
               title=legend_title, cex.title=leg.title.cex))
      }
      else {  # lines
        p <- update(p,
             key=list(
               cex=axis_x_cex, space="right",
               text=list(levels(by)),  # by is always a factor at this point
               border="gray80", background=col.bg,
               lines=list(lwd=size.ln, col=fill),
               title=legend_title, cex.title=leg.title.cex))
      }
    }  # n.groups > 1

    # need to get working
    if (n.groups > 1 &&  fit != "off")  {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "If by parameter used, then currently no fitted lines by group.\n\n")
    }

    p <- update(p,
        panel = function(x, y, ...) {

          panel.grid(h=0, v=-1, col=g.x_color,
                     lwd=grid_x_lwd, lty=grid_x_lty)
          panel.grid(h=-1, v=0, g.y_color,
                     lwd=grid_y_lwd, lty=grid_y_lty)

          if (length(x) > 0) {  # plot only if data for panel

            if (area_fill != "transparent")  # fill under the line
              panel.xyarea(x, y, origin=origin, col=area_fill)

            if (object == "point")
              tp <- "p"
            else if (object == "both")
              tp <- "b"
            else if (object == "line")
              tp <- "l"
            panel.xyplot(x, y, type=tp, col=pt.color, fill=pt.fill,
                         lwd=size.ln, ...)

            if (fit != "off"  &&  n.groups == 1) {

              pf <- .plt.fit (x, y, fit, fit_power, fit_new=NULL)

              x <- pf$x.lv  # x and y get reduced in .plt.fit if NA
              y <- pf$y.lv
              f.ln <- pf$f.ln
              l.ln <- pf$l.ln
              mse <- pf$mse
              b0 <- pf$b0
              b1 <- pf$b1
              Rsq <- pf$Rsq

              mse.pn <- prettyNum(mse, big.mark = ",", scientific = FALSE)
              Rsq.pn <- .fmt(Rsq, 3)
              b0.pn <- .fmt(b0, 3)
              b1.pn <- .fmt(b1, 3)
              if (!quiet) {
                cat("\n")
                if (panel.number() == 1) {
                  cat("Regression analysis of linearized", y.name, "values\n")
                  msg <- paste("Need back transformation of regression model",
                              "to compute predicted values\n\n")
                  cat(msg)
                }
              }
              facet1.name <- getOption("facet1name")
              panel.n <- panel.number()
              if (!quiet) cat(facet1.name, " ", panel.n, "  ",
                  "Line: b0 = ", b0.pn, "  b1 = ", b1.pn, "   ",
                  "Fit: MSE = ", mse.pn, "   Rsq = ", Rsq.pn,
                  sep="", "\n")

              if (fit %in% c("exp", "log", "quad", "null"))
                fit_se[1] <- 0

              se_fill <- getOption("se_fill")
              nrows <- length(f.ln)
              for (j in 1:length(fit_se)) {
                p.ln <- predict(l.ln, se=TRUE)
                prb <- (1 - fit_se[j]) / 2
                up.ln <- f.ln + (qt(prb,nrows-1) * p.ln$se.fit)
                dn.ln <- f.ln - (qt(prb,nrows-1) * p.ln$se.fit)
                panel.polygon(c(x, rev(x)), c(up.ln, rev(dn.ln)),
                              col=se_fill, border="transparent")
              }  # end fit_se

              panel.lines(x, f.ln, col=fit_color, lwd=fit_lwd)

              if (plot_errors)
                panel.segments(y0=f.ln, y1=y, x0=x, x1=x, col="darkred", lwd=1)

            }  # end fit != "off"

            if (lvl > 0)
              panel.ellipse(x, y, center.cex=0,
                            level=lvl, col=ellipse_color, lwd=ellipse_lwd)

          }  # end length > 0
        }  # end panel function
      )  # end update

  }  # end cont_cont


  else if (T.type %in% c("con_cat", "cont")) {

    #myboxStats <- function(...)
      #if (!box_adj)
        #return(boxplot.stats(x))
      #else
        #return(adjboxStats(x, coef=k.iqr, a=a, b=b))

    if (fences) {  # make room for the fences with horizontal lengthening
      p <- update(p,
         prepanel=function(x=x) {

           num5 <- fivenum(x, na.rm=TRUE)
           q1 <- num5[2];  q3 <- num5[4];  iqr <- q3 - q1

           m.c <- ifelse (box_adj, mc(x, na.rm=TRUE), 0)
           if (m.c >= 0) {
             fnc.lwr <- q1 - (k.iqr * exp(a*m.c) * iqr)
             fnc.upr <- q3 + (k.iqr * exp(b*m.c) * iqr)
           }
           else {  # m.c < 0
             fnc.lwr <- q1 - (k.iqr * exp(-b*m.c) * iqr)
             fnc.upr <- q3 + (k.iqr * exp(-a*m.c) * iqr)
           }

           min_x <- min(x, fnc.lwr)
           max.x <- max(x, fnc.upr)

           list(xlim=c(min_x, max.x))
         }
       )
    }  # end fences

    if (n.groups > 1) {
      p <- update(p, 
           key=list(  # defines the properties of the legend
             space="top", columns=n.groups,
             text=list(levels(by), cex=leg.text.cex),
             points=list(pch=21, fill=pt.fill, col=pt.color, cex=leg.title.cex),
             border="gray80", background=col.bg, padding.text=2,
             title=legend_title, cex.title=leg.title.cex))

      box_fill <- .maketrans(box_fill, 0.25*256)  # more trans box needed
    }


    p <- update(p,
          
        par.settings=list(  # col option does not work directly on panel.bwplot
          box.rectangle=list(fill=box_fill, lwd=2,
                             col=getOption("box_color")),
          box.umbrella=list(col=getOption("box_color"), lty="solid")
        ),

        panel=function(x=x, box.ratio, wID=ID, ...,
                       groups=groups, subscripts=subscripts) {

          panel.grid(h=0, v=-1, col=g.x_color, lwd=grid_x_lwd, lty=grid_x_lty)

          if (length(x) > 0) {  # plot only if data for panel

            jitter_data <- ifelse (jitter > 0, TRUE, FALSE)
            size.pt <- size.pt * 1.2  # lattice adjustment

            num5 <- fivenum(x, na.rm=TRUE)
            q1 <- num5[2]
            q3 <- num5[4]
            iqr <- q3 - q1
            fnc.in <- rep(NA_real_, 2)   # inner fences
            fnc.out <- rep(NA_real_, 2)  # outer fences

            m.c <- ifelse(box_adj, mc(x, na.rm=TRUE), 0)
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

            if (violin && length(x)>1) {
              # to get a violin plot, cannot have y and facet1
              # just Plot(x) gives a VBS plot with no groups and only 1 panel
              # a giant do loop that iterates over groups, i.e., panel.number()
              vw <- ifelse (!is.null(y) && !is.null(facet1), FALSE, TRUE)
              vf <- ifelse (n.panels>1, violin_fill[panel.number()], violin_fill)
              panel.violin(x=x, ...,
                  col=vf,
                  border=getOption("violin_color"),
                  varwidth=vw, box.width=vbs_ratio, bw=bw)
            }

            if ((box || size.pt>0) && length(x)>1) {

              n.lvl <- ifelse (is.null(facet1), 1, nlevels(facet1))
              n <- adj.bx.ht
              int <- ifelse (n <= 25000, 4.10 - 0.000065*n, 3.25 - 0.00003*n)
              denom <- int - 0.5*n.lvl
              if (denom < 1.5) denom <- 1.5

              if (box) {  # could just be a scatterplot with red outlier points
                if (!box_adj)  # did the panel.number() access in .panel.bwplot()
                  .panel.bwplot(x=x, ..., pch="|", vbs_mean=vbs_mean,
                      fences=fences,
                      box.ratio=vbs_ratio/denom, mean_color=out_fill,
                      stats=boxplot.stats, k.iqr=k.iqr, do.out=FALSE)
                else
                  .panel.bwplot(x=x, ...,  pch="|", vbs_mean=vbs_mean,
                      fences=fences,
                      box.ratio=vbs_ratio/denom, mean_color=out_fill,
                      stats=adjboxStats, k.iqr=k.iqr, a=a, b=b, do.out=FALSE)
             }

              # convert name such as "circle" to actual pch num, e.g., 21
              shp <- .plt.shapes(shape, out_shape)
              shape <- shp$shape
              out_shape <- shp$out_shape

              # plotting a subset of x requires adjusting y, in .panel.strip
              # identify extreme outliers, if any
              x.out <- which(x<fnc.out[1] | x>fnc.out[2])
              if (n.groups == 1) {
                x.out_clr <- 1
                fill_out <- out2_fill
              }
              else {  # if a by group, and plot regular colors to differentiate
                fill_out <- fill
                out2_color <- fill
              }

              # plot extreme outliers
#             .panel.stripplot(x=x[x.out],
              panel.stripplot(x=x[x.out],
                 cex=out_size, col=out2_color, fill=fill_out, pch=out_shape, ...)

              # identify outliers, if any
              x.out <- which(x>=fnc.out[1] & x<fnc.in[1] |
                              x>fnc.in[2] & x<=fnc.out[2])
              if (n.groups == 1) {
                x.out_clr <- 1
                fill_out <- out_fill
              }
              else {  # if a by group, and plot regular colors to differentiate
                fill_out <- fill
                out_color <- fill
              }

              # plot outliers
              panel.stripplot(x=x[x.out],
                 cex=out_size, col=out_color, fill=fill_out, pch=out_shape, ...)

              # label outliers
              if (out_cut > 0) {
                wwID <- wID[subscripts]

                ind.lo <- which(x < fnc.in[1])
                x.lo <- x[ind.lo]
                ID.lo <- wwID[ind.lo]
                ord <- order(x.lo, decreasing=FALSE)
                x.lo <- x.lo[ord]
                x.lo <- na.omit(x.lo[1:min(length(x.lo),out_cut)])
                ID.lo <- ID.lo[ord]
                ID.lo <- na.omit(ID.lo[1:min(length(ID.lo),out_cut)])

                ind.hi <- which(x > fnc.in[2])
                x.hi <- x[ind.hi]
                ID.hi <- wwID[ind.hi]
                ord <- order(x.hi, decreasing=TRUE)
                x.hi <- x.hi[ord]
                x.hi <- na.omit(x.hi[1:min(length(x.hi),out_cut)])
                ID.hi <- ID.hi[ord]
                ID.hi <- na.omit(ID.hi[1:min(length(ID.hi),out_cut)])

                x.out <- c(x.lo, x.hi)
                ID.lbl <- union(ID.lo, ID.hi)  # combine factors
                ix <- order(-abs(x.out))[1:out_cut]  # subset to largest abs

                panel.text(x.out[ix], y=1.08, labels=ID.lbl[ix],
                           col=ID_color, cex=ID_size, adj=0, srt=90)
              }
            }  # end box || size.pt > 0

            # regular or all (no box)  points
            if (size.pt > 0) {
              s.pt <- ifelse (n.groups > 1, size.pt*1.2, size.pt)
              if (box) {
                x.out <- which(x>=fnc.in[1] & x<=fnc.in[2])
                if (n.groups == 1) {
                  x.out_clr <- 1
                  fill_out <- pt.fill
                }
                else {
                  x.out_clr <- as.numeric(groups[x.out])
                  fill_out <- pt.fill[x.out_clr]
                }
              }  # end box
              else  # all pts
                x.out <- 1:length(x)

              .panel.stripplot(x=x[x.out],   # in plt.panel.R
                 cex=s.pt, pch=shape, col=pt.color, fill=pt.fill,
                 jitter_data=jitter_data, factor=jitter, ...)
            }

          }  # end length(x) > 0
        }  # end panel function
      )  # end update
  }


  # display
  if (!is.null(pdf_file)) {
    pdf(pdf_file, width=width, height=height, onefile=FALSE)
    print(p)
    dev.off()
  }
  else {
    print(p)
  }

}

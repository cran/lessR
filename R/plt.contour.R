.plt.contour <-
function(x, y, contour_n, contour_nbins,
         theme, scale_x, scale_y, pad_x, pad_y, x.lab, y.lab,
         ellipse, ellipse_color, ellipse_lwd, fit.line, fit_color, fit_lwd,
         axis_x_pre, axis_y_pre, axis_fmt, xlab.adj, ylab.adj) {


  if (theme %in% c("white", "gray", "light"))
    clrs <- colorRampPalette(c("white", gray.colors(contour_n,
                             rev=TRUE, start=0.20, end=0.92)))
  else {
    sc <- .corcolors()
    clrs <- colorRampPalette(c("white", getColors(sc$fill_hi)))
  }

  good <- complete.cases(x, y)
  x <- x[good, , drop=FALSE]
  y <- y[good, , drop=FALSE]

  dens <- MASS::kde2d(x, y, n=50)
  dz   <- dens$z / sum(dens$z)

  # 2. Find the 95% mass threshold
  z_sorted    <- sort(as.vector(dz), decreasing=TRUE)
  z_cum       <- cumsum(z_sorted)
  z_idx       <- which(z_cum >= 0.95)[1]
  z_threshold <- z_sorted[z_idx]
 #p(z_threshold)

  # 3. Contour-derived ranges (with extreme outliers, these overestimate)
  mask      <- dz >= z_threshold
  x_idx     <- which(apply(mask, 1, any))
  y_idx     <- which(apply(mask, 2, any))
  xlim_cont <- range(dens$x[x_idx])
  ylim_cont <- range(dens$y[y_idx])

  # 4. Ellipse-derived ranges (truncated to data if needed)  
  mu <- c(mean(x), mean(y))
  Sigma <- cov(cbind(x, y))
  ellipse.pts <- ellipse::ellipse(Sigma, centre=mu, level=0.95)
  ellipse.trunc <- ellipse.pts[
    ellipse.pts[, 1] >= min(x) & ellipse.pts[, 1] <= max(x) &
      ellipse.pts[, 2] >= min(y) & ellipse.pts[, 2] <= max(y),
  ]

  # 5. Full data ranges
  data_xlim <- range(x)
  data_ylim <- range(y)

  # 6. Decide per axis
  if ((xlim_cont[1] > data_xlim[1]) || (xlim_cont[2] < data_xlim[2])) {
    x.lim <- range(ellipse.trunc[, 1])
    x_decision <- "ellipse range"
  } 
  else {
    x.lim <- data_xlim
    x_decision <- "full data"
  }

  if ((ylim_cont[1] > data_ylim[1]) || (ylim_cont[2] < data_ylim[2])) {
    y.lim <- range(ellipse.trunc[, 2])
    y_decision <- "ellipse range"
  }
  else {
    y.lim <- data_ylim
    y_decision <- "full data"
  }

# 7. Report decisions
#message("X axis: contour [", paste(round(xlim_cont,2), collapse=" to "),
#  "], data [", paste(round(data_xlim,2), collapse=" to "),
#  "] --> using ", x_decision)
#message("Y axis: contour [", paste(round(ylim_cont,2), collapse=" to "),
#  "], data [", paste(round(data_ylim,2), collapse=" to "),
#  "] --> using ", y_decision)

  # axis limits
  if (is.null(scale_x)) {
    x.marginL <- diff(x.lim) * pad_x[1]
    x.marginR <- diff(x.lim) * pad_x[2]
    x.lim <- x.lim + c(-x.marginL, x.marginR)
    axT1 <- pretty(x.lim, eps.correct=0)
  }
  else {
    x.lim <- scale_x[1:2]
    axT1 <- axTicks(1, axp=scale_x)
  }
  lbl.x <- .axis.format(axT1, axis_fmt, axis_x_pre, axis_y_pre="no")
   if (is.null(scale_y)) {
    y.marginL <- diff(y.lim) * pad_y[1]
    y.marginR <- diff(y.lim) * pad_y[2]
    y.lim <- y.lim + c(-y.marginL, y.marginR)
    ayT2 <- pretty(y.lim, eps.correct=0)
  }
  else {
    y.lim <- scale_y[1:2]
    ayT2 <- axTicks(1, axp=scale_y)
  }
  lbl.y <- .axis.format(ayT2, axis_fmt, axis_x_pre="no", axis_y_pre)

  # Plot
  ax <- .axes_dim()
  filled.contour(
    dens$x, dens$y, dens$z,
    xlim=x.lim, ylim=y.lim,
    nlevels=contour_n,
    color.palette=clrs,

    plot.axes={

      # buffer to y.lab vertical extra room, preview actual axis text
      ticks <- ayT2[ayT2 >= y.lim[1] & ayT2 <= y.lim[2]]
      ticks <- .axis.format(ticks, axis_fmt, axis_x_pre="no", axis_y_pre)
      mnv <- max(nchar(ticks))
      buf.y <- -0.8 + 0.32* mnv

      old.par <- par()
#     par(mgp=c(8, 2.25, 0), tcl=-1)
      par(mgp=c(3, .5, 0), tcl=-.28)  # axes text closer to the axes
      axis(1, at=axT1, labels=lbl.x,
           cex.axis=ax$axis_x_cex*1.1, col.axis=ax$axis_x_text_color)
      axis(2, at=ayT2, labels=lbl.y,
           cex.axis=ax$axis_y_cex*1.1, col.axis=ax$axis_y_text_color)
      lab.cex <- getOption("lab_cex")*1.2
      title(xlab=x.lab, line=2.25+xlab.adj, cex.lab=lab.cex)
      title(ylab=y.lab, line=3+ylab.adj+buf.y, cex.lab=lab.cex)

      if (ellipse[1] > 0) {
        for (j in seq_along(ellipse)) {
          e <- ellipse::ellipse(Sigma, centre=mu,
                                level=ellipse[j], npoints=100)
          lines(e, col=ellipse_color, lwd=ellipse_lwd)
        }
        ellipse <- 0
      }  # end do.ellipse
#     lines(ellipse.trunc, col=ellipse_color, lwd=2)

      if (fit.line != "off") {
        pf <- .plt.fit(x, y, fit.line=fit.line, fit_power=0, fit_new=NULL)
        x.lv <- pf$x.lv  # x and y get reduced in .plt.fit if NA
        f.ln <- pf$f.ln  # fitted
        lines(x.lv, f.ln, col=fit_color, lwd=fit_lwd)
        fit.line <- "off"
      }
    },

    key.axes={axis(4, cex.axis=ax$axis_x_cex*1.1)}  # legend
  )
  par(old.par)
}


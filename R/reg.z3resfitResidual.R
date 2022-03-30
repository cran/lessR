.reg3resfitResidual <-
function(lm.out, cook, cooks_cut,
         pdf=FALSE, width=5, height=5, manage.gr=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.keep <- nrow(lm.out$model)
  
  fit <- lm.out$fitted
  res <- lm.out$residuals
  class(lm.out) <- "lm"

  # get largest Cook's distance
  max.cook <- max(cook, na.rm=TRUE)
  if (max.cook < cooks_cut) {
    cooks_cut <- floor(max.cook*100)/100
    sub.lab <- paste("Point with largest Cook's Distance of",
                     .fmt(max.cook,2), "is labeled")
  }
  else
    sub.lab <- paste("Points with Cook's Distance >",
                     cooks_cut, "are labeled")


  # pdf graphics option
  if (pdf) { 
    pdf_file <- "RegResidFitted.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  plt.i <- plt.i + 1L
  plt.title[plt.i] <- "Residuals vs Fitted Values"

  # plot of residuals vs fitted
  ord <- order(fit)
  fit.ord <- fit[ord]
  res_ord <- res[ord]

  bm <- .9; tm <- .25;  rm <- .25
  # get left margin to adapt to size of axis numbers
  ctitle <- ""
  max.width <- strwidth(as.character(max(pretty(res_ord))), units="inches")
  margs <- .marg(max.width, y.lab=nm[1], x.lab=nm[2], main=ctitle, sub=NULL)
  lm <- margs$lm
  par(bg=getOption("window_fill"))

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  par(mai=c(bm, lm, tm, rm))


  plot(fit.ord, res_ord, type="n", axes=FALSE, ann=FALSE)

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4],
       col=getOption("panel_fill"), border=getOption("panel_color"))

  abline(v=axTicks(1), col=getOption("grid_x_color"),
         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))
  abline(h=axTicks(2), col=getOption("grid_y_color"),
         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))

  .axes(NULL, NULL, axTicks(1), axTicks(2))

  main.lab <- NULL
  x.label <- "Fitted Values"
  y.label <- "Residuals"
  .axlabs(x.label, y.label, main.lab, sub.lab, lab_x_cex=0.9, lab_y_cex=0.9,
          xlab_adj=1.4)

  fill <- getOption("pt_fill")
  color <- getOption("pt_color")
  points(fit.ord, res_ord, pch=21, col=color, bg=fill, cex=0.76)

  abline(h=0, lty="dotted", lwd=1.5, col=getOption("bar_fill_cont"))
  lines(lowess(fit.ord, res_ord, f=.9), col=getOption("pt_color"))
  res_c <- res[which(cook >= cooks_cut)]
  fit.c <- fit[which(cook >= cooks_cut)]
  if (length(fit.c) > 0) {
    points(fit.c, res_c, col=color, pch=19)
    text(fit.c, res_c, names(fit.c), pos=1, cex=.8)
  }

  if (pdf) {
    dev.off()
    .showfile(pdf_file, "residuals vs. fitted plot")
  }

  return(list(i=plt.i, ttl=plt.title))

}

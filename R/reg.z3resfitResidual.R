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
    txt <- paste("Largest Cook's Distance, ", .fmt(max.cook,2), 
      ", is highlighted", sep="")
  }
  else
    txt <- paste("Points with Cook's Distance >", cooks_cut, "are highlighted")


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

  par(bg=getOption("window_fill"))

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
  sub.lab <- "Largest Cook's Distance Highlighted"
  x.label <- "Fitted Values"
  y.label <- "Residuals"
  .axlabs(x.label, y.label, main.lab, sub.lab, max.lbl.y=3, cex.lab=0.85) 

  col_fill <- getOption("pt_fill")
  col_color <- getOption("pt_color")
  points(fit.ord, res_ord, pch=21, col=col_color, bg=col_fill, cex=0.8)

  abline(h=0, lty="dotted", lwd=1.5, col=getOption("bar_fill_ordered"))
  lines(lowess(fit.ord, res_ord, f=.9), col=getOption("pt_color"))
  res_c <- res[which(cook >= cooks_cut)]
  fit.c <- fit[which(cook >= cooks_cut)]
  if (length(fit.c) > 0) {
    col.out <- getOption("pt_color")
    points(fit.c, res_c, col=col.out, pch=19)
    text(fit.c, res_c, names(fit.c), pos=1, cex=.8)
  }

  if (pdf) {
    dev.off()
    .showfile(pdf_file, "residuals vs. fitted plot")
  }

  return(list(i=plt.i, ttl=plt.title))

}

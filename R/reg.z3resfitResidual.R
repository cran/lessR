.reg3resfitResidual <-
function(lm.out, cook, cooks.cut,
         pdf=FALSE, width=5, height=5, manage.gr=FALSE) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.keep <- nrow(lm.out$model)
  
  fit <- lm.out$fitted
  res <- lm.out$residuals
  class(lm.out) <- "lm"

  # get largest Cook's distance
  max.cook <- max(cook, na.rm=TRUE)
  if (max.cook < cooks.cut) {
    cooks.cut <- floor(max.cook*100)/100
    txt <- paste("Largest Cook's Distance, ", .fmt(max.cook,2), 
      ", is highlighted", sep="")
  }
  else
    txt <- paste("Points with Cook's Distance >", cooks.cut, "are highlighted")


  # pdf graphics option
  if (pdf) { 
    pdf.file <- "RegResidFitted.pdf"
    pdf(file=pdf.file, width=width, height=height)
  }

  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  plt.i <- plt.i + 1L
  plt.title[plt.i] <- "Residuals vs Fitted Values"

  # plot of residuals vs fitted
  ord <- order(fit)
  fit.ord <- fit[ord]
  res.ord <- res[ord]

  par(bg=getOption("window.fill"))

  plot(fit.ord, res.ord, type="n", axes=FALSE, ann=FALSE)

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4],
       col=getOption("panel.fill"), border=getOption("panel.color"))

  abline(v=axTicks(1), col=getOption("grid.x.color"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))
  abline(h=axTicks(2), col=getOption("grid.y.color"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))

  .axes(NULL, NULL, axTicks(1), axTicks(2),
        par("usr")[1], par("usr")[3])

  main.lab <- NULL
  sub.lab <- "Largest Cook's Distance Highlighted"
  x.label <- "Fitted Values"
  y.label <- "Residuals"
  .axlabs(x.label, y.label, main.lab, sub.lab, max.lbl.y=3, cex.lab=0.85) 

  col.fill <- getOption("pt.fill")
  col.color <- getOption("pt.color")
  points(fit.ord, res.ord, pch=21, col=col.color, bg=col.fill, cex=0.8)

  abline(h=0, lty="dotted", lwd=1.5, col=getOption("bar.fill"))
  lines(lowess(fit.ord, res.ord, f=.9), col=getOption("pt.color"))
  res.c <- res[which(cook>=cooks.cut)]
  fit.c <- fit[which(cook>=cooks.cut)]
  if (length(fit.c) > 0) {
    col.out <- getOption("pt.color")
    points(fit.c, res.c, col=col.out, pch=19)
    text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
  }

  if (pdf) {
    dev.off()
    .showfile(pdf.file, "residuals vs. fitted plot")
  }

  return(list(i=plt.i, ttl=plt.title))

}

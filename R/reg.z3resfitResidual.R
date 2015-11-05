.reg3resfitResidual <-
function(lm.out, cook, cooks.cut,
         pdf=FALSE, pdf.width=5, pdf.height=5, manage.gr=FALSE) {

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
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
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
  .plt.main(fit.ord, res.ord, by=NULL, type="p", 
      n.cat=getOption("n.cat"), col.fill=getOption("col.fill.pt"),
      col.stroke=getOption("col.stroke.pt"),
      col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
      shape.pts=21, col.area=NULL, col.box="black", 
      cex.axis=.85, col.axis="gray30", xy.ticks=TRUE,
      xlab="Fitted Values", ylab="Residuals",
      main="", cex=NULL, kind="default",
      fit.line="none", col.fit.line="black", bubble.size=.25,
      ellipse=FALSE, diag=FALSE, col.diag=par("fg"), lines.diag=TRUE,
      quiet=TRUE, sub=txt, cex.sub=.8) 
  abline(h=0, lty="dotted", lwd=1.5, col=getOption("col.fill.bar"))
  lines(lowess(fit.ord, res.ord, f=.9), col=getOption("col.stroke.pt"))
  res.c <- res[which(cook>=cooks.cut)]
  fit.c <- fit[which(cook>=cooks.cut)]
  if (length(fit.c) > 0) {
    col.out <- getOption("col.stroke.pt")
    points(fit.c, res.c, col=col.out, pch=19)
    text(fit.c, res.c, names(fit.c), pos=1, cex=.8)
  }

  if (pdf) {
    dev.off()
    .showfile(pdf.file, "residuals vs. fitted plot")
  }

  return(list(i=plt.i, ttl=plt.title))

}

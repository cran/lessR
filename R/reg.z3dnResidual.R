.reg3dnResidual <-
function(lm.out, pdf=FALSE, width=5, height=5, manage.gr=FALSE, ...) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.keep <- nrow(lm.out$model)


  # pdf graphics option
  if (pdf) {
    pdf.file <- "RegResiduals.pdf"
    pdf(file=pdf.file, width=width, height=height)
  }

  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  plt.i <- plt.i + 1L
  plt.title[plt.i] <- "Distribution of Residuals"

  # frequency distribution of residuals
  .dn.main(lm.out$residuals, 
    col.fill=getOption("bar.fill"),
    col.bg=getOption("panel.fill"), 
    col.box=getOption("panel.color"),
    col.nrm="gray40", col.gen="gray40",
    col.fill.nrm="transparent", col.fill.gen="transparent",
    xlab="Residuals", quiet=TRUE)

  if (pdf) {
    dev.off()
    .showfile(pdf.file, "residuals plot")
  }

  invisible(list(i=plt.i, ttl=plt.title))

}

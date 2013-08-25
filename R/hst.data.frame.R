hst.data.frame <-
function(x, n.cat,
         col.fill, col.stroke, col.bg, col.grid, col.reg,
         over.grid, cex.axis, col.axis, col.ticks, breaks, bin.start, bin.width,
         bin.end, prop, cumul, digits.d, xlab, ylab, main, quiet,
         pdf.width, pdf.height, ...)  {


  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu > n.cat) {

      pdf.file <- paste("Hist_", x.name, ".pdf", sep="")
      .opendev(pdf.file=pdf.file, pdf.width, pdf.height)

      .hst.main(x[,i], col.fill, col.stroke, col.bg, col.grid, col.reg,
          over.grid, cex.axis, col.axis, col.ticks, breaks, bin.start, bin.width,
          bin.end, prop, cumul, digits.d, xlab, ylab, main, quiet, ...)

      dev.off()
      if (!quiet) .showfile(pdf.file, "histogram")

    }

    if (is.numeric(x[,i]) && nu <= n.cat)
      cat("\n>>> ", x.name,  "is numeric,",
          "but only has", nu, "<= n.cat =", n.cat, "levels,",
          "so treat as a categorical variable.\n",
          "   To obtain the histogram decrease  n.cat  to specify a",
          "lower number of unique values.\n",
          "   Suggest making this variable a factor with R factor function.\n")

  }
  
}

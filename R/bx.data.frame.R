bx.data.frame <-
function(x, n.cat,
         col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis, col.ticks,
         horiz, add.points, xlab, main, digits.d, quiet,
         pdf.width, pdf.height, ...)  {


  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    x.name <- names(x)[i]
    options(xname = x.name)

    if (is.numeric(x[,i]) && nu > n.cat) {

      pdf.file <- paste("BoxPlot_", x.name, ".pdf", sep="")
      .opendev(pdf.file=pdf.file, pdf.width, pdf.height)

      .bx.main(x[,i], col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis, col.ticks,
         horiz, add.points, xlab, main, digits.d, quiet, ...)

      dev.off()
      .showfile(pdf.file, "boxplot")

    }

    if (is.numeric(x[,i]) && nu <= n.cat)
      cat("\n>>> ", x.name,  "is numeric,",
          "but only has", nu, "<= n.cat =", n.cat, "levels,",
          "so treat as a categorical variable.\n",
          "   To obtain the boxplot decrease  n.cat  to specify a",
          "lower number of unique values.\n",
          "   Suggest making this variable a factor with R factor function.\n")

  }
  
}

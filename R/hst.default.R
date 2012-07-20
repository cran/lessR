hst.default <- 
function(x, col.bars=NULL, col.border=NULL, col.bg=NULL, 
         col.grid=NULL, col.reg="snow2", over.grid=FALSE,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         prop=FALSE, cumul=c("off", "on", "both"), 
         digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL, text.out=TRUE,
         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  cumul <- match.arg(cumul)

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  .hst.default(x, col.bars, col.border, col.bg, 
         col.grid, col.reg, over.grid,
         colors, cex.axis, col.axis, col.ticks,
         breaks, bin.start, bin.width,
         prop, cumul, digits.d, xlab, ylab, main, text.out, ...) 

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "histogram")
  }

}

hst.default <- 
function(x,
         col.fill=getOption("col.fill.bar"), 
         col.stroke=getOption("col.stroke.bar"),
         col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),

         col.reg="snow2", over.grid=FALSE,
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",

         breaks="Sturges", bin.start=NULL, bin.width=NULL,

         prop=FALSE, cumul=c("off", "on", "both"), 
         digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL, text.out=TRUE,

         pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {

  cumul <- match.arg(cumul)

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  .hst.default(x, col.fill, col.stroke, col.bg, col.grid, col.reg,
       over.grid, cex.axis, col.axis, col.ticks, breaks, bin.start, bin.width,
       prop, cumul, digits.d, xlab, ylab, main, text.out, ...) 

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "histogram")
  }

}

bc.default <-
function(x, by=NULL, 

         col.bars=NULL, col.border="black", col.bg="ghostwhite",
         col.grid="grey86", random.col=FALSE,
         colors=c("blue", "gray", "rose", "green", "gold", "red",
                  "rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, brief=TRUE, prop=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL,
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",

         beside=FALSE, col.low=NULL, col.hi=NULL, count.names=NULL,

         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE, 

         text.out=TRUE, pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  if (!is.null(pdf.file)) on.exit(dev.off(), add=TRUE)

  .bc.default(x, by, 
         col.bars, col.border, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, brief, prop, xlab, ylab, main,
         cex.axis, col.axis, col.ticks, beside, col.low, col.hi, count.names,
         legend.title, legend.loc, legend.labels, legend.horiz, text.out, ...)

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "bar chart")
  }


}

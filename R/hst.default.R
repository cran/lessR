hst.default <- 
function(x, col.bars=NULL, col.border=NULL, col.bg=NULL, 
         col.grid=NULL, col.reg="snow2", over.grid=FALSE,
         colors=c("blue", "gray", "rose", "green", "gold", "red"), 
         cex.axis=.85, col.axis="gray30", col.ticks="gray30",
         breaks="Sturges", bin.start=NULL, bin.width=NULL,
         prop=FALSE, cumul=c("off", "on", "both"), 
         digits.d=NULL, xlab=NULL, ylab=NULL, main=NULL, ...) {

  if (missing(colors)) col.default <- TRUE else col.default <- FALSE
  colors <- match.arg(colors)
  if (col.default && !is.null(getOption("colors"))) colors <- getOption("colors")

  cumul <- match.arg(cumul)

 .hst.default(x, col.bars, col.border, col.bg, 
         col.grid, col.reg, over.grid,
         colors, cex.axis, col.axis, col.ticks,
         breaks, bin.start, bin.width,
         prop, cumul, digits.d, xlab, ylab, main, ...) 

}

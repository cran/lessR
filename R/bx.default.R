bx.default <-
function(x, col.box=NULL, col.pts=NULL, 
        col.bg=NULL, col.grid=NULL,
        colors=c("blue", "gray", "rose", "green", "gold", "red"), 
        cex.axis=.85, col.axis="gray30", col.ticks="gray30",
        horiz=TRUE, dotplot=FALSE,
        xlab=NULL, main=NULL, digits.d=NULL, ...) {        

  if (missing(colors)) col.default <- TRUE else col.default <- FALSE
  colors <- match.arg(colors)
  if (col.default && !is.null(getOption("colors"))) colors <- getOption("colors")

  .bx.default(x, col.box, col.pts, col.bg, col.grid,
          colors, cex.axis, col.axis, col.ticks,
          horiz, dotplot, xlab, main, digits.d, ...)      

}

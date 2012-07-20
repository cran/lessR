bx.default <-
function(x, col.box=NULL, col.pts=NULL, 
        col.bg=NULL, col.grid=NULL,
        colors=c("blue", "gray", "rose", "green", "gold", "red"), 
        cex.axis=.85, col.axis="gray30", col.ticks="gray30",
        horiz=TRUE, dotplot=FALSE,
        xlab=NULL, main=NULL, digits.d=NULL, text.out=TRUE,
        pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {        


  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  .bx.default(x, col.box, col.pts, col.bg, col.grid,
          colors, cex.axis, col.axis, col.ticks,
          horiz, dotplot, xlab, main, digits.d, text.out, ...)      

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "box chart")
  }

}

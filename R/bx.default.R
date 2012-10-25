bx.default <-
function(x, col.fill=getOption("col.fill.pt"),
        col.stroke=getOption("col.stroke.pt"), 
        col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),

        cex.axis=.85, col.axis="gray30", col.ticks="gray30",
        xlab=NULL, main=NULL, digits.d=NULL, text.out=TRUE,

        horiz=TRUE, add.points=FALSE,
        pdf.file=NULL, pdf.width=5, pdf.height=5, ...)  {        


  # set up graphics system
  .opendev(pdf.file, pdf.width, pdf.height)

  .bx.default(x, col.fill, col.stroke, col.bg, col.grid,
          cex.axis, col.axis, col.ticks,
          horiz, add.points, xlab, main, digits.d, text.out, ...)      

  # terminate pdf graphics system
  if (!is.null(pdf.file)) {
    dev.off()
    .showfile(pdf.file, "box chart")
  }

}

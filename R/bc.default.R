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

         legend.title=NULL, legend.loc=NULL, legend.labels=NULL,
         legend.horiz=FALSE, ...) {


  if (missing(colors)) no.colors <- TRUE else no.colors <- FALSE
  colors <- match.arg(colors)
  if (no.colors && !is.null(getOption("colors"))) colors <- getOption("colors")


  .bc.default(x, by, 
         col.bars, col.border, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, brief, prop, xlab, ylab, main,
         cex.axis, col.axis, col.ticks, beside, col.low, col.hi, count.names,
         legend.title, legend.loc, legend.labels, legend.horiz, ...)


}

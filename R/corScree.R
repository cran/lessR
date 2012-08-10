corScree <- 
function (x=mycor, 
          colors=c("blue", "gray", "rose", "green", "gold", "red"),
          main=NULL, pdf=FALSE, pdf.width=5, pdf.height=5, ...) {


  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  if (missing(colors)) 
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)
  
  # extract eigenvectors
  eig <- eigen(mycor, symmetric=TRUE, only.values=TRUE)
  ev <- eig$values

  # set up graphics system for 2 windows
  if (!pdf) {
    .graphwin(2)
    dev.set(which=3)
  }
  else { 
    pdf.file <- "Scree.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }

  # scree plot
  .lc.main(ev, type=NULL, 
         col.line=NULL, col.area=NULL, col.box="black",
         col.pts=NULL, col.fill=NULL, trans.pts=NULL,
         shape.pts=21, col.grid=NULL, col.bg=NULL, colors,
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab="Eigenvalue", main=main, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,
         center.line="off", text.out=FALSE, ...)

  if (pdf) {
    dev.off()
    .showfile(pdf.file, "scree chart")
  }


  if (!pdf) 
    dev.set(which=4) 
  else { 
    pdf.file <- "ScreeDiff.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }

  # differences scree plot
  ev.diff <- -diff(ev)

  .lc.main(ev.diff, type=NULL, 
         col.line=NULL, col.area=NULL, col.box="black",
         col.pts=NULL, col.fill=NULL, trans.pts=NULL,
         shape.pts=21, col.grid=NULL, col.bg=NULL, colors,
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE,
         xlab=NULL, ylab="Difference of Successive Eigenvalues",
         main=main, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,
         center.line="off", text.out=FALSE, ...)
   n.dregs <- ceiling(length(ev.diff)/1.35)  # get bottom sequence of ev differences
   dregs <- numeric(length=n.dregs)
   for (i in 1:n.dregs) dregs[i] <- ev.diff[length(ev.diff)-(i-1)] 
   abline(h=mean(dregs), col="gray50", lwd=2)

  if (pdf) {
    dev.off()
    .showfile(pdf.file, "scree difference chart")
    cat("\n\n")
  }

  cat("Eigenvalues of", deparse(substitute(x)), "\n")
  .dash(20)
  print(round(ev,3))

  cat("\n")
  cat("Differences of Successive Eigenvalues of", deparse(substitute(x)), "\n")
  .dash(46)
  print(round(ev.diff,3))



}






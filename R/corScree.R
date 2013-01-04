corScree <- 
function (x=mycor, 
          main=NULL, pdf=FALSE, pdf.width=5, pdf.height=5, ...) {


  cor.name <- deparse(substitute(x))
  if (!exists(cor.name, where=.GlobalEnv)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n")
  }

  # set up graphics system for 2 windows
  if (!pdf) {
    .graphwin(2)
    dev.set(which=3)
  }
  else { 
    pdf.file <- "Scree.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }
  
  # extract eigenvectors
  eig <- eigen(mycor, symmetric=TRUE, only.values=TRUE)
  ev <- eig$values

  # scree plot
  .lc.main(ev, type=NULL, 
         col.line=getOption("col.stroke.bar"),
         col.area=NULL, col.box="black",
         col.stroke=getOption("col.stroke.pt"),
         col.fill=getOption("col.fill.pt"),
         shape.pts=21, col.grid=getOption("col.grid"),
         col.bg=getOption("col.bg"),
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE, line.width=1.1,
         xlab=NULL, ylab="Eigenvalue", main=main, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,
         center.line="off", quiet=TRUE, ...)

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
         col.line=getOption("col.stroke.bar"),
         col.area=NULL, col.box="black",
         col.stroke=getOption("col.stroke.pt"),
         col.fill=getOption("col.fill.pt"),
         shape.pts=21, col.grid=getOption("col.grid"),
         col.bg=getOption("col.bg"),
         cex.axis=.85, col.axis="gray30",
         col.ticks="gray30", xy.ticks=TRUE, line.width=1.1,
         xlab=NULL, ylab="Eigenvalue", main=main, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time.start=NULL, time.by=NULL, time.reverse=FALSE,
         center.line="off", quiet=TRUE, ...)
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
  .dash(15+nchar(deparse(substitute(x))))
  print(round(ev,3))

  cat("\n")
  cat("Differences of Successive Eigenvalues of", deparse(substitute(x)), "\n")
  .dash(35+nchar(deparse(substitute(x))))
  print(round(ev.diff,3))

}

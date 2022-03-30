corScree <- 
function (R=mycor, 
          main=NULL, pdf=FALSE, width=5, height=5, ...) {

  if (!("matrix" %in% class(R))) { # R is a matrix, can be called indirectly
    # cor matrix:  mycor as class out_all, mycor$R, or stand-alone matrix
    cor.nm <- deparse(substitute(R))
    .cor.exists(cor.nm)  # see if matrix exists in one of the 3 locations
    if ("out_all" %in% class(R))    # R 4.0 results in two values: matrix, array
      R <- eval(parse(text=paste(cor.nm, "$R", sep="")))  # go to $R
  }
  else
    cor.nm <- "Correlation Matrix"
  
  # extract eigenvectors
  eig <- eigen(R, symmetric=TRUE, only.values=TRUE)
  ev <- eig$values

  # manage graphics
  # ---------------
  # see if graphics are to be managed (not needed for RStudio)
  manage.gr <- .graphman()

  # if manage, set up graphics system for 2 windows default
  if (!pdf) {
    if (manage.gr) {
      .graphwin(2)
      dev.set(which=3)
    }
  }
  else { 
    pdf_file <- "Scree.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

  if (getOption("theme") == "gray" ||
     (getOption("theme") == "gray"  &&  getOption("sub_theme") == "black"))
    col.ln <- getOption("bar_fill_cont")
  else
    col.ln <- getOption("bar_color_cont")

  # keep track of generated graphics
  plot.i <- 0
  plot.title  <- character(length=0)

  plot.i <- plot.i + 1
  plot.title[plot.i] <- "Eigenvalues"
  # ---------------

  # scree plot
  .lc.main(ev, type=NULL, 
         col.line=col.ln,
         col.area=NULL, col.box="black",
         col_color=getOption("pt_color"),
         col_fill=getOption("pt_fill"),
         shape_pts=21,
         col.bg=getOption("panel_fill"),
         lab_cex=getOption("lab_cex"), axis_cex=.85, col.axis="gray30",
         rotate_x=0, rotate_y=0, offset=0.5,
         xy_ticks=TRUE, line_width=1.1,
         xlab=NULL, ylab="Eigenvalues", main=main, sub=NULL, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time_start=NULL, time_by=NULL, time_reverse=FALSE,
         center_line="off", quiet=TRUE, ...)

  # manage graphics
  # ---------------
  if (pdf) {
    dev.off()
    .showfile(pdf_file, "scree chart")
  }

  if (!pdf) {
    if (manage.gr) {
      dev.set(which=4) 
    }
  }
  else { 
    pdf_file <- "ScreeDiff.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

  plot.i <- plot.i + 1
  plot.title[plot.i] <- "Differences of Successive Eigenvalues"
  # ---------------

  # differences scree plot
  ev.diff <- -diff(ev)

  .lc.main(ev.diff, type=NULL, 
         col.line=col.ln,
         col.area=NULL, col.box="black",
         col_color=getOption("pt_color"),
         col_fill=getOption("pt_fill"),
         shape_pts=21,
         col.bg=getOption("panel_fill"),
         lab_cex=getOption("lab_cex"), axis_cex=.85, col.axis="gray30",
         rotate_x=0, rotate_y=0, offset=0.5,
         xy_ticks=TRUE, line_width=1.1,
         xlab=NULL, ylab="Differences of Successive Eigenvalues",
         main=main, sub=NULL, cex=NULL,
         x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
         time_start=NULL, time_by=NULL, time_reverse=FALSE,
         center_line="off", quiet=TRUE, ...)

# does not work, maybe because using .lc.main instead of .plt.main
#  n.dregs <- 5
#  l.dregs <- length(ev.diff)
#  i.start <- l.dregs-n.dregs
#  dregs <- numeric(length=n.dregs)
#  for (i in 1:n.dregs) dregs[i] <- ev.diff[i+i.start] 
#  abline(h=mean(dregs), col="red", lwd=1)

  # manage graphics
  # ---------------
  if (pdf) {
    dev.off()
    .showfile(pdf_file, "scree difference chart")
    cat("\n\n")
  }
  # ---------------

  cat("\n")
  cat("Eigenvalues of", cor.nm, "\n")
  .dash(15+nchar(cor.nm))
  for (i in 1:length(ev)) cat(round(ev[i],3), " ")
  cat("\n")

  cat("\n")
  cat("Differences of Successive Eigenvalues of", cor.nm, "\n")
  .dash(41+nchar(cor.nm))
  for (i in 1:length(ev.diff)) cat(round(ev.diff[i],3), " ")
  cat("\n")

  if (is.null(options()$knitr.in.progress))
    .plotList(plot.i, plot.title)

  cat("\n")

}

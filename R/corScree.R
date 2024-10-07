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
  ev.df = data.frame(ev)
  x.df <- data.frame(1:nrow(ev.df))
  .plt.main(x.df, ev.df, segments=TRUE, size=.9,
            xlab="Index", ylab="Eigenvalues", main=main)

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
  ev.diff.df = data.frame(ev.diff)
  x.df <- data.frame(1:nrow(ev.diff.df))
  .plt.main(x.df, ev.diff.df, segments=TRUE, size=.9,
            xlab="Index", ylab="Differences of Successive Eigenvalues",
             main=main)

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

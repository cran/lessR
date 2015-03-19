.reg5Plot <-
function(lm.out, res.rows=NULL, pred.rows=NULL,
         scatter.coef=FALSE, scatter.3D=FALSE, X1.new=NULL,
         numeric.all, in.data.frame, c.int, p.int,
         pdf=FALSE, pdf.width=5, pdf.height=5, manage.gr=FALSE, ...) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1
  n.obs <- nrow(data)
  n.keep <- nrow(lm.out$model)
  if (is.null(pred.rows)) if (n.keep < 25) pred.rows <- n.keep else pred.rows <- 4 
  if (pred.rows == "all") pred.rows <- n.keep  # turn off preds with pred.rows=0


  # pdf graphics option
  if (pdf) { 
    pdf.file <- "RegScatterplot.pdf"
    if (n.pred > 1) pdf.file <- "RegScatterMatrix.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }

  # keep track of the plot in this routine
  plt.i <- 0
  plt.title  <- character(length=0)

  if (n.pred == 1) {  # scatterplot, if one predictor variable

    if ( (pred.rows==0) || is.factor(lm.out$model[,nm[2]])
         || !is.null(X1.new) || is.null(p.int)) 
      do.predint <- FALSE
    else 
      do.predint <- TRUE

    if (!do.predint) {
      ctitle <- "Scatterplot"
      if (!is.factor(lm.out$model[,nm[2]]))
        ctitle <- paste(ctitle, "and Regression Line")
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Regression Line,\nConfidence and Prediction Intervals"
      y.min <- min(p.int$lwr)
      y.max <- max( max(p.int$upr),  max(lm.out$model[,nm[1]]) )
    }

    if (!is.factor(lm.out$model[,nm[2]])) fl <- "ls" else fl <- "none"

    plt.i <- plt.i + 1
    plt.title[plt.i] <- gsub(pattern="\n", replacement=" ", x=ctitle)

    x.values <- lm.out$model[,nm[2]]
    y.values <- lm.out$model[,nm[1]] 
    .plt.main(x.values, y.values, by=NULL, type="p", n.cat=getOption("n.cat"),
       col.area=NULL, col.box="black",
       col.fill=getOption("col.fill.pt"),
       col.stroke=getOption("col.stroke.pt"),
       col.bg=getOption("col.bg"), col.grid=getOption("col.grid"),
       shape.pts=21, cex.axis=.85, col.axis="gray30",
       xy.ticks=TRUE,
       xlab=nm[2], ylab=nm[1], main=ctitle,
       cex=.8, kind="default", 
       x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
       fit.line=fl, col.fit.line="grey55", center.line=NULL,
       col.bubble=NULL, bubble.size=.25, col.flower=NULL,
       ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
       diag=FALSE, col.diag=par("fg"), lines.diag=TRUE,
       quiet=TRUE, ylim=c(y.min,y.max))

    if (do.predint) {
      col.ci <- getOption("col.stroke.pt")
      col.pi <- "gray30"
      lines(lm.out$model[,nm[2]], c.int$lwr, col=col.ci, lwd=0.75)
      lines(lm.out$model[,nm[2]], c.int$upr, col=col.ci, lwd=0.75)
      lines(lm.out$model[,nm[2]], p.int$lwr, col=col.pi, lwd=1.5)
      lines(lm.out$model[,nm[2]], p.int$upr, col=col.pi, lwd=1.5)
    }
  }  # end n.pred==1


  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      col.pts <- getOption("col.stroke.pt")
      col.line <- getOption("col.stroke.bar")
      col.bg=getOption("col.bg")

      panel2.smooth <- function (x, y, pch=par("pch"), cex=.9,
        col.pt=getOption("col.stroke.pt"), col.smooth=getOption("col.stroke.bar"),
        span=2/3, iter=3, ...) 
      {
          points(x, y, pch=pch, col=col.pt, cex=cex)
          ok <- is.finite(x) & is.finite(y)
          if (any(ok)) 
            lines(lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
      }

      plt.i <- plt.i + 1
      plt.title[plt.i] <- "ScatterPlot Matrix"

      if (scatter.coef) {
        panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr=c(0, 1, 0, 1))
          r <- cor(x, y)
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if (missing(cex.cor)) cex.cor <- .9/strwidth(txt)
          text(0.5, 0.5, txt, cex=2, col=col.pts)  # or cex=cex.cor * r
        }
        pairs(lm.out$model[c(nm)],
          lower.panel=panel2.smooth, upper.panel=panel.cor)
      }
      else pairs(lm.out$model[c(nm)], panel=panel2.smooth)
    }
    else {
      cat("\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
      dev.off()
    }
  }

  if (pdf) {
    dev.off()
    if (n.pred == 1)
      .showfile(pdf.file, "scatterplot")
    else
      .showfile(pdf.file, "scatterplot matrix")
    cat("\n\n")
  }

  if (scatter.3D) {  # 3d scatterplot option for 2-predictor models
    if (is.numeric(lm.out$model[,nm[2]]) && is.numeric(lm.out$model[,nm[3]]))
       proceed.3d <- TRUE
    else {
      proceed.3d <- FALSE
      cat("\n>>> No 3D scatterplot because both predictor variables must be numeric.\n")
    }
    if (proceed.3d) { 
      #check.rgl <- require(rgl, quietly=TRUE)
      cat("\n\n\n",
          "Directions for 3D scatterplot from the car package\n",
          "--------------------------------------------------\n\n",
          ">>> The 3D scatter plot requires package rgl.", "\n",
          ">>> To get the rgl package, run one time only: install.packages(\"rgl\")", "\n\n",
          "1. Can re-size the plot window, click and drag to rotate plot.\n",
          "2. Press the right mouse button and drag a rectangle around any points to be\n",
          "   identified, and then release. Repeat for each set of points to be identified.\n",
          "3. To exit, right-click in a blank area of the 3d-scatterplot.\n", sep="")
      suppressMessages(scatter3d(lm.out$terms, id.method="identify", data=lm.out$model))  # car
      cat("\n")
    }
  }

  # just generated plot
  invisible(list(i=plt.i, ttl=plt.title))

}

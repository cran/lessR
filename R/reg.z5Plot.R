.reg5Plot <-
function(nm, mydframe, my.formula, brief, res.rows,
         n.vars, n.pred, n.obs, n.keep, digits.d, explain, show.R, pre, line,
         new.data, pred.sort, pred, pred.all, scatter.3d, scatter.coef,
         numeric.all, in.data.frame, colors, X1.new, 
         X2.new, X3.new, X4.new, X5.new, c.int, p.int,
         pdf, pdf.width, pdf.height) {

  if (!pdf) 
    if (res.rows > 0) 
      dev.set(which=5) 
    else {
      .graphwin()  #  just do a scatterplot
      dev.set(which=3)
  }
  else { 
    pdf.file <- "RegScatterplot.pdf"
    if (n.pred > 1) pdf.file <- "RegScatterMatrix.pdf"
    pdf(file=pdf.file, width=pdf.width, height=pdf.height)
  }


  if (n.pred == 1) {  # scatterplot, if one predictor variable
    if ( (pred==FALSE) || is.factor(lm.out$model[,nm[2]]) || !is.null(X1.new) ) 
      do.int <- FALSE
    else 
      do.int <- TRUE
    if (!do.int) {
      ctitle <- "Scatterplot and Regression Line"
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Regression Line,\nConfidence and Prediction Intervals"
      y.min <- min(p.int$lwr)
      y.max <- max( max(p.int$upr),  max(lm.out$model[,nm[1]]) )
    }
    if (!is.factor(lm.out$model[,nm[2]])) fl <- "ls" else fl <- "none"
    x.values <- lm.out$model[,nm[2]]
    y.values <- lm.out$model[,nm[1]] 
    .plt.main(x.values, y.values, by=NULL, type="p", n.cat=getOption("n.cat"),
       col.line=NULL, col.area=NULL, col.box="black",
       col.pts=NULL, col.fill=NULL, trans.pts=NULL,
       shape.pts=21, col.grid=NULL, col.bg=NULL,
       colors=colors, 
       cex.axis=.85, col.axis="gray30",
       col.ticks="gray30", xy.ticks=TRUE,
       xlab=nm[2], ylab=nm[1], main=ctitle,
       cex=.8,    
       x.start=NULL, x.end=NULL, y.start=NULL, y.end=NULL,
       time.start=NULL, time.by=NULL, time.reverse=FALSE, kind="default",
       fit.line=fl, col.fit.line="grey55", center.line=NULL,
       col.bubble=NULL, bubble.size=.25, col.flower=NULL,
       ellipse=FALSE, col.ellipse="lightslategray", fill.ellipse=TRUE,
       text.out=FALSE, ylim=c(y.min,y.max))

    if (do.int) {
      if (colors == "gray") {
        col.ci <- "gray60"
        col.pi <- "gray30"
      }
      else {
        col.ci <- "lightsteelblue"
        col.pi <- "darksalmon"
      }
      lines(lm.out$model[,nm[2]], c.int$lwr, col=col.ci, lwd=2)
      lines(lm.out$model[,nm[2]], c.int$upr, col=col.ci, lwd=2)
      lines(lm.out$model[,nm[2]], p.int$lwr, col=col.pi, lwd=2)
      lines(lm.out$model[,nm[2]], p.int$upr, col=col.pi, lwd=2)
    }
  }
  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      if (scatter.coef) {
        panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- cor(x, y)
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if(missing(cex.cor)) cex.cor <- .9/strwidth(txt)
          text(0.5, 0.5, txt, cex=2)  # or cex = cex.cor * r
        }
        suppressWarnings(pairs(lm.out$model[c(nm)], 
          lower.panel=panel.smooth, col.smooth="grey50", upper.panel=panel.cor))
      }
      else suppressWarnings(pairs(lm.out$model[c(nm)], 
                            panel=panel.smooth, col.smooth="grey50"))
    }
    else {
      cat("\n\n>>> No scatterplot matrix reported because not all variables are ")
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

  if (scatter.3d) {  # 3d scatterplot option for 2-predictor models
    if (is.numeric(lm.out$model[,nm[2]]) && is.numeric(lm.out$model[,nm[3]]))
       proceed.3d <- TRUE
    else {
      proceed.3d <- FALSE
      cat("\n>>> No 3d scatterplot because both predictor variables must be numeric.\n")
    }
    cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", 
          "Note: As of May 2012 there is a bug in the Mac OS X implementation of the rgl\n",
           "package needed to generate this scatterplot.  So it is disabled for now.\n",
           "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", sep="")
    proceed.3d <- FALSE
    if (proceed.3d) { 
      cat("1. Click and drag to rotate plot.\n",
          "2. Press the right mouse button and drag a rectangle around any points to be\n",
          "   identified, and then release. Repeat for each set of points to be identified.\n",
          "3. To exit, right-click in a blank area of the 3d-scatterplot.\n", sep="")
      suppressMessages(scatter3d(my.formula, id.method="identify", data=lm.out$model))  # car
    }
  }

}

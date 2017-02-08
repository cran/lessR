.reg5Plot <-
function(lm.out, res.rows=NULL, pred.rows=NULL,
         scatter.coef=FALSE, X1.new=NULL,
         numeric.all, in.data.frame, c.int, p.int,
         pdf=FALSE, width=5, height=5, manage.gr=FALSE,
         scatter.3D, ...) {

         
  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)
  n.keep <- nrow(lm.out$model)
  if (is.null(pred.rows))
    pred.rows <- ifelse (n.keep < 25, n.keep, 4)
  if (pred.rows == "all") pred.rows <- n.keep  # no preds with pred.rows=0


  # pdf graphics option
  if (pdf) { 
    pdf.file <- "RegScatterplot.pdf"
    if (n.pred > 1) pdf.file <- "RegScatterMatrix.pdf"
    pdf(file=pdf.file, width=width, height=height)
  }

  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  if (n.pred <= 1) {  # scatterplot, if one predictor variable

    do.predint <- ifelse ((pred.rows==0) || !is.null(X1.new) || is.null(p.int),
      FALSE, TRUE) 
    if (n.pred > 0) if (is.factor(lm.out$model[,nm[2]])) do.predint <- FALSE

    if (!do.predint) {
      ctitle <- "Scatterplot"
      if (n.pred > 0) if (!is.factor(lm.out$model[,nm[2]]))
        ctitle <- paste(ctitle, "and Regression Line")
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Regression Line,\nConfidence and Prediction Intervals"
      y.min <- min(p.int$lwr)
      y.max <- max(max(p.int$upr),  max(lm.out$model[,nm[1]]) )
    }

    plt.i <- plt.i + 1L
    plt.title[plt.i] <- gsub(pattern="\n", replacement=" ", x=ctitle)

    if (n.pred > 0)
      x.values <- lm.out$model[,nm[2]]
    else
      x.values <- 1:n.obs
    y.values <- lm.out$model[,nm[1]]
               
    # scale for regular R or RStudio
    cex.axis <- 0.76
    bubble.scale <- 0.25
    adj <- .RSadj(bubble.scale, cex.axis)
    bubble.scale <- adj$bubble.scale
    size.axis <- adj$size.axis
    size.lab <- adj$size.lab
    cex.txt <- adj$size.txt
    
    # size of points
    size.pt <- ifelse (.Platform$OS == "windows", 1.00, 0.80)
    if (options("device") == "RStudioGD")
      size.pt <- ifelse (.Platform$OS == "windows", size.pt*1.05, size.pt*1.13)
      
    # set margins
    max.width <- strwidth(as.character(max(pretty(y.values))), units="inches")
    
    margs <- .marg(max.width, y.lab=nm[1], x.lab=nm[2], main=ctitle)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
      
    par(mai=c(bm, lm, tm, rm))
    
    plot(x.values, y.values, type="n", axes=FALSE, ann=FALSE)

    usr <- par("usr")
    col.bg <- getOption("bg")
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")

    col.grid <- getOption("grid")
    abline(v=axTicks(1), col=col.grid, lwd=.5)
    abline(h=axTicks(2), col=col.grid, lwd=.5)

    if (is.factor(x.values)) {
      x.lvl <- levels(x.values)
      axT1 <- 1:length(x.lvl)   # mark category values
    }
    else {
      x.lvl <- NULL
      axT1 <- axTicks(1)  # else numeric, so all the ticks
    }
      
    .axes(x.lvl, NULL, axT1, axTicks(2),
      par("usr")[1], par("usr")[3], cex.axis=.8, col.axis="gray30")

    .axlabs(x.lab=nm[2], y.lab=nm[1], main.lab=ctitle, sub.lab=NULL,
        max.lbl.y=3, cex.lab=size.lab) 

    col.fill <- getOption("fill.pt")
    col.stroke <- getOption("stroke.pt")
    points(x.values, y.values, pch=21, col=col.stroke, bg=col.fill, cex=size.pt)

    if (n.pred == 0) {
      m <- lm.out$coefficients[1]  # mean of Y
      mv <- rep(m, n.obs)
      names(mv) <- NULL
      lines(x.values, mv, lwd=0.75)
    }
    else {  # plot reg line
      if (!is.factor(lm.out$model[,nm[2]])) {
        abline(lm.out$coefficients[1], lm.out$coefficients[2])
      }
    }

    if (do.predint) {
      col.ci <- getOption("stroke.pt")
      col.pi <- "gray30"

      lines(x.values, c.int$lwr, col=col.ci, lwd=0.75)
      lines(x.values, c.int$upr, col=col.ci, lwd=0.75)
      lines(x.values, p.int$lwr, col=col.pi, lwd=1.5)
      lines(x.values, p.int$upr, col=col.pi, lwd=1.5)

      len <- length(x.values)
      xx <- c( c(x.values[1],x.values,x.values[len]),
                 rev(c(x.values[1],x.values,x.values[len])) )

      yy <- c( c(min(c.int$upr),c.int$upr,min(c.int$upr)),
                 rev(c(min(c.int$lwr),c.int$lwr,min(c.int$lwr))) )
      polygon(xx, yy, col=getOption("se.fill"), border="transparent")

      yy <- c( c(min(p.int$upr),p.int$upr,min(p.int$upr)),
                 rev(c(min(p.int$lwr),p.int$lwr,min(p.int$lwr))) )
      polygon(xx, yy, col=getOption("fill.ellipse"), border="transparent")


    }
  }  # end n.pred<=1

  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      col.pts <- getOption("stroke.pt")
      col.line <- getOption("stroke.bar")
      col.bg=getOption("bg")

      panel2.smooth <- function (x, y, pch=par("pch"), cex=.9,
        col.pt=col.pts, col.smooth=col.line,
        span=2/3, iter=3, ...) 
      {
          points(x, y, pch=pch, col=col.pt, cex=cex)
          ok <- is.finite(x) & is.finite(y)
          if (any(ok)) 
            lines(lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
      }

      plt.i <- plt.i + 1L
      plt.title[plt.i] <- "ScatterPlot Matrix"

      if (scatter.coef) {
        panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr=c(0, 1, 0, 1))
          r <- cor(x, y)
          txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
          if (missing(cex.cor)) cex.cor <- .9/strwidth(txt)
          cex.adj <- 2.5 - (0.18*n.pred)  # adjust size of displayed r
          text(0.5, 0.5, txt, cex=cex.adj, col=col.pts)  # or cex=cex.cor * r
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
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "scatter.3D option disabled\n",
      "car package no longer included because of dependencies issues\n\n",
      "If interested, directly call the scatter3d function from the car package\n",
      "First install the needed packages, then invoke the library function:\n",
      "  install.packages(\"rgl\", \"car\")\n",
      "  library(car)\n\n",
      "Example\n ",
      "  scatter3d(Ozone ~ Wind + Temp, id.method=\"identify\", data=airquality)\n\n",
      "Directions\n",
      "  Can re-size the plot window, click and drag to rotate plot\n",
      "  Press the right mouse button and drag a rectangle around any points to be\n",
      "    identified, and then release\n",
      "  To exit, right-click in a blank area of the 3d-scatterplot\n\n", sep="")

      #suppressMessages(scatter3d(lm.out$terms, id.method="identify", data=lm.out$model))  # car
  }

  # just generated plot
  invisible(list(i=plt.i, ttl=plt.title))

}

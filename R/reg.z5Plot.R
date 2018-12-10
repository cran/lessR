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
      ctitle <- "Reg Line, Confidence and Prediction Intervals"
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
    axis.cex <- 0.76
    radius <- 0.22
    adj <- .RSadj(radius, axis.cex, lab.cex=getOption("lab.cex"))
    radius <- adj$radius
    size.lab <- getOption("lab.cex")
    cex.txt <- getOption("axis.cex")
    
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
      
    par(bg=getOption("window.fill"))
    par(mai=c(bm, lm, tm, rm))
    
    if (!is.numeric(x.values))
      if (!is.factor(x.values)) x.values <- as.factor(x.values)

    plot(x.values, y.values, type="n", axes=FALSE, ann=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4],
      col=getOption("panel.fill"), border=getOption("panel.color"))

    abline(v=axTicks(1), col=getOption("grid.x.color"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))
    abline(h=axTicks(2), col=getOption("grid.y.color"),
         lwd=getOption("grid.lwd"), lty=getOption("grid.lty"))

    if (is.factor(x.values)) {
      x.lvl <- levels(x.values)
      axT1 <- 1:length(x.lvl)   # mark category values
    }
    else {
      x.lvl <- NULL
      axT1 <- axTicks(1)  # else numeric, so all the ticks
    }
      
    .axes(x.lvl, NULL, axT1, axTicks(2))

    .axlabs(x.lab=nm[2], y.lab=nm[1], main.lab=ctitle, sub.lab=NULL,
        max.lbl.y=3, cex.lab=size.lab) 

    col.fill <- getOption("bar.fill.ordered")
    col.color <- getOption("pt.color")
    
    eq.int <- TRUE
    if (is.numeric(x.values)) {
      d.x <- diff(x.values) 
      for (i in 2:(length(d.x)))
        if ((abs(d.x[i-1] - d.x[i]) > 0.0000000001)) eq.int <- FALSE
    }

    if (length(unique(x.values)) > getOption("n.cat")  ||  !eq.int)
      points(x.values, y.values, pch=21, col=col.color, bg=col.fill, cex=size.pt)

    else {
      mytbl <- table(x.values, y.values)  # get the counts, all x-y combinations
      n.count <- nrow(mytbl) * ncol(mytbl)
      count <- integer(length=n.count)

      # melt the table of counts to a data frame with xx, yy, count
      xx <- integer(length=n.count)
      yy <- integer(length=n.count)
      k <- 0
      for (i in 1:nrow(mytbl)) {
        for (j in 1:ncol(mytbl)) {
          if (mytbl[i,j] != 0) {  # 0 plots to a single pixel, so remove
            k <- k + 1
            count[k] <- mytbl[i,j]
            xx[k] <- as.numeric(rownames(mytbl)[i])  # rownames are factors
            yy[k] <- as.numeric(colnames(mytbl)[j])
          }
        }
      }
      cords <- data.frame(xx, yy, count)

      power <- 0.6
      sz <- cords[,3]**power  # radius unscaled 
      radius <- 0.25
      symbols(cords$xx, cords$yy, circles=sz, inches=radius,
          bg=col.fill, fg=col.color, add=TRUE, ...)

      q.ind <- 1:nrow(cords)  # all bubbles get text
      for (i in 1:nrow(cords)) if (cords[i,3] < 5) cords[i,3] <- NA 
      text(cords[q.ind,1], cords[q.ind,2], cords[q.ind,3], cex=0.8)

    }  # end bubble plot


    if (n.pred == 0) {
      m <- lm.out$coefficients[1]  # mean of Y
      mv <- rep(m, n.obs)
      names(mv) <- NULL
      lines(x.values, mv, lwd=0.75)
    }
    else {  # plot reg line
      if (!is.factor(lm.out$model[,nm[2]])) {
        abline(lm.out$coefficients[1], lm.out$coefficients[2],
               col=getOption("segment.color"), lwd=1)
      }
    }

    if (do.predint) {
      col.ci <- getOption("segment.color")
      col.pi <- "gray30"

      lines(x.values, c.int$lwr, col=col.ci, lwd=0.75)
      lines(x.values, c.int$upr, col=col.ci, lwd=0.75)
      lines(x.values, p.int$lwr, col=col.pi, lwd=1)
      lines(x.values, p.int$upr, col=col.pi, lwd=1)

      len <- length(x.values)
      xx <- c( c(x.values[1],x.values,x.values[len]),
                 rev(c(x.values[1],x.values,x.values[len])) )

      yy <- c( c(min(c.int$upr),c.int$upr,min(c.int$upr)),
                 rev(c(min(c.int$lwr),c.int$lwr,min(c.int$lwr))) )
      polygon(xx, yy, col=getOption("se.fill"), border="transparent")

      yy <- c( c(min(p.int$upr),p.int$upr,min(p.int$upr)),
                 rev(c(min(p.int$lwr),p.int$lwr,min(p.int$lwr))) )
      polygon(xx, yy, col=getOption("ellipse.fill"), border="transparent")


    }
  }  # end n.pred<=1

  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      plt.i <- plt.i + 1L
      plt.title[plt.i] <- "ScatterPlot Matrix"

      panel.fill <- getOption("panel.fill")  
      window.fill <- getOption("window.fill")  
      bckg <- ifelse(panel.fill=="transparent", window.fill, panel.fill)
      .plt.mat(lm.out$model[c(nm)], fit="lm", col.bg=bckg)
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

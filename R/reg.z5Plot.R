.reg5Plot <-
function(lm.out, res_rows=NULL, pred_rows=NULL,
         scatter_coef=FALSE, X1_new=NULL,
         numeric.all, in.data.frame, c.int, p.int,
         pdf=FALSE, width=5, height=5, manage.gr=FALSE,
         scatter_3D, ...) {

         
  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)
  n.keep <- nrow(lm.out$model)
  if (is.null(pred_rows))
    pred_rows <- ifelse (n.keep < 25, n.keep, 4)
  if (pred_rows == "all") pred_rows <- n.keep  # no preds with pred_rows=0


  # pdf graphics option
  if (pdf) { 
    pdf_file <- "RegScatterplot.pdf"
    if (n.pred > 1) pdf_file <- "RegScatterMatrix.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  if (n.pred <= 1) {  # scatterplot, if one predictor variable

    if (n.pred > 0)
      x.values <- lm.out$model[,nm[2]]
    else
      x.values <- 1:n.obs
    y.values <- lm.out$model[,nm[1]]

    do.predint <- ifelse ((pred_rows==0) || !is.null(X1_new) || is.null(p.int),
      FALSE, TRUE) 
    if (n.pred > 0) if (is.factor(lm.out$model[,nm[2]])) do.predint <- FALSE

    if (!do.predint  ||  !is.numeric(x.values)) {
      ctitle <- "Scatterplot"
      if (n.pred > 0  &&  is.numeric(x.values))
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
               
    # scale for regular R or RStudio
    axis_cex <- 0.76
    radius <- 0.22
    adj <- .RSadj(radius, axis_cex, lab_cex=getOption("lab_cex"))
    radius <- adj$radius
    size.lab <- getOption("lab_cex")
    cex.txt <- getOption("axis_cex")
    
    # size of points
    size.pt <- ifelse (.Platform$OS == "windows", 0.85, 0.70)
#   if (options("device") == "RStudioGD")
#     size.pt <- ifelse (.Platform$OS == "windows", size.pt*0.9, size.pt*0.9)
      
    # set margins
    max.width <- strwidth(as.character(max(pretty(y.values))), units="inches")
    
    margs <- .marg(max.width, y.lab=nm[1], x.lab=nm[2], main=ctitle)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm

    lm <- lm + 0.08
      
    par(bg=getOption("window_fill"))
    par(mai=c(bm, lm, tm, rm))
    
    if (!is.numeric(x.values))
      if (!is.factor(x.values)) x.values <- as.factor(x.values)

    plot(x.values, y.values, type="n", axes=FALSE, ann=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4],
      col=getOption("panel_fill"), border=getOption("panel_color"))

    abline(v=axTicks(1), col=getOption("grid_x_color"),
         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))
    abline(h=axTicks(2), col=getOption("grid_y_color"),
         lwd=getOption("grid_lwd"), lty=getOption("grid_lty"))
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

    col_fill <- getOption("pt_fill")
    col_color <- getOption("pt_color")
    
    eq.int <- TRUE
    if (is.numeric(x.values)) {
      d.x <- diff(x.values) 
      for (i in 2:(length(d.x)))
        if ((abs(d.x[i-1] - d.x[i]) > 1.0000000001)) eq.int <- FALSE
    }
    # getOption("n_cat") = 1
    ux <- length(unique(x.values))
    uy <- length(unique(y.values))
    if ( (ux > 10  &&  uy > 10)  ||  !eq.int  || !is.numeric(x.values)) {
      points(x.values, y.values, pch=21, col=col_color, bg=col_fill,
             cex=size.pt)
    }
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
          bg=.maketrans(col_fill, 110), fg=col_color, add=TRUE, ...)

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
      if (!is.factor(x.values)) {
        abline(lm.out$coefficients[1], lm.out$coefficients[2],
               col=getOption("segment_color"), lwd=1)
      }
    }

    if (!is.factor(x.values)  &&  do.predint) {
      col.ci <- getOption("segment_color")
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
      polygon(xx, yy, col=getOption("se_fill"), border="transparent")

      yy <- c( c(min(p.int$upr),p.int$upr,min(p.int$upr)),
                 rev(c(min(p.int$lwr),p.int$lwr,min(p.int$lwr))) )
      polygon(xx, yy, col=getOption("ellipse_fill"), border="transparent")


    }
  }  # end n.pred<=1

  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      plt.i <- plt.i + 1L
      plt.title[plt.i] <- "ScatterPlot Matrix"

      panel_fill <- getOption("panel_fill")  
      window_fill <- getOption("window_fill")  
      bckg <- ifelse(panel_fill=="transparent", window_fill, panel_fill)
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
      .showfile(pdf_file, "scatterplot")
    else
      .showfile(pdf_file, "scatterplot matrix")
    cat("\n\n")
  }

  if (scatter_3D) {  # 3d scatterplot option for 2-predictor models
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "scatter_3D option disabled\n",
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

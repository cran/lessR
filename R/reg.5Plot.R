.reg5Plot <-
function(lm.out, n_res_rows=NULL, n_pred_rows=NULL,
         scatter_coef=FALSE, X1_new=NULL, ancova,
         numeric.all, in.data.frame, c.int, p.int, plot_errors=FALSE,
         digits_d, n_cat, pdf=FALSE, width=5, height=5, manage.gr=FALSE,
         quiet, ...) {
         
  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  n.obs <- nrow(lm.out$model)
  n.keep <- nrow(lm.out$model)
  b0 <- lm.out$coefficients[1]
  b1 <- lm.out$coefficients[2]
  if (is.null(n_pred_rows))
    n_pred_rows <- ifelse (n.keep < 25, n.keep, 4)
  if (n_pred_rows == "all") n_pred_rows <- n.keep  # no preds with n_pred_rows=0

  # pdf graphics option
  if (pdf) { 
    pdf_file <- "RegScatterplot.pdf"
    if (n.pred > 1) pdf_file <- "RegScatterMatrix.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }
  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  x.cat <- 0
  x.cont <- 0
  do.sp <- ifelse (n.pred < 2, TRUE, FALSE) 
  if (ancova) {
    if (is.numeric(lm.out$model[,nm[2]]) && is.factor(lm.out$model[,nm[3]])) {
      x.cat <- 3
      x.cont <- 2
      do.sp <- TRUE
    }
    if (is.numeric(lm.out$model[,nm[3]]) && is.factor(lm.out$model[,nm[2]])) {
      x.cat <- 2
      x.cont <- 3
      do.sp <- TRUE
    }
    plot_errors <- FALSE
    lvl <- levels(lm.out$model[,nm[x.cat]])
  }

  txeqs <- NULL

  # ----------------------------------------------------
  # scatterplot, if one (or no) pred variables or ancova
  if (do.sp) {

    if (n.pred %in% 1:2) {
      if (!ancova)
        x.values <- lm.out$model[,nm[2]]
      else
        x.values <- lm.out$model[,nm[x.cont]]
    }
    else if (n.pred == 0) {  # null model
      x.values <- 1:n.obs
      nm[2] <- "Index"
      x.lab <- nm[2] 
    }
    y.values <- lm.out$model[,nm[1]]

    do.predint <- ifelse (n_pred_rows==0 || !is.null(X1_new) || is.null(p.int)
      || ancova, FALSE, TRUE) 
    if (n.pred > 0)
      if (is.factor(lm.out$model[,nm[2]])) do.predint <- FALSE
 
    # title
    if (!do.predint || !is.numeric(x.values)) {
      ctitle <- "Scatterplot"
      if (is.numeric(x.values)) {
        if (n.pred == 0)
          ctitle <- paste(ctitle, "and Null Model")
        else
          ctitle <- paste(ctitle, "and Least-Squares Line")
        if (ancova)
          ctitle <- paste(ctitle, "s", sep="")
      }
      else if (is.factor(x.values) && n.pred==1 && nlevels(x.values)==2) {
        ctitle <- paste(ctitle, "and Least-Squares Line")
      }
      y.min <- min(lm.out$model[,nm[1]])
      y.max <- max(lm.out$model[,nm[1]])
    }
    else {
      ctitle <- "Reg Line, Confidence & Prediction Intervals"
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

    # set margins
    max.width <- strwidth(as.character(max(pretty(y.values))), units="inches")

    margs <- .plt.marg(max.width, y.lab=nm[1], x.lab=nm[2], main=NULL, sub=NULL)
    lm <- margs$lm
    tm <- margs$tm
    rm <- margs$rm
    bm <- margs$bm
    
    if (ancova) {
      big.nm <- max(nchar(lvl))
      if (big.nm > 6) rm <- rm + (.05 * (big.nm - 6))
      rm <- rm + .30  + (.65 * getOption("axis_cex"))  # better if axis_y_cex
    }

    par(bg=getOption("window_fill"))
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mai=c(bm, lm, tm, rm))

    plot(x.values, y.values, type="n", axes=FALSE, ann=FALSE)
    usr <- par("usr")

    if (is.factor(x.values)) {
      x.lvl <- levels(x.values)
      axT1 <- 1:length(x.lvl)   # mark category values
    }
    else {
      x.lvl <- NULL
      axT1 <- axTicks(1)  # else numeric, so all the ticks
    }

    .plt.bck(usr, axT1, axTicks(2))
      
    .axes(x.lvl, NULL, axT1, axTicks(2))

    theme <- getOption("theme")
    if (!ancova) {
      .axlabs(x.lab=nm[2], y.lab=nm[1], main.lab=NULL, sub.lab=NULL)
      fill <- getOption("pt_fill")
      color <- getOption("pt_color")
    }
    else {
      .axlabs(x.lab=nm[x.cont], y.lab=nm[1], main.lab=NULL, sub.lab=NULL)
      clr <- .get_fill(theme)
      fill <- getColors(clr, n=length(lvl))
      color <- getColors(clr, n=length(lvl))
    }

    # Plot legend for ancova
    if (ancova) {
      pts_trans <- 0
      shp <- 21
      fill_bg <- "transparent"
      options(byname = nm[x.cat])
      .plt.by.legend(lvl, color, fill, shp, pts_trans, fill_bg, usr)
    }
    
    # Plot points
    # -----------
    ux <- length(unique(x.values))
    uy <- length(unique(y.values))
    n_cat <- 10
    discrete <- ifelse (ux>n_cat && uy>n_cat || !.is.integer(x.values) ||
                        !.is.integer(y.values), FALSE, TRUE)

    if (!discrete)  {
      n.iter <- ifelse(ancova, nlevels(lm.out$model[,nm[x.cat]]), 1)

      for (i in 1:n.iter) {  # iter > 1 only for ancova, levels of cat var
        if (!ancova)
          ind <- 1:length(x.values)
        else
          ind <- which(lm.out$model[,nm[x.cat]] == lvl[i])

        points(x.values[ind], y.values[ind],
               pch=21, col=color[i], bg=fill[i], cex=size.pt)
      }  # end 1:n.iter

    }  # end !discrete

    if (discrete) {  # bubble plot, not for ancova
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
      radius <- 0.18
      symbols(cords$xx, cords$yy, circles=sz, inches=radius,
          bg=.maketrans(fill, 110), fg=color, add=TRUE, ...)

      q.ind <- 1:nrow(cords)  # all bubbles get text
      for (i in 1:nrow(cords)) if (cords[i,3] < 5) cords[i,3] <- NA 
      text(cords[q.ind,1], cords[q.ind,2], cords[q.ind,3], cex=0.8)
    }  # end bubble plot


    # Plot Line
    # ---------

    if (n.pred == 0) {
      m <- lm.out$coefficients[1]  # mean of Y
      mv <- rep(m, n.obs)
      names(mv) <- NULL
      lines(x.values, mv, lwd=0.75)
    }

    else if (n.pred == 1) {
      if (!is.factor(x.values)) {
        abline(b0, b1, col=getOption("segment_color"), lwd=1)
      }
      else if (nlevels(x.values)==2) {
        y0 <- b0 + (b1*0)
        y1 <- b0 + (b1*1)
        abline(v=1, col="gray60", lwd=.5) 
        abline(v=2, col="gray60", lwd=.5) 
        abline(h=y0, col="gray60", lwd=.5) 
        abline(h=y1, col="gray60", lwd=.5) 
        segments(y0=y0, y1=y1, x0=1, x1=2, col="black", lwd=1.5) 
      }
    }

    else if (ancova) {
      coefs <- lm.out$coefficients
      n.lvl <- nlevels(lm.out$model[,nm[x.cat]])
      b.cont <- ifelse (x.cont == 2, coefs[2], coefs[1+n.lvl])
      if (x.cat == 2)
        b.cat <- coefs[2:n.lvl] 
      else
        b.cat <- coefs[3:(1+(n.lvl))] 

      tx <- character(length = 0)
      for (i.coef in 0:length(b.cat)) {
        if (i.coef == 0)
          b00 <- b0
        else
          b00 <- b0 + b.cat[i.coef] 
        abline(b00, b.cont, col=fill[i.coef+1], lwd=1.5)
        tx[length(tx)+1] <- paste("Level ",lvl[i.coef+1], ": y^_", nm[1],
            " = ", .fmt(b00, digits_d), " + ", .fmt(b.cont, digits_d),
            "(x_", nm[x.cont], ")", sep="")
      }
      txeqs <- tx
    }

    # Plot Errors
    # -----------
    if (plot_errors)  {
      theme <- getOption("theme")
      red <- rgb(130,40,35, maxColorValue=255) 
      pe.clr <- ifelse (theme %in% c("gray", "white"), "gray58", red)
      segments(y0=lm.out$fitted.values, y1=lm.out$model[,1],
               x0=x.values, x1=x.values, col=pe.clr, lwd=1) 
    }

    # Plot Intervals
    # --------------
    if (!is.factor(x.values) && do.predint) {
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

  }  # end do.sp, a single scatterplot

  else {  # scatterplot matrix for multiple regression
    if (numeric.all && in.data.frame) {
      plt.i <- plt.i + 1L
      plt.title[plt.i] <- "ScatterPlot Matrix"

      panel_fill <- getOption("panel_fill")  
      window_fill <- getOption("window_fill")  
      bckg <- ifelse(panel_fill=="transparent", window_fill, panel_fill)
      .plt.mat(lm.out$model[c(nm)], fit="lm", col.bg=bckg, 
               pt.size=TRUE, size.miss=TRUE)
    }
    else if (!quiet) {
      cat("\n>>> No scatterplot matrix reported because not all variables are ")
      if (!in.data.frame) cat("in the data frame.\n")
      if (!numeric.all) cat("numeric.\n")
      if (dev.cur() > 1) dev.off()  # 1 is the null device
    }
  }

  if (pdf) {
    if (dev.cur() > 1) {
      dev.off()
      if (n.pred==1 || ancova)
        .showfile(pdf_file, "scatterplot")
      else
        .showfile(pdf_file, "scatterplot matrix")
      cat("\n\n")
    }
  }

  # just generated plot
  return(invisible(list(i=plt.i, ttl=plt.title, txeqs=txeqs,
                        cat=nm[x.cat], cont=nm[x.cont])))

}

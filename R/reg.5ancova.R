.reg5ancova <-
function(lm.out, d.ancova, digits_d, 
         pdf=FALSE, width=5, height=5, manage.gr=FALSE, quiet, ...) {
         
  nm <- all.vars(lm.out$terms)  # names of vars in the model
  nm.a <- names(d.ancova)
  n.vars <- length(nm)
  n.pred <- n.vars - 1L
  b0 <- lm.out$coefficients[1]

  # pdf graphics option
  if (pdf) { 
    pdf_file <- "RegScatterplot.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }
  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  # identify the grouping variable and the covariate
  if (is.numeric(d.ancova[,nm.a[2]]) && is.factor(d.ancova[,nm.a[3]])) {
    x.cat <- 3
    x.cont <- 2
  }
  if (is.numeric(d.ancova[,nm.a[3]]) && is.factor(d.ancova[,nm.a[2]])) {
    x.cat <- 2
    x.cont <- 3
  }
  lvl <- levels(d.ancova[,nm.a[x.cat]])


  # Set up plot
  # -----------
  x.values <- d.ancova[,nm.a[x.cont]]
  y.values <- d.ancova[,nm.a[1]]
  y.min <- min(y.values)
  y.max <- max(y.values)

  plt.i <- plt.i + 1L
  plt.title[plt.i] <- "Scatterplot and Least-Squares Lines"
             
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
  lm <- margs$lm;  tm <- margs$tm;  rm <- margs$rm;  bm <- margs$bm
  
  big.nm <- max(nchar(lvl))
  if (big.nm > 6) rm <- rm + (.05 * (big.nm - 6))
  rm <- rm + .30  + (.65 * getOption("axis_cex"))  # better if axis_y_cex

  par(bg=getOption("window_fill"))
  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  par(mai=c(bm, lm, tm, rm))

  plot(x.values, y.values, type="n", axes=FALSE, ann=FALSE)
  usr <- par("usr")

  axT1 <- axTicks(1)  # numeric, so all the ticks
  .plt.bck(usr, axT1, axTicks(2))
  .axes(NULL, NULL, axT1, axTicks(2))

  theme <- getOption("theme")
  .axlabs(x.lab=nm[x.cont], y.lab=nm[1], main.lab=NULL, sub.lab=NULL)
  clr <- .get_fill(theme)
  fill <- getColors(clr, n=length(lvl))
  color <- getColors(clr, n=length(lvl))

  # Plot legend
  # -----------
  pts_trans <- 0
  shp <- 21
  fill_bg <- "transparent"
  options(byname = nm.a[x.cat])
  .plt.by.legend(lvl, color, fill, shp, pts_trans, fill_bg, usr)
    
  # Plot points
  # -----------
  ux <- length(unique(x.values))
  uy <- length(unique(y.values))

  n.iter <- nlevels(d.ancova[,nm.a[x.cat]])

  for (i in 1:n.iter) {
    ind <- which(d.ancova[,nm.a[x.cat]] == lvl[i])

    points(x.values[ind], y.values[ind],
           pch=21, col=color[i], bg=fill[i], cex=size.pt)
  }  # end 1:n.iter

  # Plot Lines
  # ----------
  coefs <- lm.out$coefficients
  n.lvl <- nlevels(d.ancova[,nm.a[x.cat]])
  b.slope <- ifelse (x.cont == 2, coefs[2], coefs[1+n.lvl])
  if (x.cat == 2)
    b.cat <- coefs[2:n.lvl] 
  else
    b.cat <- coefs[3:(1+(n.lvl))] 

  # also gather separate equation for each line
  tx <- character(length = 0)
  for (i.coef in 0:length(b.cat)) {
    if (i.coef == 0)
      b00 <- b0  # y-intercept of the model
    else
      b00 <- b0 + b.cat[i.coef]  # intercept of one reg line
    abline(b00, b.slope, col=fill[i.coef+1], lwd=1.5)
    tx[length(tx)+1] <- paste("Level ",lvl[i.coef+1], ": y^_", nm.a[1],
        " = ", .fmt(b00, digits_d), " + ", .fmt(b.slope, digits_d),
        "(x_", nm.a[x.cont], ")", sep="")
  }
  txeqs <- tx

  if (pdf) {
    if (dev.cur() > 1) {
      dev.off()
      .showfile(pdf_file, "scatterplot")
      cat("\n\n")
    }
  }

  # Generate ancova text output
  # ---------------------------
  tx <- character(length = 0)
  title_eqs <- paste("\n  MODELS OF", nm[1], "FOR LEVELS OF", nm.a[x.cat])

  # test interaction
  tx[1] <- "-- Test of Interaction"
  tx[length(tx)+1] <- " "

  nm.a <- names(d.ancova)
  lm2.out <- lm(d.ancova[,nm.a[1]] ~ 
                d.ancova[,nm.a[2]] * d.ancova[,nm.a[3]])
  a.tbl <- anova(lm2.out)
  a <- round(a.tbl[3,], 3)  # 3rd row is interaction row
  a.int <- paste(nm.a[2], ":", nm.a[3],
                 "  df: ", a[1,1], "  df resid: ", a.tbl[nrow(a.tbl),1],
                 "  SS: ", a[1,2], "  F: ",  a[1,4], "  p-value: ", a[1,5],
                 sep="") 

  # generate the equation for each group
  tx[length(tx)+1] <- a.int 
  tx[length(tx)+1] <- " "
  tx[length(tx)+1] <- paste(
      "-- Assume parallel lines, no interaction of", nm.a[x.cat], "with",
      nm.a[x.cont], "\n")
  n.levels <- length(unique(na.omit(d.ancova[,nm.a[x.cat]])))
  for (i.level in 1:n.levels)
    tx[length(tx)+1] <- txeqs[i.level]

  tx[length(tx)+1] <- " "
  tx[length(tx)+1] <- "-- Visualize Separately Computed Regression Lines"
  tx[length(tx)+1] <- paste("\n", "Plot(", nm.a[x.cont], ", ", nm[1],
       ", by=", nm.a[x.cat], ", fit=\"lm\")", sep="")   
  txmodels <- tx


  return(invisible(list(i=plt.i, ttl=plt.title, txmdl=txmodels)))
}

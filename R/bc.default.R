bc.default <- 
function(x, by=NULL, 

         col.bars=NULL, col.border="black", col.bg="ghostwhite",
         col.grid="grey86", random.col=FALSE,
         colors=c("relaxed", "vivid", "gray", "rainbow", "terrain", "heat"),

         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, brief=FALSE, prop=FALSE,
         
         xlab=NULL, ylab=NULL, main=NULL, mag.axis=.85,

         beside=FALSE, col.low=NULL, col.hi=NULL, count.names=NULL,

         legend.title=NULL, legend.loc=NULL, legend.labels=NULL,
         legend.horiz=FALSE,
         
         chisq=NULL, graph=TRUE, ...) {
 
  dash <- function(n.dash) { for (i in 1:(n.dash)) cat("-"); cat("\n") }

  colors <- match.arg(colors)

  if (is.null(chisq)) if (brief || prop) chisq <- FALSE else chisq <- TRUE

  if (chisq && prop) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Chi-square analysis here not valid for proportions.\n\n")
  }

  if (!is.null(by) && prop) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Analysis of proportions not valid for two variables.\n\n")
  }

  if (horiz && addtop!=1) { 
    cat("\n"); warning(call.=FALSE, "\n","------\n",
    "addtop  only works for a vertical bar plot.\n\n")
  }

  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))

  # use variable label if it exists
  x.lbl <- ""
  y.lbl <- ""
  if (exists("mylabels")) {
    x.lbl <- as.character(mylabels[which(row.names(mylabels)==x.name), "label"])
    if (length(x.lbl) == 0) x.lbl <- "" 
    if (!is.null(by)) {
      y.lbl <- as.character(mylabels[which(row.names(mylabels)==y.name), "label"])
      if (length(y.lbl) == 0) y.lbl <- ""
    }
  }

  # axis and legend labels
  if (!is.null(xlab)) x.lab <- xlab else x.lab <- x.name
  if (is.null(ylab)) if (!prop) y.lab <- "Frequency" else y.lab <- "Proportion"
    else y.lab <- ylab

  if (!is.null(legend.title)) l.lab <- legend.title 
  else if (!is.null(by)) l.lab <- y.name else l.lab=NULL

  # title
  if (!is.null(main)) main.lbl <- main 
  else {
    main.lbl <- x.lbl
    if (!is.null(by)) if (exists("y.lbl"))
      if (nchar(y.lbl) > 0) main.lbl <- paste(x.lbl, "\nBY\n", y.lbl, sep="")
  }

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # count.names is getting counts directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  if (!is.integer(x) && is.double(x) && entered.pre) 
    entered <- TRUE else entered <- FALSE
  if (!is.null(count.names)) {
    x <- as.numeric(x)
    names(x) <- count.names
    entered <- TRUE
  }

  if (is.null(by) && beside && !entered) { 
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }
  
  # save ordered status before converting x to a table
  if (is.ordered(x) && is.null(by)) order.x <- TRUE else order.x <- FALSE
  if (is.ordered(by)) order.y <- TRUE else order.y <- FALSE

  # convert to table, with variable names, if needed
  if (!entered && !is.table(x))
    if (!is.null(by)) x <- table(by,x, dnn=c(l.lab,x.lab)) 
    else {  
      x <- table(x, dnn=NULL)
      if (prop) x <- x/sum(x)
    }

  if (graph) {  
    # get number of colors
    if (is.null(by) && !order.x && !is.matrix(x)) ncolors <- 1 else ncolors <- nrow(x)
    if (!is.null(by) && order.y) ncolors <- nrow(x)

    if ( (colors == "rainbow"  ||  colors=="terrain"  || colors=="heat") ) {
      nogo <- FALSE
      if (ncolors == 1) nogo <- TRUE
      if (is.ordered(x) && is.null(by)) nogo <- TRUE
      if (is.ordered(by)) nogo <- TRUE
      if (nogo) {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Can only do an R color range when there is more than one color. Choose a\n",
        "built-in lessR range for colors of relaxed (default), vivid or gray.\n\n")
      }
    }

    # color palette
  if ((order.x && is.null(by)) || order.y) {
      if (colors == "relaxed") { 
        if (is.null(col.low)) col.low <- "slategray2"
        if (is.null(col.hi)) col.hi <- "slategray4"
      }
      else if (colors == "vivid") { 
        if (is.null(col.low)) col.low <- "coral1"
        if (is.null(col.hi)) col.hi <- "coral4"
      }
      else if (colors == "gray") {
        if (is.null(col.low)) col.low <- "gray90"
        if (is.null(col.hi)) col.hi <- "gray30"
      }
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(ncolors)
    }
    else {
      if (colors == "relaxed")
        clr <- c("slategray", "peachpuff2", "darksalmon", "darkseagreen1", 
          "thistle4", "azure3", "mistyrose")
      else if (colors == "vivid") {
        clr <- c("coral3", "seagreen3", "maroon3", "dodgerblue3", "purple3", 
          "turquoise3", "yellow3")
        if (col.bg == "seashell") col.bg <- "cornsilk1"
      }
      else if (colors == "gray") {
        color.palette <- colorRampPalette(c("gray30","gray90"))
        clr <- color.palette(nrow(x))
      }
      else if (colors == "rainbow") clr <- rainbow(ncolors)
      else if (colors == "terrain") clr <- terrain.colors(ncolors)
      else if (colors == "heat") clr <- heat.colors(ncolors)
      if (random.col) clr <- clr[sample(length(clr))]
    }
    if (!is.null(col.bars)) {
      for (i in 1:(min(length(col.bars),length(clr)))) clr[i] <- col.bars[i]
      ncolors <- min(length(col.bars),length(clr))
    }
    palette(clr)
    col <- 1:ncolors 
   
    if (is.matrix(x) && !beside) max.y <- max(colSums(x)) else max.y <- max(x)
    if (prop) addtop <- .01
    max.y <- max.y + addtop

    if (is.null(legend.labels)) legend.labels <- row.names(x)
    if (!is.null(legend.labels)) if (is.null(legend.loc)) legend.loc <- "topleft"
    if (beside) legend.horiz <- FALSE
    if ((!is.null(by) || is.matrix(x)) && !beside) {
      legend.horiz <- TRUE
      if (is.null(legend.loc)) legend.loc <- "top"
      max.y <- max.y + .18*max.y
    }

    if (is.null(gap)) if (is.matrix(x) && beside) gap <- c(0.1,1) else gap <- 0.2

     # get max label size
    the.names <- integer(length=0)
    if (length(dim(x)) == 0) the.names <- names(x) else the.names <- rownames(x)
    max.nm <- 0
    for (i in (1:length(the.names))) {
      li <- nchar(the.names[i])
      if (li > max.nm) max.nm <- li
    }

    # extend the left margin to accommodate horizontal labels
    extend <- FALSE
    if (horiz && max.nm>5) {
      add.left <- max.nm/2.0
      if (y.lab != "") add.left <- add.left + 1.5
      extend <- TRUE
      par(mar=c(5, add.left, 4, 2) + 0.1)
    } 

    # set up plot area, color background, grid lines
    if(is.null(count.names))  if (horiz) { temp <- x.lab; x.lab <- y.lab; y.lab <- temp }
 
    if (class(x) == "numeric"  &&  entered) x <- as.table(x)
    rescale <- 0
    if (is.null(by)) if (nrow(x) <=4) rescale <- nrow(x)
    if (!is.null(by) && !beside) if (ncol(x) <=4) rescale <- ncol(x)
    if (class(x) == "matrix" && entered) rescale <- 0  # turned off for now

    # set rescale to control bar width for small number of bars:w
    if (rescale == 0) {
      if (!horiz)
        barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
          beside=beside, space=gap, axes=FALSE, ...)
      else
        barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
          beside=beside, space=gap, axes=FALSE, ...)
    }
    else {
      if (rescale == 4) width.bars <- .17
      if (rescale == 3) width.bars <- .22
      if (rescale == 2) width.bars <- .28
      gap <- 0.246 + 0.687*width.bars
      if (!horiz)
        barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
          beside=beside, space=gap, width=width.bars, xlim=c(0,1), axes=FALSE, ...)
      else
        barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
          beside=beside, space=gap, width=width.bars, ylim=c(0,1), axes=FALSE, ...)
    }

    if (extend) {
      mtext(y.lab, side=2, line=add.left-1)
      y.lab <- ""
      las.value <- 1
    }
    else las.value <- 0

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
    if (max.y > 1) vy <- pretty(0:max.y) else vy <- pretty(1:100*max.y)/100
    
    # bar plot, grid lines and legend
    if (!over.grid) {
      if (!horiz) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
      else abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    }

    if (rescale == 0) {
       #width.bars <- .8
       #gap <- .6*width.bars
       barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz, xlab=x.lab,
            ylab=y.lab, main=main.lbl, border=col.border, las=las.value, 
            space=gap, cex.axis=mag.axis, cex.names=mag.axis, ...)
    }
    else
       barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz, xlab=x.lab,
            ylab=y.lab, main=main.lbl, border=col.border, las=las.value, 
            space=gap, width=width.bars, xlim=c(0,1), 
            cex.axis=mag.axis, cex.names=mag.axis, ...)
    if (over.grid) {
      if (!horiz) abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
      else abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    }
    if ((!is.null(by) || is.matrix(x)) && !is.null(legend.loc)) 
      legend(legend.loc, legend=legend.labels, title=l.lab, fill=col, 
             horiz=legend.horiz, cex=.7, bty="n")

  } # end graph  


  # print table, chi-square analysis
  txt1 <- x.name
  if (nchar(x.lbl) > 0) txt1 <- paste(txt1, ", ", x.lbl, sep="")
  if (is.null(by)) txt1 <- paste("---", txt1, "---")
  if (!is.null(by)) {
    txt2 <- paste("  by\n", l.lab, sep="")
    if (nchar(y.lbl) > 0) {
      txt2 <- paste(txt2, ", ", y.lbl, sep="") 
    }
  }
  if (nchar(x.lbl) > 0  ||  (is.null(by))) {
    cat("\n")
    cat(txt1, "\n")
    if (!is.null(by)) cat(txt2, "\n")
  }
  cat("\n")

  if (!is.null(by) || is.matrix(x)) {  # two variables
    dash(30); cat("Joint and Marginal Frequencies\n"); dash(30); 
    print(addmargins(x))
    if (chisq) { 
      cat("\n"); dash(19); cat("Chi-square Analysis\n"); dash(19); 
      ch <- (summary(as.table(x)))
      pvalue <- format(sprintf("%6.4f", ch$p.value), justify="right")
      cat("Number of observations (cases) in analysis:", ch$n.cases, "\n")
      cat("Number of variables:", ch$n.vars, "\n")
      cat("Test of independence: ", 
          "  Chisq = ", ch$statistic, ", df = ", ch$parameter, ", p-value = ", 
          pvalue, sep="", "\n")
    if (!ch$approx.ok) 
      cat(">>> Low cell expected frequencies,",
          "so chi-squared approximation may not be accurate", "\n")
    }
    if (!brief) {
      cat("\n\n"); dash(30); cat("Cell Proportions and Marginals\n"); dash(30); 
        print(round(addmargins(prop.table(x)),3))
        cat("\n"); dash(30); cat("Proportions within Each Column\n"); dash(30);
        x.col <- prop.table(x, margin=2)
        Sum <- numeric(ncol(x.col))
        for (i in 1:ncol(x.col)) Sum[i] <- sum(x.col[,i])
        x.col2 <- round(rbind(x.col,Sum),3)
        names(dimnames(x.col2)) <- names(dimnames(x.col))
        print(x.col2)
      cat("\n"); dash(27); cat("Proportions within Each Row\n"); dash(27); 
        x.row <- prop.table(x, margin=1)
        Sum <- numeric(nrow(x.row))
        for (i in 1:nrow(x.row)) Sum[i] <- sum(x.row[i,])
        x.row2 <- round(cbind(x.row,Sum),3)
        names(dimnames(x.row2)) <- names(dimnames(x.row))
        print(x.row2)
    }
  }
  else {  # one variable
    proceed <- TRUE
    if ( length(x) > 10  &&  length(names(x)) < sum(x) ) {
      proceed <- FALSE
      print(x)
    }
    if (length(names(x)) == sum(x)) {
      proceed <- FALSE
      cat("\nAll values are unique.  Probably a row ID instead of a variable.\n",
          "Perhaps use  row.names  option when reading. See help(read.table).\n\n", sep="")
      if (sum(x) < 100) print(names(x))
      else cat("\nOnly the first 100 values listed.  To see all, use\n",
               "the  values  function.\n\n")
    }
    if (proceed) {
      max.ln <- integer(length=0)
      for (i in 1:length(x)) {
        ln.nm <- nchar(names(x[i]))
        ln.vl <- nchar(as.character(x[i]))
        if (!prop) max.ln[i] <- max(ln.nm, ln.vl) + 1 else max.ln[i] <- ln.nm+1
        if (max.ln[i] < 6) max.ln[i] <- 6
      }
      cat("             ")
      w <- nchar(as.character(sum(x)))
      for (i in 1:length(x)) 
        cat(format(names(x[i]), width=max.ln[i], justify="right", sep=""))
      cat(format("Total", width=w+6, justify="right", sep=""))
      cat("\n")
      if (!prop) {
        cat("Frequencies: ")
        for (i in 1:length(x))
          cat(format(sprintf("%i", x[i]), width=max.ln[i], justify="right"))
        cat(format(sum(x), width=w+6, justify="right", sep=""))
        cat("\n")
      }
      cat("Proportions: ")
      for (i in 1:length(x)) {
        if (!prop) xdiv <- sum(x) else xdiv <- 1
        cat(format(sprintf("%*.3f",max.ln[i], round(x[i]/xdiv,3)), justify="right"))
      }
      cat(format("1.000", width=w+6, justify="right", sep=""))
      cat("\n")
      if (chisq) {
        ch <- suppressWarnings(chisq.test(x))
        pvalue <- format(sprintf("%6.4f", ch$p.value), justify="right")
        cat("\nChi-squared test of null hypothesis of equal probabilities\n")
        cat("  Chisq = ", ch$statistic, ",  df = ", ch$parameter, ",  p-value = ", 
          pvalue, sep="", "\n")
        if (any(ch$expected < 5)) 
          cat(">>> Low cell expected frequencies,",
              "so chi-squared approximation may not be accurate", "\n")
      }
    }
  }
  cat("\n")


}


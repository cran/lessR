.pc.main <- 
function(x,
         random.col, col.fill, col.low, col.hi,
         colors, cex, cex.main, quiet, main, ...) {

  # set the labels
  # use variable label for main if it exists and main not specified
  gl <- .getlabels()
  x.name <- gl$xn;  x.lbl <- gl$xl

  if (!is.null(main)) main.lbl <- main
  else if (length(x.lbl) == 0) main.lbl <- x.name else main.lbl <- x.lbl

  # entered counts typically integers as entered but stored as type double
  # if names(x) is null, likely data from sample and c functions
  if (!is.integer(x) && is.double(x) && !is.null(names(x)))  x <- as.table(x)
  if (!is.factor(x) && !is.table(x)) x <- factor(x)
  if (!is.table(x)) ncolors <- nlevels(x) else ncolors <- length(x)

  # color palette
  # set some default colors in case not assigned below
  if (is.ordered(x)) {
    if (colors == "dodgerblue") { 
      if (is.null(col.low)) col.low <- "dodgerblue1"
      if (is.null(col.hi)) col.hi <- "dodgerblue4"
    }
    if (colors == "blue") { 
      if (is.null(col.low)) col.low <- "slategray2"
      if (is.null(col.hi)) col.hi <- "slategray4"
    }
    else if (colors == "gray") {
      if (is.null(col.low)) col.low <- "gray90"
      if (is.null(col.hi)) col.hi <- "gray30"
    }
    else if (colors == "sienna") {
      if (is.null(col.low)) col.low <- "sienna1"
      if (is.null(col.hi)) col.hi <- "sienna4"
    }
    else if (colors == "rose") {
      if (is.null(col.low)) col.low <- "orchid1"
      if (is.null(col.hi)) col.hi <- "orchid4"
    }
    else if (colors == "green") {
      if (is.null(col.low)) col.low <- "darkseagreen1"
      if (is.null(col.hi)) col.hi <- "darkseagreen4"
    }
    else if (colors == "gold") {
      if (is.null(col.low)) col.low <- "goldenrod1"
      if (is.null(col.hi)) col.hi <- "goldenrod4"
    }
    else if (colors == "red") { 
      if (is.null(col.low)) col.low <- "coral1"
      if (is.null(col.hi)) col.hi <- "coral4"
    }
    else if (colors == "purple") { 
      if (is.null(col.low)) col.low <- "darkorchid1"
      if (is.null(col.hi)) col.hi <- "darkorchid4"
    }
    else if (colors == "orange.black") { 
      if (is.null(col.low)) col.low <- "darkorange4"
      if (is.null(col.hi)) col.hi <- "darkorange1"
    }
    else if (colors == "gray.black") { 
      if (is.null(col.low)) col.low <- "gray30"
      if (is.null(col.hi)) col.hi <- "gray90"
    }
    else if (colors == "white") { 
      if (is.null(col.low)) col.low <- "gray30"
      if (is.null(col.hi)) col.hi <- "gray90"
    }
    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(ncolors)
  }
  else if (!is.null(col.low) && !is.null(col.hi)) {
      color.palette <- colorRampPalette(c(col.low, col.hi))
      clr <- color.palette(ncolors)
  }
  else {
    if (colors == "blue")
      clr <- c("slategray", "peachpuff2", "darksalmon", "darkseagreen1", 
        "thistle4", "azure3", "mistyrose")
    else if (colors == "gray") {
      color.palette <- colorRampPalette(c("gray28","gray92"))
      clr <- color.palette(ncolors)
    }
    else if (colors == "rainbow") clr <- rainbow(ncolors)
    else if (colors == "terrain") clr <- terrain.colors(ncolors)
    else if (colors == "heat") clr <- heat.colors(ncolors)
    else  {
      clr <- c("coral3", "seagreen3", "maroon3", "dodgerblue3", "purple3", 
        "turquoise3", "yellow3")
    }
    if (random.col) clr <- clr[sample(length(clr))]
  }

  if (!is.null(col.fill)) {
    for (i in 1:(min(length(col.fill),length(clr)))) clr[i] <- col.fill[i]
    ncolors <- min(length(col.fill),length(clr))
  }

  palette(clr)
  col <- 1:ncolors 

  # plot the pie chart
  if (!is.table(x)) x <- table(x)
  # putting ... makes chart a rectangle
  pie(x, col=col, main=main.lbl, cex=cex, cex.main=cex.main)

# legend("bottom", legend=unique(na.omit(x)), horiz=TRUE, cex=0.8, fill=col)

  # text output
  if (!quiet) {

     .ss.factor(x, brief=TRUE) 

      ch <- chisq.test(x)
      pvalue <- format(sprintf("%6.4f", ch$p.value), justify="right")
      cat("\nChi-squared test of null hypothesis of equal probabilities\n")
      cat("  Chisq = ", ch$statistic, ",  df = ", ch$parameter, ",  p-value = ", 
        pvalue, sep="", "\n")
      if (any(ch$expected < 5)) 
        cat(">>> Low cell expected frequencies,",
            "so chi-squared approximation may not be accurate", "\n")
  }
 
  cat("\n")

}  #  end pc.main

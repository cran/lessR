pc <-
function(x, dframe=mydata, 
         colors=c("relaxed", "vivid", "gray", "rainbow", "terrain", "heat"),
         random.col=FALSE,
         col.slices=NULL, col.low=NULL, col.hi=NULL,
         text.out=TRUE, main=NULL, ...) {


pc.main <- 
function(x, ...) {

  # set the labels
  # use variable label for main if it exists and main not specified
  if (!is.null(main)) main.lbl <- main
  else {
    main.lbl <- ""
    if (exists("mylabels")) {
      lbl <- mylabels[which(row.names(mylabels)==x.name), "label"]
      if (length(lbl) > 0) main.lbl <- lbl
    }
  }
  # entered counts typically integers as entered but stored as type double
  # if names(x) is null, likely data from sample and c functions
  if (!is.integer(x) && is.double(x) && !is.null(names(x)))  x <- as.table(x)
  if (!is.factor(x) && !is.table(x)) x <- factor(x)
  if (!is.table(x)) ncolors <- nlevels(x) else ncolors <- length(x)

    # color palette
  if (is.ordered(x)) {
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
  else if (!is.null(col.low) && !is.null(col.hi)) {
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
    }
    else if (colors == "gray") {
      color.palette <- colorRampPalette(c("gray30","gray90"))
      clr <- color.palette(ncolors)
    }
    else if (colors == "rainbow") clr <- rainbow(ncolors)
    else if (colors == "terrain") clr <- terrain.colors(ncolors)
    else if (colors == "heat") clr <- heat.colors(ncolors)
    if (random.col) clr <- clr[sample(length(clr))]
  }

  if (!is.null(col.slices)) {
    for (i in 1:(min(length(col.slices),length(clr)))) clr[i] <- col.slices[i]
    ncolors <- min(length(col.slices),length(clr))
  }

  palette(clr)
  col <- 1:ncolors 

  # plot the pie chart
  if (!is.table(x)) x <- table(x)
  pie(x, col=col, main=main.lbl, ...)

# legend("bottom", legend=unique(na.omit(x)), horiz=TRUE, cex=0.8, fill=col)

  # text output
  if (text.out) {

    bc.default(x, graph=FALSE)

    n <- sum(!is.na(x))
    n.miss <- sum(is.na(x))

    cat("Sample Size: ", n, "\n")
    cat("Missing Values: ", n.miss, "\n")

  }

  cat("\n")

}  #  end pc.main


#-------------------------------------------------------------------------

# get actual variable name before potential call of dframe$x
x.name <<- deparse(substitute(x)) 

# see if the variable exists in the Global Environment
if (exists(x.name, where=1)) in.global <- TRUE else in.global <- FALSE

# see if the variable exists from a function call
# indicate a function call with sys.frame returns larger than 1 
if (exists(x.name, where=parent.frame(n=1)) && sys.nframe() > 1) 
  in.call <- TRUE else in.call <- FALSE

# see if the data frame exists, if x not in Global Env or function call
dframe.name <- deparse(substitute(dframe))
if (!in.global && !in.call) {
  if (!exists(dframe.name)) {
    if (dframe.name == "mydata") 
      txtA <- ", the default data frame name, " else txtA <- " "
    txtB1 <- "So either create data frame by reading with the rad function, or\n"
    txtB2 <- "  specify the actual data frame with the parameter: dframe\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Data frame ", dframe.name, txtA, "does not exist\n\n", txtB, "\n")
  }
}

# see if variable exists in the data frame, if x not in Global Env or function call 
if (!missing(x) && !in.global && !in.call) {
  if (!exists(x.name, where=dframe)) { 
    if (dframe.name == "mydata") {
      txt1 <- ", the default name \n\n"
      txt2 <- "So either make sure you are using the correct variable name, or\n"
      txt3 <- "  specify the actual data frame with the parameter: dframe\n"
      txt <- paste(txt1, txt2, txt3, sep="")
    }
    else txt <- " "
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Variable ", x.name, " does not exist either by itself ",
        "or in the data frame ", dframe.name, txt, "\n\n")
  }
}

  colors <- match.arg(colors)

if (in.global || in.call) 
  pc.main(x, ...)
else
  pc.main(eval(substitute(dframe$x)), ...)

rm(x.name, envir=.GlobalEnv)

}

dots <-
function(x, dframe=mydata, 
         col.bg="ghostwhite", col.grid="grey85", 
         col.pts="midnightblue", pt.reg=21, pt.out=19, 
         col.out30="firebrick2", col.out15="firebrick4",
         xlab=NULL, main=NULL, text.out=TRUE, new=TRUE, ...) {


dots.main <- 
function(x, ...) {

  # set the labels
  if (is.null(xlab)) x.lbl <- x.name else x.lbl <- xlab
  # use variable label for main if it exists and main not specified
    if (!is.null(main)) main.lbl <- main
    else {
      main.lbl <- ""
      if (exists("mylabels")) {
        lbl <- mylabels[which(row.names(mylabels)==x.name), "label"]
        if (length(lbl) > 0) main.lbl <- lbl
      }
    }

  # text output (before remove missing)
  if (text.out && new) ss.numeric(x, brief=TRUE)

  n <- sum(!is.na(x))
  n.miss <- sum(is.na(x))
  if (n.miss > 0) x <- na.omit(x)
 
  mx <- mean(x)
  
  if (new) {
    # set up plot area
    suppressWarnings(stripchart(x, col="transparent", xlab=x.lbl,
       main=main.lbl, axes=FALSE, ...))
    op <- options()  # save current options to reset later
    options(scipen=30) # turn off scientific notation
    suppressWarnings(axis(1, ...))
    options(op)
    
    # colored background for plotting area
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
    
    # grid lines computation and print
    vx <- pretty(c(usr[1],usr[2]))
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
  }

  # mark outliers
  q1 <- quantile(x, probs=0.25)
  q3 <- quantile(x, probs=0.75)
  lo30 <- q1 - 3.0*IQR(x)
  lo15 <- q1 - 1.5*IQR(x)
  up15 <- q3 + 1.5*IQR(x)
  up30 <- q3 + 3.0*IQR(x)
  stripchart(x[x<lo30], add=TRUE, method="stack", col=col.out30, pch=pt.out, ...)
  stripchart(x[x<lo15], add=TRUE, method="stack", col=col.out15, pch=pt.out, ...)
  stripchart(x[x>up15], add=TRUE, method="stack", col=col.out15, pch=pt.out, ...)
  stripchart(x[x>up30], add=TRUE, method="stack", col=col.out30, pch=pt.out, ...)

  # dots for regular points
  suppressWarnings(stripchart(x[x>lo15 & x<up15], add=TRUE, method="stack",
                   col=col.pts, pch=pt.reg, ...))

  cat("\n")

}  #  end dots.main


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


if (in.global || in.call) 
  dots.main(x, ...)
else
  dots.main(eval(substitute(dframe$x)), ...)

if (new) if (exists("x.name", where=.GlobalEnv)) rm(x.name, envir=.GlobalEnv)

}

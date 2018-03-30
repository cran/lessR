getColors <-
function(clr=NULL, end.clr=NULL,
         n=5, h1=0, h2=NULL, c=100, l=65, fixup=TRUE,
         mixup=TRUE, radius=0.9, 
         labels=TRUE, labels.cex=0.8, border="black", lty="solid",
         quiet=getOption("quiet"), ...) {


# if (!is.null(clr) && (clr[1] != "range") && (!missing(h1) || !missing(h2)
#           || !missing(n))) {
#   cat("\n"); stop(call.=FALSE, "\n","------\n",
#     "Either provide a manual list of colors or specify hcl colors\n",
#     "but not both\n\n")
# }

  if (!is.null(end.clr) && length(clr) > 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "If specifying a range of colors, only specify one beginning color\n\n")
  }

  if (border == "off") border <- NA

  
  # default color scale
  if (is.null(clr) && is.null(end.clr)) {
    theme <- getOption("theme")
    clr <- ifelse (theme %in% c("gray", "white"), "grays", "hcl")
  }
  clr.keep <- tolower(clr)

  # ----------
  # set colors
  
  lbl <- character(length=n)
  
  if (!is.null(end.clr)) {
    color.palette <- colorRampPalette(c(clr, end.clr))
    clr <- color.palette(n)
	ttl <- ""
  }

  else {  # not creating a clr -- end.clr progression

    # set user specified multiple colors
    if (length(clr) > 1) {
      n <- length(clr)
      j <- 0
      for (i in 1:(n)) {
        j <- j + 1
        if (j > length(clr)) j <- 1  # recycle colors
        clr[i] <- clr[j]
      }
 	  ttl <- ""
   } # end clr is multiple values
	
    else if (clr == "hcl") {
      # generate a range of hues of hcl colors at constant c and l
      if (is.null(h2)) h2 <- h1 + (360 * (n - 1) / n) 
      if (n <= 24) {
	     if (mixup)
           h <- c(240,0,120,60,300,180,90,30, 270,210,330,150,
		          255,15,135,75,315,195,255,285,45,225,345,165)  # mixed hues
         # 17,1,9,5,21,13,7,3,19,15,23,11,18,2,10,6,22,14,18,20,4,16,24,12
		 else {
           h <- seq(h1, h2, length=n)  # little different hues
		}
	  }
	  else {  # n too big
        h <- seq(h1, h2, length=n)  # the hcl hues 
 		if (mixup) {
		  o <- sample.int(n)
          h <- h[o]
		}
	  }
      
      clr <- hcl(h, c, l, fixup=fixup)[1:n]  # generate the colors
      #clr <- hex(polarLUV(L=l, C=c, H=h), fixup=fixup, ...)
      lbl <- .fmt(h, 0)
      ttl <- paste("HCL Color Wheel for\n",
                   "Chroma=", c, "Luminance=", l)
    }  # end hcl

    # set grays
    else if (clr == "grays") {
      if (n == 2) { light <- "gray70"; dark <- "gray50" }
      else { light <- "gray82"; dark <- "gray30" }
      color.palette <- colorRampPalette(c(light, dark))
      clr <- color.palette(n)
	  ttl <- "The Grays"
    } 

    # set blues
    else if (clr == "blues") {
      if (n == 2) { light <- "#88AAFF"; dark <- "#1B22FF" }
      else { light <- "lightblue1"; dark <- "blue1"}
      color.palette <- colorRampPalette(c(light, dark))
      clr <- color.palette(n)
	  ttl <- "The Blues"
    } 

    # set reds
    else if (clr == "reds") {
      if (n == 2) { light <- "red1"; dark <- "red4" }
      else { light <- "pink1"; dark <- "red3"}
      color.palette <- colorRampPalette(c(light, dark))
      clr <- color.palette(n)
	  ttl <- "The Reds"
    } 

    # set greens
    else if (clr == "greens") {
      if (n == 2) { light <- "lightgreen"; dark <- "springgreen4" }
      else { light <- "green1"; dark <- "green4"}
      color.palette <- colorRampPalette(c(light, dark))
      clr <- color.palette(n)
	  ttl <- "The Greens"
    }

    else if (clr == "rainbow") {
	 clr <- rainbow(n)
	  ttl <- "Rainbow Colors"
	}
	
	else if (clr == "terrain") {
      clr <- terrain.colors(n)
	  ttl <- "Terrain Colors"
	}
	
	else if (clr == "heat") {
	  clr <- heat.colors(n)
	  ttl <- "Heat Colors"
	}
	
	else {
	  ttl <- "" 
    }

	if (lbl[1] == "") lbl <- clr  # only HCL sets custom lbl
  }

  if (sys.nframe() == 1) {  # called directly, not as an arg to another function

    par(bg=getOption("panel.clr"))
   par(mai=c(.4, .5, .8, .5))

    pin <- par("pin")  # plot dimensions in inches
    xlim <- c(-1, 1)
   ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) 
      xlim <- (pin[1L]/pin[2L]) * xlim
    else
      ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp=1)


    if (!labels) lbl <- NA
    pie(rep(1, length(clr)), col=clr, radius=radius, labels=lbl, main=ttl,
        border=border, lty=lty, cex=labels.cex)

    mc <- max(nchar(clr))
    if (!quiet) {
      cat("\n") 
      if (clr.keep[1] == "hcl") {  # HCL colors
        cat("     HCL   hex      r    g    b\n")
        cat("-------------------------------\n")
        for (i in 1:length(clr))
              cat(.fmt(i,0,w=2), " ", .fmt(h[i],0, w=3), clr[i], 
                  .fmt(col2rgb(clr[i])[1],0,w=4),  .fmt(col2rgb(clr[i])[2],0,w=4),
                  .fmt(col2rgb(clr[i])[3],0,w=4),"\n")
      }
      else {
        cat("  color    r    g    b\n")
        cat("----------------------\n")
        for (i in 1:length(clr))
              cat(.fmtc(clr[i], w=mc, j="left"), 
                  .fmt(col2rgb(clr[i])[1],0,w=4),  .fmt(col2rgb(clr[i])[2],0,w=4),
                  .fmt(col2rgb(clr[i])[3],0,w=4),"\n")
      }
    }
    cat("\n") 
  }  # called directly


  invisible(clr)
}


getColors <-
function(clr=NULL, end.clr=NULL, shape=c("rectangle", "wheel"),
         n=5, h=0, h2=NULL, c=70, l=55, fixup=TRUE,
         in.order=FALSE, radius=0.9, main=NULL,
         labels=NULL, labels.cex=0.8, border="lightgray", lty="solid",
         output=NULL, quiet=getOption("quiet"), ...) {


  shape <- match.arg(shape)

  miss.l <- ifelse (missing(l), TRUE, FALSE)

  if (!is.null(end.clr) && length(clr) > 1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "To specify a sequence of colors, only specify one beginning color\n\n")
  }

  if (!missing(h2)  &&  in.order == FALSE) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
       "h2 only applies to generate a straight sequence of HCL colors\n",
       "  so in.order must be TRUE\n\n")
  }

  if (border == "off") border <- NA

  # default color scale
  ln.c <- length(c)
  ln.l <- length(l)
  if (is.null(clr) && is.null(end.clr) && ln.c==1 && ln.l==1) {
    if (getOption("theme") %in% c("gray", "white")) clr <- "grays"
  }

  nm <- c("blues", "reds", "greens", "yellows", "purples", "olives",
          "rusts", "turquoises", "aquas", "grays")
  nmR <- c("rainbow", "heat", "terrain")


  # set kind of analysis: qualitative, sequential, or manual
  # --------------------------------------------------------

  if (!is.null(clr)) {
    if (clr[1] %in% nm)
      kind <- "sequential"
    else if (clr[1] %in% nmR)
      kind <- "seq.R"
    else if (clr[1] == "colors")
      kind <- "qualitative"
    else {
      if (is.null(end.clr))
        kind <- "manual.q"
      else
        kind <- "manual.s"
    }
  }
  else {  # is.null clr, so HCL
    if (length(c) > 1  ||  length(l) > 1)
      kind <- "sequential" 
    else
      kind <- "qualitative" 
  }


  # misc
  # ----

  # set default value of labels
  if (is.null(labels))
    labels <- ifelse (kind == "qualitative", TRUE, FALSE)

  # set hcl hue for pre-defined color sequence
  if (!is.null(clr) && length(clr) == 1) {
    if (clr %in% nm  &&  clr != "heat") {
      if (clr == "reds") h <- 0
      if (clr == "rusts") h <- 30
      if (clr == "yellows") h <- 60
      if (clr == "olives") h <- 100
      if (clr == "greens") h <- 120
      if (clr == "turquoises") h <- 180
      if (clr == "aquas") h <- 200
      if (clr == "blues") h <- 240
      if (clr == "purples") h <- 280
      if (clr == "grays") c <- 0
      clr <- NULL
    }
  }


  # -----------------
  # set color palette

  lbl <- character(length=n)

  # generate qualitative HCL colors at constant c and l
  if (kind == "qualitative") {
    if (is.null(h2)) h2 <- h + (360 * (n - 1) / n)
    if (n <= 24) {
        if (!in.order)  # mixed hues
          h <- c(120,60,240,0,270,180,90,30,330,210,300,150,
                 255,15,135,75,315,195,255,285,45,225,345,165)
         # 17,1,9,5,21,13,7,3,19,15,23,11,18,2,10,6,22,14,18,20,4,16,24,12
        else
          h <- seq(h, h2, length=n)  # little different hues
    }
    else {  # n too big
      h <- seq(h, h2, length=n)  # the hcl hues
      if (!in.order) {
         o <- sample.int(n)
         h <- h[o]
      }
    }

    clr <- hcl(h, c, l, fixup=fixup)[1:n]  # generate the colors
    #clr <- hex(polarLUV(L=l, C=c, H=h), fixup=fixup, ...)
    lbl <- .fmt(h, 0)
    ttl <- paste("HCL Color Palette for\n",
                 "Chroma=", c, " Luminance=", l)
  }  # end qualitative

  # set sequential HCL color palette
  else if (kind == "sequential") {
    txt.c <- .fmt(c[1],0)
    if (length(c) > 1)
      txt.c <- paste(txt.c, " to ", .fmt(c[2],0), sep="")
    if (miss.l) l <- c(75,45)
    txt.l <- .fmt(l[1],0)
    if (length(l) > 1)
      txt.l <- paste(txt.l, " to ", .fmt(l[2],0), sep="")

    clr <- sequential_hcl(n, h=h, c.=c, l=l, power=1,
                          fixup=fixup, alpha=1)
    ttl <- paste("Sequential Colors for\n", "h=", .fmt(h,0),
                  ", c=", txt.c, ",  l=", txt.l, sep="")
  }

  # custom color sequence
  else if (kind == "manual.s") {
    color.palette <- colorRampPalette(c(clr, end.clr))
    clr <- color.palette(n)
    ttl <- "Custom Color Sequence"
  }

  # set user specified multiple colors
  else if (kind == "manual.q") {
    n <- length(clr)
    j <- 0
    for (i in 1:(n)) {
      j <- j + 1
      if (j > length(clr)) j <- 1  # recycle colors
      clr[i] <- clr[j]
    }
    ttl <- ""
  } 

  else if (kind == "seq.R") {

    if (clr == "rainbow") {
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
  }

  # set lbl except for hcl which provides the hues
  if (lbl[1] == "") lbl <- clr

  # evaluate if text and graphics output
  go.out <- NULL
  if (!is.null(output)) {
    if (output == "on") go.out <- TRUE 
    if (output == "off") go.out <- FALSE
  }
  if (is.null(go.out)) {
    if (is.null(options()$knitr.in.progress))  # not in knitr
      go.out <- ifelse (sys.nframe() == 1, TRUE, FALSE)
    else  # in knitr (generates 1st 18 function calls)
      go.out <- ifelse (sys.nframe() == 19, TRUE, FALSE)
  } 

  # -------------
  # plot and text
  # sys.nframe(): depth of function call,  sys.calls(): the calls

  if (go.out) { 

    # ----
    # plot

    if (!labels) lbl <- NA

    par(bg=getOption("panel.fill"))

    if (shape == "wheel") {
      par(mai=c(.4, .5, .8, .5))

      pin <- par("pin")  # plot dimensions in inches
      xlim <- c(-1, 1)
      ylim <- c(-1, 1)
      if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
      else
        ylim <- (pin[2L]/pin[1L]) * ylim
      plot.window(xlim, ylim, "", asp=1)

      pie(rep(1, length(clr)), col=clr, radius=radius, labels=lbl,
          border=border, lty=lty, cex=labels.cex)
    }  # end wheel

    else if (shape == "rectangle")  {
      if (labels) {
        if (kind == "qualitative") {
          rotate.x <- 0
          bm <- 0.05
          bm.tx <- 0
        }
        else {
          rotate.x <- 90
          bm <- 0.24
          bm.tx <- 0.10
        }
      }  # end label
      else {
        rotate.x <- 0
        bm <- 0
        bm.tx <- 0
      }

      plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE,
           xlab="", ylab="")
      rect(0:(n-1)/n, bm, 1:n/n, 1, col=clr, border=border)
      text(0:(n-1)/n + 1/(2*n), bm.tx, labels=lbl[1:n], srt=rotate.x,
           cex=labels.cex)
    }  # end rectangle

    main.lab <- ifelse (is.null(main), ttl, main)
    title(main=main.lab, cex.main= getOption("main.cex"),
        col.main=getOption("main.color"), ...)

    # -----------
    # text output

    if (!quiet) {
      mc <- max(nchar(clr))
      cat("\n")
      if (kind %in% c("qualitative", "sequential")) {  # HCL colors
        if (kind == "sequential") {
          hh <- h[1]  # h a scaler here (except for grays)
          h <- integer(length(n))
          for (i in 1:n) h[i] <- hh 
        }
        cat("      h    hex      r    g    b\n")
        cat("-------------------------------\n")
        for (i in 1:length(clr))
              cat(.fmt(i,0,w=2), " ", .fmt(h[i],0, w=3), clr[i],
                  .fmt(col2rgb(clr[i])[1],0,w=4), 
                  .fmt(col2rgb(clr[i])[2],0,w=4),
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


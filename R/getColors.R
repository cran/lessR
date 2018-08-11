getColors <-
function(clr=NULL, end.clr=NULL,
         n=12, h=0, h2=NULL, c=NULL, l=NULL, trans=0,
         in.order=FALSE, fixup=TRUE,
         shape=c("rectangle", "wheel"), radius=0.9, border="lightgray",
         main=NULL, labels=NULL, labels.cex=0.8, lty="solid",
         output=NULL, quiet=getOption("quiet"), ...) {


  shape <- match.arg(shape)

  miss.h <- ifelse (missing(h), TRUE, FALSE)
  miss.l <- ifelse (missing(l), TRUE, FALSE)
  miss.c <- ifelse (missing(c), TRUE, FALSE)

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

  nm <- c("reds", "rusts", "yellows", "olives", "greens", "emeralds",  
          "turquoises", "aquas", "blues", "purples", "violets",
          "magentas", "grays")
  nmR <- c("rainbow", "heat", "terrain")


  # set kind of analysis: qualitative, sequential, divergent,  or manual
  # ---------------------------------------------------------------------

  if (!is.null(clr)) {  # at least one color specified
      if (clr[1] %in% nm) {
        kind <- "sequential"
      if (!is.null(end.clr[1])) if (end.clr[1] %in% nm)
        kind <- "divergent"
    }
    else if (clr[1] %in% nmR)
      kind <- "seq.R"
    else if (clr[1] == "colors")
      kind <- "qualitative"
    else {
      if (is.null(end.clr))  # no ending color specified
        kind <- "manual.q"  # manual qualitative sequence
      else
        kind <- "manual.s"  # manual sequential sequence
    }
  }  # end clr is not null
  else {  # is.null clr, so HCL
    if (length(c) > 1  ||  length(l) > 1)  # multiple chroma or luminance
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
  if (!is.null(clr)) {
    if (clr[1] %in% nm  &&  !(clr[1] %in% nmR)) {
      if (clr[1] == "reds") h <- 0
      if (clr[1] == "rusts") h <- 30
      if (clr[1] == "yellows") h <- 60
      if (clr[1] == "olives") h <- 90
      if (clr[1] == "greens") h <- 120
      if (clr[1] == "emeralds") h <- 150
      if (clr[1] == "turquoises") h <- 180
      if (clr[1] == "aquas") h <- 210
      if (clr[1] == "blues") h <- 240
      if (clr[1] == "purples") h <- 270
      if (clr[1] == "violets") h <- 300
      if (clr[1] == "magentas") h <- 330
      if (clr[1] == "grays") {
        c <- 0
        miss.c <- FALSE
      }
      if (is.null(end.clr)) clr <- NULL
    }
    if (!is.null(end.clr)) {
      if (end.clr %in% nm  &&  !(end.clr %in% nmR)) {
      if (end.clr == "reds") h2 <- 0
      if (end.clr == "rusts") h2 <- 3
      if (end.clr == "yellows") h2 <- 60
      if (end.clr == "olives") h2 <- 90
      if (end.clr == "greens") h2 <- 120
      if (end.clr == "emeralds") h2 <- 150
      if (end.clr == "turquoises") h2 <- 180
      if (end.clr == "aquas") h2 <- 210
      if (end.clr == "blues") h2 <- 240
      if (end.clr == "purples") h2 <- 270
      if (end.clr == "violets") h2 <- 300
      if (end.clr == "magentas") h2 <- 330
      if (end.clr == "grays") {
          c <- 0
          miss.c <- FALSE
        }
        clr <- NULL
      }
    }
  }

  
  # -----------------
  # set color palette

  lbl <- character(length=n)

  # qualitative HCL colors at constant c and l
  if (kind == "qualitative") {
    if (!miss.h) if (length(h) > 1) n <- length(h)
    if (is.null(h2)) h2 <- h + (360 * (n - 1) / n)
    if (n <= 24) {
      if (!in.order) { # mixed hues
        h <- c(240,60,120,0,275,180,30,90,210,330,150,300)
        h <- c(h, h+15)
      }
      else  # in.order
        h <- seq(h, h2, length=n)  # vary hue systematically 
    }
    else {  # n too big
      h <- seq(h, h2, length=n)  # the hcl hues
      if (!in.order) {
         o <- sample.int(n)
         h <- h[o]
      }
    }
    
    h[which(h >= 360)] <- h[which(h >= 360)] - 360
    h[which(h < 0)] <- h[which(h < 0)] + 360

    if (miss.c) c <- 65
    if (miss.l) l <- 55
    clr <- hcl(h, c, l, fixup=fixup)[1:n]  # generate the colors
    #clr <- hex(polarLUV(L=l, C=c, H=h), fixup=fixup, ...)
    lbl <- .fmt(h, 0)
    ttl <- paste("HCL Color Palette for\n",
                 "Chroma=", c, " Luminance=", l)
  }  # end qualitative

  # sequential HCL color palette
  else if (kind == "sequential") {
    if (miss.c) c <- c(35,75)
    txt.c <- .fmt(c[1],0)
    if (length(c) > 1)
      txt.c <- paste(txt.c, " to ", .fmt(c[2],0), sep="")

    l.dk <- 25  # darkest color
    l.lt <- 58 + (5*n)  # lightest color
    if (l.lt > 94) {
      l.lt <- 94
      l.dk <- 15
    }
    if (miss.l) l <- c(l.lt, l.dk)  # 2 -> 68, 3 -> 73, 6 -> 88, 8 -> 98
    txt.l <- .fmt(l[1],0)
    if (length(l) > 1)
      txt.l <- paste(txt.l, " to ", .fmt(l[2],0), sep="")

    clr <- sequential_hcl(n, h=h, c.=c, l=l, power=1,
                          fixup=fixup, alpha=1)
    ttl <- paste("Sequential Colors for\n", "h=", .fmt(h,0),
                  ", c=", txt.c, ",  l=", txt.l, sep="")
  }

  # divergent HCL color palette
  else if (kind == "divergent") {
    h <- c(h, h2)
    txt.h <- .fmt(h[1],0)
    if (length(h) > 1)
      txt.h <- paste(txt.h, " to ", .fmt(h[2],0), sep="") 

    if (miss.c) c <- 50
    txt.c <- .fmt(c,0)
    if (length(c) > 1)
      txt.c <- paste(txt.c, " to ", .fmt(c[2],0), sep="")
      
    if (miss.l) l <- c(40,70)
    txt.l <- .fmt(l[1],0)
    if (length(l) > 1)
      txt.l <- paste(txt.l, " to ", .fmt(l[2],0), sep="")

    clr <- diverge_hcl(n, h=h, c=c, l=l, power=0.75,
                       fixup=fixup, alpha=1)
    ttl <- paste("Divergent Colors for\n", "h=", txt.h,
                  ", c=", txt.c, ",  l=", txt.l, sep="")
  }

  # custom color sequence
  else if (kind == "manual.s") {
    color.palette <- colorRampPalette(c(clr, end.clr))
    clr <- color.palette(n)
    ttl <- "Custom Color Sequence"
  }

  # user specified multiple colors
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

  if (!is.null(trans)) 
   for (i in 1:length(clr)) clr[i] <- .maketrans(clr[i], (1-trans)*256) 

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
                .fmt(col2rgb(clr[i])[1],0,w=4), .fmt(col2rgb(clr[i])[2],0,w=4),
                .fmt(col2rgb(clr[i])[3],0,w=4),"\n")
      }
    }
    cat("\n")
  }  # called directly

  invisible(clr)
}


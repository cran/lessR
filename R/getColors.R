getColors <-
function(pal=NULL, end_pal=NULL,
         n=12, h=0, h2=NULL, c=NULL, l=NULL, transparency=0,
         in_order=NULL, fixup=TRUE, power=NULL,
         shape=c("rectangle", "wheel"), radius=0.9, border="lightgray",
         main=NULL, labels=NULL, labels_cex=0.8, lty="solid",
         output=NULL, quiet=getOption("quiet"), ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("end.pal", "in.order", "labels.cex")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  shape <- match.arg(shape)

  h.miss <- ifelse (missing(h), TRUE, FALSE)
  l.miss <- ifelse (missing(l), TRUE, FALSE)
  c.miss <- ifelse (missing(c), TRUE, FALSE)

  main.miss <- ifelse (missing(main), TRUE, FALSE)
  if (!is.null(main)) if (!main.miss) if (main=="" || main==" ") main <- NULL

  output.miss <- ifelse (missing(output), TRUE, FALSE)

  # default output is FALSE unless a direct manual call from the console
  # for Markdown files, need to set OUTPUT=TRUE if a direct call
  if (output.miss)
    output <- ifelse(sys.nframe() == 1, TRUE, FALSE)

  if (!is.null(end_pal) && length(pal) > 1) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
      "To specify a sequence of colors, only specify one beginning color\n\n")
  }

  if (is.null(in_order))
    in_order <- ifelse (shape == "wheel", TRUE, FALSE)  # for a wheel do in order

  if (!missing(h2)  &&  in_order == FALSE) {
    cat("\n"); stop(call.=FALSE, "\n------\n",
       "h2 only applies to generate a straight sequence of HCL colors\n",
       "  so in_order must be TRUE\n\n")
  }

  if (border %in% c("off", "transparent")) border <- NA

  # default color scale
  ln.c <- length(c)
  ln.l <- length(l)
  if (is.null(pal) && is.null(end_pal) && ln.c==1 && ln.l==1) {
    if (getOption("theme") %in% c("gray", "white"))
      pal <- "grays"
    else
      pal <- "hues"
  }

  if (!is.null(pal[1]))
    if (pal[1] == "yellows") pal[1] <- "browns"  # as of 3.7.7
  if (!is.null(end_pal))
    if (end_pal == "yellows") end_pal <- "browns"

  if (!missing(pal)) if (pal[1] == "magma") pal[1] <- "plasma"  # approx magma

  nm <- c("reds", "rusts", "browns", "olives", "greens", "emeralds",
          "turquoises", "aquas", "blues", "purples", "violets",
          "magentas", "grays")
  nmV<- c("viridis", "cividis", "plasma", "spectral")
  nmO<- c("Okabe-Ito")
  nmD<- c("distinct")
  nmW<- c("BottleRocket1", "BottleRocket2", "Rushmore1", "Rushmore",
          "Royal1", "Royal2", "Zissou1", "Darjeeling1", "Darjeeling2",
          "Chevalier1", "FantasticFox1", "Moonrise1", "Moonrise2",
          "Moonrise3", "Cavalcanti1", "GrandBudapest1", "GrandBudapest2",
          "IsleofDogs1", "IsleofDogs2")
  nmR <- c("rainbow", "heat", "terrain",
           "rainbow_hcl", "heat_hcl", "terrain_hcl")
  nmT <- c("Tableau")


  # set kind of analysis: qualitative, sequential, divergent, or manual
  # -------------------------------------------------------------------

  if (!is.null(pal)) {  # at least one color specified

    if (pal[1] %in% nm) {
      kind <- "sequential"
    if (!is.null(end_pal[1])) if (end_pal[1] %in% nm)
      kind <- "divergent"
    }
    else if (pal[1] %in% nmR)
      kind <- "seq.R"
    else if (pal[1] == "hues")
      kind <- "qualitative"
    else if (pal[1] %in% nmV)
      kind <- "viridis"
    else if (pal[1] %in% nmO)
      kind <- "oi"
    else if (pal[1] %in% nmW)
      kind <- "wes"
    else if (pal[1] %in% nmD)
      kind <- "distinct"
    else if (pal[1] %in% nmT)
      kind <- "Tableau"

    else {  # pal[1] not in any nm vector
      if (is.null(end_pal))  # no ending color specified
        kind <- "manual.q"  # manual qualitative sequence
      else
        kind <- "manual.s"  # manual sequential sequence
    }
  }  # end pal is not null

  else {  # is.null pal -- defaults
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
  if (!is.null(pal)) {
    if (pal[1] %in% nm  &&  !(pal[1] %in% nmR)) {
      if (pal[1] == "reds") h <- 0
      if (pal[1] == "rusts") h <- 30
      if (pal[1] == "browns") h <- 60
      if (pal[1] == "olives") h <- 90
      if (pal[1] == "greens") h <- 120
      if (pal[1] == "emeralds") h <- 150
      if (pal[1] == "turquoises") h <- 180
      if (pal[1] == "aquas") h <- 210
      if (pal[1] == "blues") h <- 240
      if (pal[1] == "purples") h <- 270
      if (pal[1] == "violets") h <- 300
      if (pal[1] == "magentas") h <- 330
      if (pal[1] == "grays") {
        c <- 0
        c.miss <- FALSE
      }
      if (is.null(end_pal)) pal <- NULL
    }
    if (!is.null(end_pal)) {
      if (end_pal %in% nm  &&  !(end_pal %in% nmR)) {
      if (end_pal == "reds") h2 <- 0
      if (end_pal == "rusts") h2 <- 30
      if (end_pal == "browns") h2 <- 60
      if (end_pal == "olives") h2 <- 90
      if (end_pal == "greens") h2 <- 120
      if (end_pal == "emeralds") h2 <- 150
      if (end_pal == "turquoises") h2 <- 180
      if (end_pal == "aquas") h2 <- 210
      if (end_pal == "blues") h2 <- 240
      if (end_pal == "purples") h2 <- 270
      if (end_pal == "violets") h2 <- 300
      if (end_pal == "magentas") h2 <- 330
      if (end_pal == "grays") {
          c <- 0
          c.miss <- FALSE
        }
        pal <- NULL
      }
    }
  }


  # -----------------
  # set color palette

  lbl <- character(length=n)

  # qualitative HCL colors at constant c and l
  if (kind == "qualitative") {
    if (!h.miss) if (length(h) > 1) n <- length(h)
    if (is.null(h2)) h2 <- h + (360 * (n - 1) / n)
    if (n <= 24) {
      if (!in_order) { # mixed hues
        h <- c(240,60,120,0,275,180,30,90,210,330,150,300)
        h <- c(h, h+15)  # can do 12+15=27 unique colors
      }
      else  # in_order
        h <- seq(h, h2, length=n)  # vary hue systematically
    }
    else {  # n too big
      h <- seq(h, h2, length=n)  # the hcl hues
      if (!in_order) {
         o <- sample.int(n)
         h <- h[o]
      }
    }

    h[which(h >= 360)] <- h[which(h >= 360)] - 360
    h[which(h < 0)] <- h[which(h < 0)] + 360

    if (c.miss) c <- 65
    if (l.miss) l <- 60
    pal <- hcl(h, c, l, fixup=fixup)[1:n]  # generate the colors
    #pal <- hex(polarLUV(L=l, C=c, H=h), fixup=fixup, ...)
    lbl <- .fmt(h, 0)
    ttl <- paste("HCL Color Palette for\n",
                 "Chroma=", c, " Luminance=", l)
  }  # end qualitative

  # sequential HCL color palette
  else if (kind == "sequential") {
    if (c.miss) c <- c(35,75)
    txt.c <- .fmt(c[1],0)
    if (length(c) > 1)
      txt.c <- paste(txt.c, " to ", .fmt(c[2],0), sep="")

    l.dk <- 35 - (3*n)  # darkest color
    if (l.dk < 14) l.dk <- 14  # any darker and the hue is no longer true
    l.lt <- 52 + (5*n)  # lightest color
    if (l.lt > 92) l.lt <- 92
    if (l.miss) l <- c(l.lt, l.dk)  # 2 -> 58, 3 -> 63, 6 -> 78, 8 -> 88
    txt.l <- .fmt(l[1],0)
    if (length(l) > 1)
      txt.l <- paste(txt.l, " to ", .fmt(l[2],0), sep="")

    if (is.null(power)) power <- 1
    pal <- colorspace::sequential_hcl(n, h=h, c.=c, l=l, power=power,
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

    if (c.miss) c <- 50
    txt.c <- .fmt(c,0)
    if (length(c) > 1)
      txt.c <- paste(txt.c, " to ", .fmt(c[2],0), sep="")

    if (l.miss) l <- c(30,80)
    txt.l <- .fmt(l[1],0)
    if (length(l) > 1)
      txt.l <- paste(txt.l, " to ", .fmt(l[2],0), sep="")

    if (is.null(power)) power <- 0.75
    pal <- colorspace::diverging_hcl(n, h=h, c=c, l=l, power=power,
                       fixup=fixup, alpha=1)
    ttl <- paste("Divergent Colors for\n", "h=", txt.h,
                  ", c=", txt.c, ",  l=", txt.l, sep="")
  }

  # viridis sequence
  else if (kind == "viridis") {
    ttl <- paste("Viridis Style Color Palette for:", pal[1], "\n")
    pal <- hcl.colors(n, palette = pal[1])
  }

  # Okabe-Ito colors
  else if (kind == "oi") {
    ttl <- paste("Okabe-Ito Colors Palette", pal[1], "\n")
    pal <- palette.colors(n=9, palette="Okabe-Ito", alpha=1)[2:9]
    pal[9] <- "#000000FF"  # put black at the end
    if (missing(n)) n <- 9
    if (n <= 9)
      pal <- pal[1:n]
    else {
      print(pal[1:9])
      cat("\n"); stop(call.=FALSE, "\n------\n",
         "Only 9 Okabe-Ito colors available.\n",
         "Can start with a vector of the above 9 colors, then add more.\n\n")
    }
  }

 # Tableau colors
  else if (kind == "Tableau") {
    ttl <- paste("Tableau Qualitative Colors Palette", pal[1], "\n")
    pal <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
             "#EDC949", "#AF7AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
    if (missing(n)) n <- 10
    if (n <= 10)
      pal <- pal[1:n]
    else {
      print(pal[1:10])
      cat("\n"); stop(call.=FALSE, "\n------\n",
         "Only 10 Tableau colors available.\n",
         "Can start with a vector of the above 10 colors, then add more.\n\n")
    }
  }

  # Wes Anderson sequence
  else if (kind == "wes") {
    if (!requireNamespace("wesanderson", quietly=TRUE)) {
      stop("Package \"wesanderson\" needed for these colors\n",
           "Please install it:  install.packages(\"wesanderson\")\n\n",
           call. = FALSE)
    }
    ttl <- paste("A Wes Anderson Color Palette for:", pal[1], "\n")
    pal <- wesanderson::wes_palette(pal[1], n, type="continuous")
  }

# pre-specified distinct colors
  else if (kind == "distinct") {
    ttl <- paste("Colors Palette", pal[1], "\n")
#   pal <- c(getColors(c=90, l=50, n=5),
    pal <- c(
             "goldenrod2", "gray45", "yellowgreen", "orchid3", "skyblue",
             "darkgray", "lightcoral", "navajowhite4", "cyan3", "darkorange3",
             "maroon3", "mediumaquamarine", "royalblue1", "mistyrose4",
             "thistle3")
    if (n <= 20)
      pal <- pal[1:n]
    else {
      print(pal[1:20])
      cat("\n"); stop(call.=FALSE, "\n------\n",
         "Only 20 distinct colors available.\n",
         "Can start with a vector of the above 20 colors, then add more.\n\n")
    }
  }

  # custom color sequence
  else if (kind == "manual.s") {
    color_palette <- colorRampPalette(c(pal, end_pal))
    pal <- color_palette(n)
    ttl <- "Custom Color Sequence"
  }

  # user specified multiple colors
  else if (kind == "manual.q") {
    n <- length(pal)
    j <- 0
    for (i in 1:(n)) {
      j <- j + 1
      if (j > length(pal)) j <- 1  # recycle colors
      pal[i] <- pal[j]
    }
    ttl <- ""
  }

  else if (kind == "seq.R") {
    if (is.null(l)) l <- 60
    if (is.null(c)) c <- 65

    if (pal == "rainbow") {
      pal <- rainbow(n)
      ttl <- "Rainbow Colors"
    }
    else if (pal == "terrain") {
      pal <- terrain.colors(n)
      ttl <- "Terrain Colors"
    }
    else if (pal == "heat") {
      pal <- heat.colors(n)
      ttl <- "Heat Colors"
    }
    else if (pal == "rainbow_hcl") {
      pal <- colorspace::rainbow_hcl(n, l=l, c=c)
      ttl <- "Rainbow HCL Colors"
    }
    else if (pal == "terrain_hcl") {
      pal <- colorspace::terrain_hcl(n, l=l, c.=c)
      ttl <- "Terrain HCL Colors"
    }
    else if (pal == "heat_hcl") {
      pal <- colorspace::heat_hcl(n, l=l, c.=c)
      ttl <- "Heat HCL Colors"
    }
  }

  # set lbl except for hcl which provides the hues
  if (lbl[1] == "") lbl <- pal

  if (transparency > 0)
   for (i in 1:length(pal)) pal[i] <- .maketrans(pal[i], (1-transparency)*256)

  # --------------------
  # plot and text output
  if (output) {

    # ----
    # plot

    # ttl is constructed, the default title if main not specified
    if (main.miss)
      main.lab <-  ttl
    else
      main.lab <- main  # could be NULL

    if (!labels) lbl <- NA

    par(bg=getOption("panel_fill"))

    if (shape == "wheel") {
      top <- ifelse(is.null(main.lab), 0.2, 0.7)
      par(mai=c(.1, 0, top, 0))

      pin <- par("pin")  # plot dimensions in inches
      xlim <- c(-1, 1)
      ylim <- c(-1, 1)
      if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
      else
        ylim <- (pin[2L]/pin[1L]) * ylim
      plot.window(xlim, ylim, "", asp=1)

      pie(rep(1, length(pal)), col=pal, radius=radius, labels=lbl,
          border=border, lty=lty, cex=labels_cex)
    }  # end wheel

    else if (shape == "rectangle")  {
      top <- ifelse(is.null(main.lab), 0.2, 0.5)
      par(mai=c(.1, .1, top, .1))
      if (labels) {
        if (kind == "qualitative") {
          rotate_x <- 0
          bm <- 0.05
          bm.tx <- 0
        }
        else {
          rotate_x <- 90
          bm <- 0.24
          bm.tx <- 0.10
        }
      }  # end label
      else {
        rotate_x <- 0
        bm <- 0
        bm.tx <- 0
      }

      plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1), axes=FALSE,
           xlab="", ylab="")
      rect(0:(n-1)/n, bm, 1:n/n, 1, col=pal, border=border)
      text(0:(n-1)/n + 1/(2*n), bm.tx, labels=lbl[1:n], srt=rotate_x,
           cex=labels_cex)
    }  # end rectangle

    if (!is.null(main.lab))
      title(main=main.lab, cex.main=getOption("main_cex"),
            col.main=getOption("main_color"), ...)


    # -----------
    # text output

    if (!quiet) {
      mc <- max(nchar(na.omit(pal)))
      cat("\n")
      if (kind %in% c("qualitative", "sequential")) {  # HCL colors
        if (kind == "sequential") {
          hh <- h[1]  # h a scaler here (except for grays)
          h <- integer(length(n))
          for (i in 1:n) h[i] <- hh
        }
        cat("      h    hex      r    g    b\n")
        cat("-------------------------------\n")
        for (i in 1:length(pal))
              cat(.fmt(i,0,w=2), " ", .fmt(h[i],0, w=3), pal[i],
                  .fmt(col2rgb(pal[i])[1],0,w=4),
                  .fmt(col2rgb(pal[i])[2],0,w=4),
                  .fmt(col2rgb(pal[i])[3],0,w=4),"\n")
      }

      else {  # qualitative
        cat("  color    r    g    b\n")
        cat("----------------------\n")
          for (i in 1:length(pal))
            cat(.fmtc(pal[i], w=mc, j="left"),
                .fmt(col2rgb(pal[i])[1],0,w=4), .fmt(col2rgb(pal[i])[2],0,w=4),
                .fmt(col2rgb(pal[i])[3],0,w=4),"\n")
      }
    }
    cat("\n")
  }  # do output

  return(invisible(pal))
}

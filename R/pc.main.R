.pc.main <- 
function(x, y,
        fill, color, trans, 
        radius, hole, hole_fill, edges, 
        clockwise, init_angle, 
        density, angle, lty, lwd,
        labels, labels_position, labels_color, labels_size, labels_digits,
        labels_cex, main_cex, main, main.miss,
        add, x1, x2, y1, y2,
        quiet, pdf_file, width, height, ...)  {

  is.ord <- ifelse (is.ordered(x), TRUE, FALSE)

  # set the labels
  # use variable label for main if it exists and main not specified
  gl <- .getlabels(main=main, lab_cex=getOption("lab_cex"))
  x.name <- gl$xn; x.lbl <- gl$xl
  if (!is.null(main))
    main.lbl <- main
  else {
    if (main.miss)  # main was not explicitly set to NULL 
      main.lbl <- ifelse (is.null(x.lbl), x.name, x.lbl)
    else
      main.lbl <- NULL
  }

  # size the label for display
# if (strwidth(main.lbl, units="figure", cex=main_cex) > .85) {
#   brk <- nchar(main.lbl)
#   while (strwidth(substr(main.lbl,1,brk), units="figure", cex=main_cex) > .85)
#     brk <- brk-1 
#   while (substr(main.lbl,brk,brk) != " ") brk <- brk-1
#   main.lbl <- paste(substr(main.lbl,1,brk), "\n",
#                     substr(main.lbl,brk+1,nchar(main.lbl)))
#   while (strwidth(main.lbl, units="figure", cex=main_cex) > .85)
#     main_cex <- main_cex-0.05
# }

  # entered counts typically integers as entered but stored as type double
  # if names(x) is null, likely data from sample and c functions
  if (!is.integer(x) && is.double(x) && !is.null(names(x)))
    x <- as.table(x)
  if (!is.factor(x) && !is.table(x))
    x <- factor(x)
  n_cat <- ifelse (!is.table(x), nlevels(x), length(x))
  clr <- character(length=n_cat)  # slice colors


  # ------
  # colors

  if (length(fill) > 1) {
    clr <- fill
    j <- 0
    for (i in 1:(n_cat)) {
      j <- j + 1
      if (j > length(fill)) j <- 1  # recycle colors
      clr[i] <- fill[j]
    }
  }
  else {  # length(fill) == 0
    if (is.null(fill)) {  # fill not specified
      if (!is.ord) {  # hues for nominal 
        clr <- getColors(n=n_cat, output=FALSE)
        if (!is.null(.color_range(clr, n_cat)))  
          clr <- .color_range(clr, n_cat)
      }
      else  # sequential palette for ordinal based on theme
        clr <- .color_range(.get_fill(seq.pal=TRUE), n_cat) 
    }
    else {  # fill specified by user
      if (is.null(.color_range(fill, n_cat)))
        clr <- fill  # user assigned
      else 
        clr <- .color_range(fill, n_cat)  # do default range, or user assigned
    }
  }

  # not a range, so set user specified multiple colors
# if (is.null(clr)) {
# } # end fill is multiple values

  if (!is.null(trans)) 
    for (i in 1:n_cat) clr[i] <- .maketrans(clr[i], (1-trans)*256) 


  # ----------------
  # prepare the data

  # x is categorical variable
  if (is.null(y)) {  # tabulate x
    if (!is.table(x)) x <- table(x)
  }
  else {  # y contains the values
    x.cat <- x
    x <- y
    x <- as.table(x)
    names(x) <- x.cat 
  }

  x.tbl <- x  # save tabled values for text output

  nms.x <- names(x)
  x <- as.numeric(x)
  if (labels != "off") {
    if (labels == "input")
      x.txt <- as.character(x)
    else if (labels == "%")
      x.txt <- paste(.fmt(x/sum(x) * 100, labels_digits), "%", sep="")
    else if (labels == "prop")
      x.txt <- .fmt(x/sum(x), labels_digits)
  }


  # ------------------
  # plot the pie chart
  # ------------------

  # modified R pie function to add inner hole at the end, plot a radius for each
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
      stop("'x' values must be positive.")
  
  # set up and open plot window
  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  
  par(bg=getOption("panel_fill"))
  tm <- ifelse (is.null(main.lbl), .6, .8)
  par(mai=c(.4, .5, tm, .5))
  plot.new()

  pin <- par("pin")  # plot dimensions in inches
  xlim <- c(-1, 1)
  ylim <- c(-1, 1)
  if (pin[1] > pin[2]) 
    xlim <- (pin[1]/pin[2]) * xlim
  else
    ylim <- (pin[2]/pin[1]) * ylim
  plot.window(xlim, ylim, "", asp=1)

  # set labels
  if (is.null(nms.x)) 
    nms.x <- as.character(seq_along(x))
  else
    nms.x <- as.graphicsAnnot(nms.x)

  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)

  if (length(color) < nx) color <- rep_len(color, nx)
  if (length(lty) < nx) lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    if (length(density) < nx) density <- rep_len(density, nx)

  # get coordinates of a circle with specified radius
  twopi <- ifelse (clockwise, -2*pi, 2*pi)
  t2xy <- function(t, radius) {
      t2p <- twopi * t + init_angle * pi/180
      list(x = radius * cos(t2p), y = radius * sin(t2p))
  }

  # construct plot slice by slice
  labels_cl <- character(length=nx)
  if (length(labels_color) == 1)
    for (i in 1:nx) labels_cl[i] <- labels_color[1]
  if (length(labels_color) == nx)
    labels_cl <- labels_color
  else {  # recycle
    j <- 0
    for (i in 1:nx) {
      j <- j + 1
      if (j > length(labels_color)) j <- 1
      labels_cl[i] <- labels_color[j]
    }
  }

  for (i in 1:nx) { # slice by slice 
    # plot slice
    n <- max(2, floor(edges * dx[i]))
    p <- t2xy(seq.int(x[i], x[i + 1], length.out=n), radius)
    polygon(c(p$x, 0), c(p$y, 0), density=density[i], angle=angle[i], 
        border=color[i], col=clr[i], lty=lty[i], lwd=lwd)

    # plot labels if "out"
    p <- t2xy(mean(x[i + 0:1]), radius)
    lab <- as.character(nms.x[i])
    if (labels_cex > 0)
      if (nzchar(lab)) {
        lines(c(1, 1.05)*p$x, c(1, 1.05)*p$y)  # tick marks
      if (labels != "off") if (labels_position == "out") {
        if (labels_cex > 0) {
          cx <- 1.1;  cy <- 1.175
          text(cx*p$x, (cy*p$y-.1), x.txt[i], xpd=TRUE, col=labels_cl[i],
               adj=ifelse(p$x < 0, 1, -.25), cex=labels_size, ...)
        }
      }  # end out


      # plot the labels if "in"
      if (labels != "off") if (labels_position == "in") {
        cx <- 0.82;  cy <- 0.86  # scale factors to position labels
        if (hole < 0.65) {  # scale factor to slide text down for small hole
          cx <- cx * (1 - (.16 * (1-hole)))  # max slide is 0.84
          cy <- cy * (1 - (.16 * (1-hole)))
        }
        if (hole > 0.77) {  # scale factor to slide text down for small hole
          cx <- cx * (1 - (-.42 * (1-hole)))  # max slide is 0.84
          cy <- cy * (1 - (-.42 * (1-hole)))
        }
        if (hole > 0.89) {  # scale factor to slide text down for small hole
          cx <- cx * (1 - (-.62 * (1-hole)))  # max slide is 0.84
          cy <- cy * (1 - (-.62 * (1-hole)))
        }
        text(cx*p$x, cy*p$y, x.txt[i], xpd=TRUE, col=labels_cl[i],
             cex=labels_size, ...)
      }  # end "in"

      # plot the labels
      cx <- 1.1;  cy <- 1.175
      text(cx * p$x, cy * p$y, nms.x[i], xpd=TRUE, 
        adj=ifelse(p$x < 0, 1, 0), cex=labels_cex, ...)  # labels
    }
  }  # end slice by slice

  # add centered hole over the top of the pie
  p <- t2xy(seq.int(0, 1, length.out=125), hole)
  polygon(p$x, p$y, col=hole_fill, border=color, lty=lty[1], lwd=lwd)

  title(main=main.lbl, cex.main=main_cex, col.main=getOption("main_color"))


  # -----------
  # text output
  # -----------

  #if (length(dim(x)) == 1  && !quiet) {  # one variable
# if (!quiet) { 

    txsug <- ""
    if (getOption("suggest")) {
      txsug <- ">>> suggestions"
        fc <- paste("PieChart(", x.name,
                    ", hole=0)  # traditional pie chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("PieChart(", x.name,
                    ", labels=\"%\")  # display %'s on the chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("PieChart(", x.name, ")  # bar chart", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name, ")  # bubble plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
        fc <- paste("Plot(", x.name,
                    ", labels=\"count\")  # lollipop plot", sep="")
        txsug <- paste(txsug, "\n", fc, sep="")
    }
    class(txsug) <- "out"

    if (.is.integer(x.tbl)) {
      stats <- .ss.factor(x.tbl, brief=TRUE, x.name=x.name)
      txttl <- stats$title
      counts <- stats$counts
      chi <- stats$chi
      class(txttl) <- "out"
      class(counts) <- "out"
      class(chi) <- "out"
      output <- list(out_suggest=txsug, out_title=txttl,
                     out_counts=counts, out_chi=chi)
    }
    else {
      stats <- .ss.numeric(x.tbl, brief=TRUE, x.name=getOption("yname"))
      txout <- stats$tx
      output <- list(out_suggest=txsug, out_stats=txout)
    }

    class(output) <- "out_all"
    if (!quiet) print(output)      
# }

  if (!is.null(add)) {

    add_cex <- getOption("add_cex")
    add_lwd <- getOption("add_lwd")
    add_lty <- getOption("add_lty")
    add_color <- getOption("add_color")
    add_fill <- getOption("add_fill")
    add_trans <- getOption("add_trans")

    .plt.add (add, x1, x2, y1, y2,
              add_cex, add_lwd, add_lty, add_color, add_fill, add_trans) 
  }

  cat("\n")
  return(output)

}  #  end pc.main

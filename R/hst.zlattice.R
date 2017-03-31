.hst.lattice <- 
function(x, by, by2, nrows, ncols, asp,
         fill, stroke, bg, col.grid, trans, xlab, ylab, main,
         breaks, cex.axis, width, height, pdf.file) {

  txt <- "[Trellis graphics with Deepayan Sarkar's lattice package]\n\n"

  # open graphics window of specified dimensions
  if (options("device") != "RStudioGD")
    dev.new(width=width, height=height)

  size.lab <- cex.axis 
  gl <- .getlabels(xlab, ylab, main, cex.lab=size.lab)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  main.lab <- gl$mb
  sub.lab <- gl$sb
  size.lab <- gl$cex.lab
  by.name <- getOption("byname")

  col.fill <- ifelse (is.null(trans), fill, .maketrans(fill, (1-trans)*256)) 
  col.bg <- ifelse(sum(col2rgb(bg)) < 370, "transparent", bg)
  if (is.null(trans)) trans <- getOption("trans.fill.pt")

  t.bg <- trellis.par.get("panel.background")
    t.bg$col <- bg
  trellis.par.set("panel.background", t.bg)

  t.sbg <- trellis.par.get("strip.background")
    t.sbg$col <- getOption("fill.ellipse")
  trellis.par.set("strip.background", t.sbg)

  if (is.null(by2))
    p <- histogram( ~ x | by,
               col=fill, border=stroke)
  else
    p <- histogram( ~ x | by * by2, xlab=x.lab, ylab=y.lab,
               col=fill, border=stroke)

  #p <- update(p,
         #xlab=x.lab, ylab=y.lab, aspect=asp,
         #panel = function(x, ...) {
            #panel.histogram(x, ...)
            #panel.mathdensity(dmath = dnorm, col = "black",
                                #args = list(mean=mean(x),sd=sd(x)))
          #})

#histogram( ~ height | voice.part, data = singer,
          #xlab = "Height (inches)", type = "density",
          #panel = function(x, ...) {
              #panel.histogram(x, ...)
              #panel.mathdensity(dmath = dnorm, col = "black",
                                #args = list(mean=mean(x),sd=sd(x)))
          #})

  # customize layout cols and rows, need only specify one
  if (!is.null(nrows) ||  !is.null(ncols)) {
    n.panels <- ifelse (is.null(by2), nlevels(by), nlevels(by)*nlevels(by2))
    if (is.null(ncols)) ncols <- (n.panels %/% nrows) + (n.panels %% nrows > 0)
    if (is.null(nrows)) nrows <- (n.panels %/% ncols) + (n.panels %% ncols > 0)
    p <- update(p, layout=c(ncols, nrows))
  } 



  # display
  if (pdf.file) {
    pdf(pdf.file, width=width, height=height)
    trellis.par.set("strip.background", t.sbg)
    trellis.par.set("panel.background", t.bg)
    print(p)
    dev.off()
  }
  else {
    print(p)
  }


}


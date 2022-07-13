.plt.marg <- function(max.y.width, y.lab, x.lab, main, sub,
                  rotate_x=0, mx.x.val.ln=1, mx.y.val.ln=1,
                  lab_x_cex=0.95, lab_y_cex=0.95, max.x.width=NULL) {
# not processing sub at this time

  # top margin
  tm <- 0.05  # old is 0.15
  if (!is.null(main)) tm <- tm + .25
  # if (options("device") == "RStudioGD") {
    # tm <- ifelse(.Platform$OS == "windows", tm-.15, 0)
  # }

  # right margin
  rm <- 0.15

  # bottom margin
  n.lab_x.ln <- 0  # in case x.lab is null
  if (!is.null(x.lab)) {
    if (x.lab != "") {
      strn <- unlist(gregexpr("\n", x.lab, fixed=TRUE))
      if (strn[1] == -1) strn <- NULL  # return of -1 means no \n
      n.lab_x.ln <- length(strn) + 1
    }
    else  # such as from default for time series
      n.lab_x.ln <- -0.6 + (.1 * lab_x_cex)
  }

  ln.ht <- par('cin')[2] * lab_x_cex * par('lheight')  # lin ht inches

  # rotate_x==90 and horiz=TRUE not compatible, leads to NULL max.x.width
  if (rotate_x != 90  ||  is.null(max.x.width))
    bm <- ((n.lab_x.ln + mx.x.val.ln) * .70 * ln.ht) + 0.30  # inches
  else
    bm <- max.x.width + (ln.ht * n.lab_x.ln) + 0.28
  bm <- bm + (-0.065 +(.055* n.lab_x.ln))
  tm <- ifelse (is.null(main), tm+.05, tm+.25)  #  adjust tm for increased bm
  if (rotate_x != 0) bm <- bm + .15
  if (lab_x_cex > 1.1) bm <- bm + .04  # actually should be axis_cex

  # left margin
  n.lab_y.ln <- 0  # in case x.lab is null
  if (!is.null(y.lab)) {
    if (y.lab != "") {
      strn <- unlist(gregexpr("\n", y.lab, fixed=TRUE))
      if (strn[1] == -1) strn <- NULL  # return of -1 means no \n
      n.lab_y.ln <- length(strn) + 1
    }
  }
  mm <- max.y.width + 0.27
  if (max.y.width < .10) mm <- mm + .02
  if (lab_y_cex > 1) mm <- mm + .10
  if (!is.null(y.lab)) mm <- mm + (n.lab_y.ln * .20)

  return(list(lm=mm, tm=tm, rm=rm, bm=bm,
              n.lab_x.ln=n.lab_x.ln, n.lab_y.ln=n.lab_y.ln))
}

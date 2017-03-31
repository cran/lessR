bc.data.frame <-
function(x, n.cat,
         col.fill, col.stroke, col.bg, col.grid, col.box, col.trans, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
         labels, label.size,
         cex.axis, col.axis, rotate.x, rotate.y, offset, beside,
         col.low, col.hi,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet,
         width, height, pdf, ...)  {

  sug <- getOption("suggest")
  options(suggest = FALSE)

  manage.gr <- .graphman()  # see if graphics are to be managed
  if (manage.gr  &&  !pdf) {
    i.win <- 0
    for (i in 1:ncol(x)) {
      if (is.numeric(x[,i])  &&  !.is.num.cat(x[,i], n.cat)) 
        i.win <- i.win + 1
    }
    .graphwin(i.win, width, height)
    open.win <- 2
  }

  plot.i <- 0  # keep track of generated graphics
  plot.title  <- character(length=0)

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    if (!is.numeric(x[,i]) || .is.num.cat(x[,i],n.cat)) {
 
      if (nlevels(factor(x[,i])) < length(x[,i])) {

        x.name <- names(x)[i]
        options(xname = x.name)

        if (nu == 1)
          cat("\nVariable", x.name, "has only one value. No barchart produced.\n\n")

        else {

        if (pdf) {
          pdf.fnm <- paste("BarChart_", x.name, ".pdf", sep="") 
          .opendev(pdf.fnm, width, height)
        } 
        else {
          pdf.fnm <- NULL
          plot.i <- plot.i + 1
          plot.title[plot.i] <- paste("BarChart of ", x.name, sep="")
          if (manage.gr) {
            open.win <- open.win + 1
            dev.set(which = open.win)
          }
        }

        .bc.main(x[,i], y=NULL, by=NULL,
          col.fill, col.stroke, col.bg, col.grid, col.box, col.trans, colors,
          horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
          value.labels=NULL, label.size,
          cex.axis, col.axis, rotate.x, rotate.y, offset, beside,
          col.low, col.hi, 
          legend.title, legend.loc, legend.labels, legend.horiz, quiet,
          font.main=1, ...)

        if (pdf) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "bar chart")
        }

        if (.is.integer(x[,i]) && nu <= n.cat && !quiet)
          .ncat("bar chart", x.name, nu, n.cat)

        }
      }

      else cat("\n", names(x)[i], "appears to contain unique Names or IDs\n")
    }

    #else cat("\n", "--- ", names(x)[i], " --- is numerical, better to do a ",
             #"histogram\n\n", sep="")

  }  # each column in x

    options(suggest = sug)
    if (!pdf) if (is.null(options()$knitr.in.progress))
      .plotList(plot.i, plot.title)
}

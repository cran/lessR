bc.data.frame <-
function(x, n.cat,
         col.fill, col.color, col.bg, col.box,
         col.trans, colors,
         horiz, addtop, gap, prop,
         xlab, ylab, main, lab.cex,
         labels, label.size,
         axis.cex, cex.names, rotate.x, rotate.y, offset, beside,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet,
         width, height, pdf.file, ...)  {


  sug <- getOption("suggest")
  options(suggest = FALSE)

  manage.gr <- .graphman()  # see if graphics are to be managed
  if (manage.gr  &&  is.null(pdf.file)) {
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

        if (nu == 1) {
          cat("\nVariable", x.name, "has only one value.\n",
              "No bar chart produced.\n\n")
        }

        else {

        if (!is.null(pdf.file)) {
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
          col.fill, col.color, col.bg, col.box,
          col.trans, colors,
          horiz, addtop, gap, prop,
          xlab, ylab, main, lab.cex,
          value.labels=NULL, label.size,
          axis.cex, cex.names, rotate.x, rotate.y, offset, beside,
          legend.title, legend.loc, legend.labels, legend.horiz,
          add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL, quiet,
          font.main=1, ...)

        if (!is.null(pdf.file)) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "bar chart")
        }

        if (.is.integer(x[,i]) && nu <= n.cat && !quiet)
          .ncat("bar chart", x.name, nu, n.cat)

        }  # end else
      }

      else cat("\n", names(x)[i], "appears to contain unique Names or IDs\n")
    }

    #else cat("\n", "--- ", names(x)[i], " --- is numerical, better to do a ",
             #"histogram\n\n", sep="")

  }  # each column in x

  options(suggest = sug)
  if (plot.i > 0) {
    if (is.null(pdf.file))
      if (is.null(options()$knitr.in.progress))
        .plotList(plot.i, plot.title)
  }
  else {
    cat("No categorical variables, so no bar charts.\n\n",
        "If you have integer variables that are categorical,\n",
        "  then set the n.cat parameter to the maximum\n",
        "  number of integer values (categories),\n",
        "  or convert them to R factors.\n\n", sep="")
  }

}

bc.data.frame <-
function(x, n_cat, within,
         col_fill, col_color, col.trans, fill_spit, theme,
         horiz, gap, prop, scale_y,
         xlab, ylab, main,
         labels, label.size, beside,
         rotate_x, offset, break_x, sort_x,
         values, values_color, values_cex, value.digits,
         values_position, values_cut,
         xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
         pad_y_min, pad_y_max,
         legend_title, legend_position, legend_labels,
         legend_horiz, legend_size,
         out_size, quiet, width, height, pdf, ...)  {


  sug.keep <- getOption("suggest")
  options(suggest = FALSE)

  manage.gr <- .graphman()  # see if graphics are to be managed
  if (manage.gr  &&  !pdf) {
    i.win <- 0
    for (i in 1:ncol(x)) {
      if (is.numeric(x[,i])  &&  !.is.num.cat(x[,i], n_cat)) 
        i.win <- i.win + 1
    }
    .graphwin(i.win, width, height)
    open.win <- 2
  }

  plot.i <- 0  # keep track of generated graphics
  plot.title  <- character(length=0)

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    if (!is.numeric(x[,i]) || .is.num.cat(x[,i],n_cat)) {
 
      if (nlevels(factor(x[,i])) < length(x[,i])) {

        x.name <- names(x)[i]
        options(xname = x.name)

        if (nu == 1) {
          cat("\nVariable", x.name, "has only one value.\n",
              "No bar chart produced.\n\n")
        }

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

        .bc.main(x[,i], y=NULL, by=NULL, within,
          col_fill, col_color, col.trans, fill_spit, theme,
          horiz, gap, prop, scale_y,
          xlab, ylab, main,
          value_labels=NULL, label.size, beside,
          rotate_x, offset, break_x, sort_x,
          values, values_color, values_cex, value.digits,
          values_position, values_cut,
          xlab_adj, ylab_adj, bm.adj, lm.adj, tm.adj, rm.adj,
          pad_y_min, pad_y_max,
          legend_title, legend_position, legend_labels,
          legend_horiz, legend_size,
          add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL, out_size,
          quiet, ...)

        if (pdf) {
          dev.off()
          if (!quiet) .showfile(pdf.fnm, "bar chart")
        }

        if (.is.integer(x[,i]) && nu <= n_cat && !quiet)
          .ncat("bar chart", x.name, nu, n_cat)

        }  # end else
      }

      else cat("\n", names(x)[i], "appears to contain unique Names or IDs\n")
    }

    #else cat("\n", "--- ", names(x)[i], " --- is numerical, better to do a ",
             #"histogram\n\n", sep="")

  }  # each column in x

  options(suggest = sug.keep)

  if (!pdf) {  # no evaluation for pdf is TRUE
    if (plot.i > 0) {
      if (is.null(options()$knitr.in.progress))
        .plotList(plot.i, plot.title)
    }
    else {
      cat("No categorical variables, so no bar charts.\n\n",
          "If you have integer variables that are categorical,\n",
          "  then set the n_cat parameter to the maximum\n",
          "  number of integer values (categories),\n",
          "  or convert them to R factors.\n\n", sep="")
    }
  }

}


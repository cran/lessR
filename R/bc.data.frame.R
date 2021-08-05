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
         legend_horiz, legend_size, legend_abbrev, legend_adj,
         out_size, quiet, width, height, pdf_file, ...)  {
  sug.keep <- getOption("suggest")
  options(suggest = FALSE)

  manage.gr <- .graphman()  # see if graphics are to be managed
  if (manage.gr && is.null(pdf_file)) {
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

        if (!is.null(pdf_file))  {
          if (!grepl(".pdf", pdf_file))
            pdf_file <- paste(pdf_file, ".pdf", sep="")
          .opendev(pdf_file, width, height)
        }
        else {
          pdf_file <- NULL
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
          legend_horiz, legend_size, legend_abbrev, legend_adj,
          add=NULL, x1=NULL, x2=NULL, y1=NULL, y2=NULL, out_size,
          quiet, ...)

        if (!is.null(pdf_file)) {
          dev.off()
          if (!quiet) .showfile(pdf_file, "bar chart")
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

  if (is.null(pdf_file)) {  # no evaluation
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


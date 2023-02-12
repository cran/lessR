.reg6mod <-
function(lm.out, w.nm, x.nm, 
         digits_d, pdf=FALSE, width=5, height=5, manage.gr=FALSE, ...) {

  nm <- all.vars(lm.out$terms)  # names of vars in the model
  n.vars <- length(nm)
  n.keep <- nrow(lm.out$model)

  # pdf graphics option
  if (pdf) {
    pdf_file <- "mod.pdf"
    pdf(file=pdf_file, width=width, height=height)
  }

  # keep track of the plot in this routine
  plt.i <- 0L
  plt.title  <- character(length=0)

  plt.i <- plt.i + 1L
  plt.title[plt.i] <- "Moderator Variable Interaction Plot"

  max.width=.4
  margs <- .plt.marg(max.width, y.lab=nm[1], x.lab=nm[2], main=NULL, sub=NULL)
  lm <- margs$lm
  tm <- margs$tm
  rm <- margs$rm + .85  # allow for legend
  bm <- margs$bm + .3

  par(bg=getOption("window_fill"))
  orig.params <- par(no.readonly=TRUE)
  on.exit(par(orig.params))
  par(mai=c(bm, lm, tm, rm))

  tx <- character(length = 0)

  mn.x <- min(lm.out$model[, x.nm])
  mx.x <- max(lm.out$model[, x.nm])
  mn.y <- min(lm.out$model[, 1])
  mx.y <- max(lm.out$model[, 1])
  plot(c(mn.x,mx.x), c(mn.y,mx.y), type="n", xlab=x.nm, ylab=nm[1])

  x.ind <- which(names(lm.out$model) == x.nm)
  w.ind <- which(names(lm.out$model) == w.nm)
  b0 <- lm.out$coefficients[1]
  bx <- lm.out$coefficients[x.ind]
  bw <- lm.out$coefficients[w.ind]
  bxw <- lm.out$coefficients[4]

  clr.u1 <- getColors("hues", n=2)[1]  # up 1 sd
  clr.0  <- "gray20"
  clr.d1 <- getColors("hues", n=2)[2]        # down 1 sd
  
  # wc is the constant value of mod W variable, 3 possibilities
  m.w <- mean(lm.out$model[, w.nm])
  s.w <- sd(lm.out$model[, w.nm])
  tx[length(tx)+1] <- paste("Mean of ", w.nm,": ", .fmt(m.w,digits_d), sep="")
  tx[length(tx)+1] <- paste("SD of   ", w.nm,": ", .fmt(s.w,digits_d), sep="")
  tx[length(tx)+1] <- ""

  wc <- m.w+s.w; b.0 <- b0 + bw*wc; b.1 <- bx + bxw*wc
  tx[length(tx)+1] <- paste("mean+1SD for ", w.nm,
         ":  b0=", round(b.0,digits_d), "  b1=", round(b.1,digits_d), sep="")
  abline(b.0, b.1, col=clr.u1, lwd=1.5)

  wc <- m.w; b.0 <- b0 + bw*wc; b.1 <- bx + bxw*wc
  tx[length(tx)+1] <- paste("mean     for ", w.nm,
         ":  b0=", round(b.0,digits_d), "  b1=", round(b.1,digits_d), sep="")
  abline(b.0, b.1, col=clr.0, lwd=1)

  wc <- m.w-s.w; b.0 <- b0 + bw*wc; b.1 <- bx + bxw*wc
  tx[length(tx)+1] <- paste("mean-1SD for ", w.nm,
         ":  b0=", round(b.0,digits_d), "  b1=", round(b.1,digits_d), sep="")
  abline(b.0, b.1, col=clr.d1, lwd=1.5)

  lbls <- c("+1SD", "Mean", "-1SD")
  text.cex <- ifelse(is.null(getOption("axis_x_cex")),
                   getOption("axis_cex"), getOption("axis_x_cex"))
  if (text.cex > 0.99) text.cex <- .7  * text.cex
  clr <- c(clr.u1, clr.0, clr.d1)
  l.typ <- c("solid", "solid", "solid")

  .plt.legend(lbls, FALSE, clr, "blue", "", rgb(.98,.98,.98), par("usr"), 
             legend_title=w.nm, lab_cex=text.cex, line_type=l.typ)


  if (pdf) {
    dev.off()
    .showfile(pdf_file, "moderator interaction plot")
  }

  return(invisible(list(i=plt.i, ttl=plt.title, out_mod=tx)))

}

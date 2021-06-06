simFlips <-
function(n, prob=.5, seed=NULL,
         show_title=TRUE, show_flips=TRUE,
         grid="grey90", pause=FALSE,
         main=NULL, pdf_file=NULL, width=5, height=5, ...) {


  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    change <- c("show.title", "show,flips", "pdf.file")
    for (i in 1:length(dots)) {
      if (names(dots)[i] %in% change) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (missing(n)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Specify the number of flips with:  n\n\n")
  }

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (substr(names(dots)[i], 1, 4) == "col.") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
          "options that began with the abbreviation  col  now begin with  ",
          "color \n\n")
      }
    }
  }

  if (!is.null(pdf_file))
    if (!grepl(".pdf", pdf_file)) pdf_file <- paste(pdf_file, ".pdf", sep="")

  # set up graphics system
  if (!is.null(pdf_file)) {
    if (!grepl(".pdf", pdf_file)) pdf_file <- paste(pdf_file, ".pdf", sep="")
    .opendev(pdf_file, width, height)
  }

  # plot the individual flips and the running mean
  orig.params <- par(no.readonly=TRUE)
  par(mar=c(3,3,1.5,3.5), mgp=c(1.75,.5,0))

  plot(0, type="n", xlim=c(1,n), ylim=c(0,1), xlab="Number of Flips", 
       ylab="Estimate", cex.lab=0.8, cex.axis=0.7)

  # color the plot region between the axes
  usr = par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col="ghostwhite", border="black")

  # grid lines
  vy <- pretty(c(usr[3],usr[4]))
  abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=grid, lwd=.5)

  abline(h=prob, col="lightsteelblue", lwd=2)

  # do the n coin flips and calculate the running mean, ybar
  if (!is.null(seed)) set.seed(seed)
  flips = rbinom(n, 1, prob)  # flip one coin n times
  ybar <- cumsum(flips)/(1:n)

  if (!pause) {
    lines(ybar, type="l", lwd=3, col="gray40")
    if (show_flips) 
      points(1:n, flips, col="lightsteelblue", pch=23, bg="darkblue", cex=.7)
  }
  else {
    if (pause) cat("\n>>> Press Enter to obtain the next sample <<< \n\n")
    for (i in 1:(n)) {
      if (show_flips)
        points(i, flips[i], col="lightsteelblue", pch=23, bg="darkblue", cex=.7)
      return(invisible(readline()))
      segments(i, ybar[i], i+1, ybar[i+1], lwd=3, col="gray40")
    }

  }

  if (show_title) {
    mainlabel <- paste("Sample Mean after", toString(n), "Coin Flips:",
      toString(.fmt(ybar[n],3)), sep=" ")
    title(main=mainlabel, cex.main=.85)
  }
  n.heads <- sum(flips)
  mtext(bquote(paste(" ", mu, "=", .(prob))), side=4, cex=.85,
        col="darkslateblue", las=2, at=c(prob))
  mtext(bquote(paste(" ", .(n.heads), " Heads")), side=4, cex=.7, las=2, at=c(1))
  mtext(bquote(paste(" ", .(n-n.heads), " Tails")), side=4, cex=.7, las=2, at=c(0))

  # terminate pdf graphics system
  if (!is.null(pdf_file)) {
    dev.off()
    .showfile(pdf_file, "coin flips")
  }

}

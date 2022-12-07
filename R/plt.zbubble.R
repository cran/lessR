.plt.bubble <-
function(x, y, size, radius, power, fill, color,
         size_cut, prop, bubble_text, object) {

  cords <- data.frame(x, y, size, stringsAsFactors=TRUE)
  cords <- na.omit(cords)

  # scale for regular R or RStudio
  adj <- .RSadj(radius=radius)  # reg R multiply by 1.6
  radius <- adj$radius

  sz <- cords[,3]**power  # radius unscaled for all the points
  symbols(cords[,1], cords[,2], circles=sz, inches=radius,
          fg=color, bg=fill, add=TRUE)
  mxru <- max(sz)
  sz <- 2 * (sz/mxru) * radius  # scaled diameter

  # text on the bubbles
  if (size_cut  &&  object == "bubble") {  

    # get q.ind before setting too small bubbles for text at NA
    if (size_cut > 1) {
      by.prob <- 1 / (size_cut - 1)
      bub.probs <- seq(0, 1, by.prob)
      qnt <- quantile(cords[,3], probs=bub.probs, type=3, na.rm=TRUE)
      qnt.TF <- logical(length(cords))
      for (i in 1:nrow(cords))  # 3rd column of cords is size
          qnt.TF[i] <- ifelse(cords[i,3] %in% qnt, TRUE, FALSE)
      q.ind <- which(qnt.TF)
    }
    else
      q.ind <- 1:nrow(cords)  # all bubbles get text

    # should get size of each amount just for those displayed (q.ind)
    sz.cex <- numeric(length=nrow(cords))
    for (i in 1:nrow(cords)) {
      sz.cex[i] <- getOption("axis_cex")  # cex target for text size
      # target for text size
      sz.txt <- strwidth(cords[i,3], units="inches", cex=sz.cex[i])
      while ((sz.txt - sz[i]) > -.01) {
        if (sz.cex[i] > 0.45) {  # need cex larger than 0.45
          sz.cex[i] <- sz.cex[i] - 0.02
          sz.txt <- strwidth(cords[i,3], units="inches", cex=sz.cex[i])
        }
        else {
          cords[i,3] <- NA
          break;
        }
      }
      if (options("device") != "RStudioGD")
        sz.cex[i] <- sz.cex[i] * 1.3
    }

    # write bubble text
    if (!prop) {
      if (bubble_text != "transparent")
        text(cords[q.ind,1], cords[q.ind,2], cords[q.ind,3],
          cex=sz.cex[q.ind], col=bubble_text)
    }
    else {
      crd <- .fmt0(cords[,3],2)
      for (j in 1:length(crd))
        if (grepl("NA", crd[j], fixed=TRUE)) crd[j] <- " "
      if (bubble_text != "transparent")
        text(cords[,1], cords[,2], crd, cex=sz.cex, col=bubble_text)
    }
  }  # end (size_cut  &&  object == "bubble")

}

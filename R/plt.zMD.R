.plt.MD <- 
function(x, y, ID, MD_cut, out_cut) {

  m.x <- mean(x, na.rm=TRUE)
  m.y <- mean(y, na.rm=TRUE)
  center <- c(m.x, m.y)
  cov.mat <- cov(matrix(c(x,y), ncol=2), use="complete.obs")

  dst <- numeric(length=length(x))
  for (i in 1:length(x))
    dst[i] <- mahalanobis(c(x[i], y[i]), center, cov.mat)

  if (MD_cut > 0)
    out_ind <- which(dst >= MD_cut)  # absolute threshold
  else if (out_cut > 0  && out_cut < 1)  # a proportion
    out_ind <- which(dst > quantile(dst, 1-out_cut, na.rm=TRUE))
  else if (out_cut >= 1)  { # a count
    out_cut <- min(sort(dst, decreasing=TRUE)[1:out_cut])
    out_ind <- which(dst >= out_cut)
  }


  tx <- character(length=0)

  n.lines <- length(out_ind) + 3  # 3 extra lines to compare MD
  ord <- order(dst, decreasing=TRUE)
  dst.srt <- dst[ord]
  ID.srt <- ID[ord]
  max.ID <- max(nchar(as.character(ID.srt)))
  max.MD <- max(nchar(.fmt(dst.srt, d=2)))
  tx[length(tx)+1] <- ">>> Outlier analysis with Mahalanobis Distance"
  tx[length(tx)+1] <- ""
  tx[length(tx)+1] <- paste(.fmtc("MD", max.MD), .fmtc(" ID", max.ID)) 
  tx[length(tx)+1] <- paste(.fmtc("-----", max.MD), .fmtc("-----", max.ID)) 
  for (i in 1:n.lines) {
    if (i == (length(out_ind)+1)  &&  length(out_ind) > 0)
      tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste(.fmt(dst.srt[i], 2), .fmtc(ID.srt[i], max.ID))
  }
  if (n.lines < length(x))
    tx[length(tx)+1] <- paste(.fmtc("...", max.MD-1), .fmtc("...", max.ID)) 

  return(list(tx.otl=tx, outlpts=out_ind))

}

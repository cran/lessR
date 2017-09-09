showWheel <-
function(clr=NULL, h.beg=0, h.end=300, n.colors=10, c=100, l=65,
         radius=0.9) {

    if (!is.null(clr) && (!missing(h.beg) || !missing(h.end)
              || !missing(n.colors))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Either provide a manual list of colors or specify hcv colors\n",
        "but not both\n\n")
    }

  interval <- 360 %/% n.colors

  if (h.end < h.beg) h.end <- h.end + 360
  h <- seq(h.beg, h.end, by=interval) 
  for (i in 1:length(h)) if (h[i] > 360) h[i] <- h[i] - 360

  if (is.null(clr)) {
    clr <- hcl(h, c, l)
    lbl <- h
    ttl <- paste("HCL Color Wheel for\n",
                 "Chroma=", c, "Luminance=", l)
  }
  else {
    lbl <- clr
    ttl <- ""
  }

  pie(rep(1, length(clr)), col=clr, radius=radius, labels=lbl,
      main=ttl)

  for (i in 1:length(clr)) cat(i, " ", h[i], clr[i], "\n")
  cat("\n") 

}



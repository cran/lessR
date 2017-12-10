showWheel <-
function(clr="discrete", h1=0, h2=NULL, n=10, c=100, l=65, fixup=TRUE,
         radius=0.9, ...) {

    if (length(clr)>1 && (!missing(h1) || !missing(h2)
              || !missing(n))) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Either provide a manual list of colors or specify hcl colors\n",
        "but not both\n\n")
    }

  if (is.null(h2)) h2 <- h1 + (360 * (n - 1)/n)

  h <- seq(h1, h2, length=n) 
  #for (i in 1:length(h)) if (h[i] > 360) h[i] <- h[i] - 360

  if (clr[1] == "discrete") {
    clr <- hcl(h, c, l, fixup=fixup)
    #clr <- hex(polarLUV(L=l, C=c, H=h), fixup=fixup, ...)
    lbl <- .fmt(h, 0)
    ttl <- paste("HCL Color Wheel for\n",
                 "Chroma=", c, "Luminance=", l)
  }
  else {
    lbl <- clr
    ttl <- ""
  }

  pie(rep(1, length(clr)), col=clr, radius=radius, labels=lbl, main=ttl)

  for (i in 1:length(clr)) cat(i, " ", h[i], clr[i], "\n")
  cat("\n") 

}



.plt.fill <-
function(geom_clr, geom_clr.miss, ord.by.call, n.by1, n.lvl, theme) {

  n.gen <- ifelse (n.by1 < n.lvl, n.by1, n.lvl)
  if (n.gen == 0) n.gen <- 1

  if (geom_clr.miss) {  
    if (!ord.by.call) {
      clr <- ifelse (theme == "colors", "hues", getOption("bar_fill"))
      if (clr == "hues")
        geom_clr <- getColors(clr, n=n.gen, output=FALSE)
      else
        for (i in 1:n.lvl) geom_clr[i] <- geom_clr[1] 
    }
    else
      geom_clr <- .color_range(.get_fill(seq.pal=TRUE), n.lvl)  # ordinal
    geom_clr.miss <- FALSE
  } 

  if (n.lvl == 0) n.lvl <- 1  # bit of a hack, better if n.by1==1 is no levels 
  if (!geom_clr.miss  &&  n.lvl > 0)  { # n.lvl = 0 means no Trellis
    if (length(geom_clr) == 1)
       geom_clr <- .color_range(geom_clr, n.lvl)  # from "blues", etc.
    if (length(geom_clr) != n.lvl) {
      n.reps <- ceiling(n.lvl / length(geom_clr))
      x.fill <- rep(geom_clr, n.reps)
      geom_clr <- x.fill[1:n.lvl] 
    }
  }

  return(geom_clr)
}


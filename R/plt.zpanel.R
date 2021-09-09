.panel.bwplot <-
function (x, y, box.ratio=1, vbs_size=box.ratio/(1 + box.ratio), 
    horizontal=TRUE, alpha=trellis.par.get("box.dot")$alpha,
    fill=box.rectangle$fill, varwidth=FALSE,
    vbs_mean=FALSE, mean_color="darkred", fences=FALSE,
    notch=FALSE, notch.frac=0.5, ...,
    levels.fos=if (horizontal) sort(unique(y)) else sort(unique(x)), 
    stats=boxplot.stats, k.iqr=1.5, a=3, b=4,
    do.out=TRUE, identifier="bwplot") { 

    stats.nm <- deparse(substitute(stats))

#   requireNamespace(lattice, quietly=TRUE)  # lattice function panel.number()

    x <- as.numeric(x)
    y <- as.numeric(y)  # could be a factor, need numeric version

    # trellis.par.get("box.rectangle")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")
    fontsize.points <- trellis.par.get("fontsize")$points

    if (!notch) 
      notch.frac <- 0

    if (horizontal) {
      if (stats.nm == "boxplot.stats")
        blist <- tapply(x, factor(y, levels=levels.fos), stats, 
          coef=k.iqr, do.out=do.out)
      if (stats.nm == "adjboxStats")
        blist <- tapply(x, factor(y, levels=levels.fos), stats, 
          coef=k.iqr, a=a, b=b, do.out=do.out)
      blist.stats <- t(sapply(blist, "[[", "stats"))
      blist.out <- lapply(blist, "[[", "out")
      blist.height <- vbs_size
      if (varwidth) {
          maxn <- max(table(y))
          blist.n <- sapply(blist, "[[", "n")
          blist.height <- sqrt(blist.n/maxn) * blist.height
      }
      blist.conf <- if (notch) 
          t(sapply(blist, "[[", "conf"))
      else blist.stats[, c(2, 4), drop=FALSE]
      xbnd <- cbind(blist.stats[, 3], blist.conf[, 2], blist.stats[, 4],
         blist.stats[, 4], blist.conf[, 2], blist.stats[, 3],
         blist.conf[, 1], blist.stats[, 2], blist.stats[, 2],
         blist.conf[, 1], blist.stats[, 3])
      ytop <- levels.fos + blist.height/2
      ybot <- levels.fos - blist.height/2
      ybnd <- cbind(ytop - notch.frac * blist.height/2, ytop, 
          ytop, ybot, ybot, ybot + notch.frac * blist.height/2, 
          ybot, ybot, ytop, ytop, ytop - notch.frac * blist.height/2)
      xs <- cbind(xbnd, NA_real_)
      ys <- cbind(ybnd, NA_real_)

      # box
      # col parameter specifies fill, only accepts one color, ignores others
      # panel.xxx is basically a do-loop, iterating over panels
       panel.polygon(t(xs), t(ys), lwd=box.rectangle$lwd, 
          lty=box.rectangle$lty, col=fill[panel.number()],
          alpha=box.rectangle$alpha, 
          border=box.rectangle$col,
          identifier=paste(identifier, "box", sep="."))

      # stems
      panel.segments(
          c(blist.stats[, 2], blist.stats[, 4]), rep(levels.fos, 2),
          c(blist.stats[, 1], blist.stats[, 5]), rep(levels.fos, 2),
          col=box.umbrella$col, alpha=box.umbrella$alpha,
          lwd=box.umbrella$lwd, lty=box.umbrella$lty,
          identifier=paste(identifier, "whisker", sep="."))

      # whiskers
      panel.segments(
          c(blist.stats[, 1], blist.stats[, 5]), levels.fos - blist.height/2,
          c(blist.stats[, 1], blist.stats[, 5]), levels.fos + blist.height/2, 
          col=box.umbrella$col, alpha=box.umbrella$alpha, 
          lwd=box.umbrella$lwd, lty=box.umbrella$lty,
          identifier=paste(identifier, "cap", sep="."))
          
       if (fences) {
           iqr <- blist.stats[, 4] - blist.stats[, 2]
           m.c <- ifelse(stats.nm == "adjboxStats", mc(x, na.rm=TRUE), 0)
            if (m.c >= 0) {  # equations from ?adjboxStats
              lo30 <- blist.stats[, 2] - 2 * k.iqr * exp(a*m.c) * iqr
              lo15 <- blist.stats[, 2] - k.iqr * exp(a*m.c) * iqr
              up15 <- blist.stats[, 4] + k.iqr * exp(b*m.c) * iqr
              up30 <- blist.stats[, 4] + 2 * k.iqr * exp(b*m.c) * iqr
            }
            else {  # m.c < 0
              lo30 <- blist.stats[, 2] - 2 * k.iqr * exp(-b*m.c) * iqr
              lo15 <- blist.stats[, 2] - k.iqr * exp(-b*m.c) * iqr
              up15 <- blist.stats[, 4] + k.iqr * exp(-a*m.c) * iqr
              up30 <- blist.stats[, 4] + 2 * k.iqr * exp(-a*m.c) * iqr
            } 

        s.t <- getOption("sub_theme")
        fence_color <- ifelse (s.t == "black", "gray85", box.umbrella$col)
        panel.segments(
            c(lo15, up15), levels.fos - blist.height/2,
            c(lo15, up15), levels.fos + blist.height/2, 
            col=fence_color, alpha=box.umbrella$alpha, 
            lwd=box.umbrella$lwd, lty="dotted",
            identifier=paste(identifier, "cap", sep="."))
      }  # end fences

      panel.segments(  # median
          blist.stats[, 3], levels.fos - blist.height/2,
          blist.stats[, 3], levels.fos + blist.height/2,
          lwd=box.rectangle$lwd, lty=box.rectangle$lty,
          col=box.rectangle$col, alpha=alpha,
          identifier=paste(identifier, "dot", sep="."))

      if (vbs_mean) {
        mx <- mean(x, na.rm=TRUE)
        panel.segments(  # mean
            mx, levels.fos - blist.height/2,
            mx, levels.fos + blist.height/2,
            lwd=2, lty=box.rectangle$lty,
            col=mean_color, alpha=alpha,
            identifier=paste(identifier, "dot", sep="."))
      }

    }  # end horizontal

}

# need to manually adjust the size of y downward because x is a subset
# cannot call panel.xyplot directly because formal arg y has mult args
.panel.stripplot <-
function (x, y, jitter_data=FALSE, factor=0.5, amount=NULL, 
    horizontal=TRUE, groups=NULL, ..., identifier="stripplot") {

    panel.xyplot(x=x, y=y[1:length(x)], jitter.x=jitter_data && !horizontal, 
        jitter.y=jitter_data && horizontal, factor=factor, 
        amount=amount, horizontal=horizontal,
        ..., identifier=identifier)
}


# for .bar.lattice
# panel.densityplot with an added line for panel.xyarea, parameters fill, color
#panel.dnFill <-
#function (x, fill, color, darg=list(n=512), plot.points="jitter", ref=FALSE, 
    #groups=NULL, weights=NULL, jitter_amount=0.01 * diff(current.panel.limits()$ylim), 
    #type="p", ..., identifier="density") 
#{
    #if (ref) {
        #reference.line <- trellis.par.get("reference.line")
        #panel.abline(h=0, col=reference.line$col, lty=reference.line$lty, 
            #lwd=reference.line$lwd, identifier=paste(identifier, 
                #"abline"))
    #}
    #if (!is.null(groups)) {
        #panel.superpose(x, darg=darg, plot.points=plot.points, 
            #ref=FALSE, groups=groups, weights=weights, 
            #panel.groups=panel.densityplot, jitter_amount=jitter_amount, 
            #type=type, ...)
    #}
    #else {
        #switch(as.character(plot.points), `TRUE`=panel.xyplot(x=x, 
            #y=rep(0, length(x)), type=type, ..., identifier=identifier), 
            #rug=panel.rug(x=x, start=0, end=0, x.units=c("npc", 
                #"native"), type=type, ..., identifier=paste(identifier, 
                #"rug")), jitter=panel.xyplot(x=x, y=jitter(rep(0, 
                #length(x)), amount=jitter_amount), type=type, 
                #..., identifier=identifier))
        #density.fun <- function(x, weights, subscripts=TRUE, 
            #darg, ...) {
            #do.call("density", c(list(x=x, weights=weights[subscripts]), 
                #darg))
        #}
        #if (sum(!is.na(x)) > 1) {
            #h <- density.fun(x=x, weights=weights, ..., darg=darg)
            #lim <- current.panel.limits()$xlim
            #id <- h$x > min(lim) & h$x < max(lim)
            #panel.lines(x=h$x[id], y=h$y[id], col=color, ..., identifier=identifier)
            #panel.xyarea(x=h$x[id], y=h$y[id], col=fill, ..., identifier=identifier)
        #}
    #}
#}


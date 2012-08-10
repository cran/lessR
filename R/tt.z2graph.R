.TwoGraph <-
function(YA, YB, bw1, bw2, Ynm, Xnm, X1nm, X2nm, digits.d, brief,
         n1, m1, s1, n2, m2, s2, mdiff, sw, smd, mmd, msmd,
         clpct, tvalue, pvalue, ub, lb, deltaL, deltaU) {

    dYA <- density(YA, bw1)
    dYB <- density(YB, bw2)

    if (!brief) {
      cat("\n\n------ Graphics Smoothing Parameter ------\n\n")
      mytitle <- "Density bandwidth for "
      cat(mytitle, Xnm, " ", X1nm, ": ", .fmt(dYA$bw,digits.d), sep="", "\n")
      cat(mytitle, Xnm, " ", X2nm, ": ", .fmt(dYB$bw,digits.d), sep="", "\n\n")
    }

    cat("--------------------------------------------------\n")

# values needed for graph
    min.x <- min(min(dYA$x),min(dYB$x))  # min x coordinate for graph
    max.x <- max(max(dYA$x),max(dYB$x))  # max x coordinate for graph
    max.y <- max(max(dYA$y),max(dYB$y))  # max y coordinate
    max.y <- max.y+.1*max.y  # allow room in graph region for d info

# colors
    col.1 <- rgb(.63,.46,.15)
    col.1t <- rgb(.63,.46,.15, alpha=.5)
    col.2 <- rgb(.49,.56,.69)
    col.2t <- rgb(.49,.56,.69, alpha=.5)

# plot: set up coordinate system
    .graphwin(1)
    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))
    par(mar=c(3,1.5,8,.4), mgp=c(2,.6,0), cex=.8, cex.axis=1, cex.lab=1.2)
    plot.new()
    plot.window(xlim=c(min.x,max.x), ylim=c(0,max.y))
    axis(1); box()
    title(xlab=Ynm)

    xleft <- par("usr")[1]  # left side of graph
    xright <- par("usr")[2]  # right side of graph
    ybot <- par("usr")[3]  # bottom of graph
    ytop <- par("usr")[4]  # height of graph

# vertical line for mean
    lines(c(m1,m1), c(0,ytop), lty="solid", lwd=2, col=col.1)
    lines(c(m2,m2), c(0,ytop), lty="twodash", lwd=2, col=col.2)

# curve area
    polygon(c(min(dYA$x),dYA$x,max(dYA$x)), c(0,dYA$y,0), col=col.1t, border=NA, 
        density=10, angle=45)
    polygon(c(min(dYB$x),dYB$x,max(dYB$x)), c(0,dYB$y,0), col=col.2t, border=NA, 
        density=10, angle=-45)

# bottom border of density curve  
    segments(min(dYA$x), 0, max(dYA$x), 0, col=col.1)
    segments(min(dYB$x), 0, max(dYB$x), 0, col=col.2)

# density curve
    lines(dYA, col=col.1t, lty="solid", lwd=1.5)
    lines(dYB, col=col.2t, lty="twodash", lwd=1.5)

# minimum mean difference of practical importance
    if ( !is.null(mmd) | !is.null(msmd) ) {
      col.e <- "gray50"  # color for effect
      mid <- (m1 + m2) / 2
      lr <- mid + .5*mmd  # line right
      ll <- mid - .5*mmd  # line left
      lines(c(lr,lr), c(ybot+.44*max.y,ytop-.44*max.y), lty="solid", lwd=2, col=col.e)
      lines(c(ll,ll), c(ybot+.44*max.y,ytop-.44*max.y), lty="solid", lwd=2, col=col.e)
      text(mid, ybot+.41*max.y, label=toString(round(mmd,2)), col=col.e)
      text(mid, ytop-.41*max.y, label=toString(round(msmd,2)), col=col.e)
      text(mid, ybot+.38*max.y, label="mmd", col=col.e)
      text(mid, ytop-.375*max.y, label="msmd", col=col.e)
    }

# legends with descriptive stats (m1 > m2)
    textR <- paste(Xnm,X1nm)
    nR <- n1
    mR <- .fmt(m1,digits.d)
    sR <- .fmt(s1,digits.d)
    col.R <- col.1
    aR <- 45

    textL <- paste(Xnm,X2nm)
    nL <- n2
    mL <- .fmt(m2,digits.d)
    sL <- .fmt(s2,digits.d)
    col.L <- col.2
    aL <- -45

    col.lgnd <- "gray25"
    cex.lgnd <- .9

    radj <- xleft + .02*(max.x-min.x)
    legend("topleft", legend = textL, fill=col.L, density=20, angle=aL, bty="n",
        text.col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.10*max.y, label=bquote(paste("n = ", .(nL))),
        adj=0, col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.14*max.y, label=bquote(paste("m = ", .(.fmtc(mL,digits.d)))),
        adj=0, col=col.lgnd, cex=cex.lgnd)
    text(radj, ytop-.18*max.y, label=bquote(paste("s = ", .(.fmtc(sL,digits.d)))),
        adj=0, col=col.lgnd, cex=cex.lgnd)

    ladj <- xright - .02*(xright-xleft)
    legend("topright", legend = textR, fill=col.R, density=20, angle=aR, bty="n",
        text.col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.10*max.y, label=bquote(paste("n = ", .(nR))),
        adj=1, col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.14*max.y, label=bquote(paste("m = ", .(.fmtc(mR,digits.d)))),
        adj=1, col=col.lgnd, cex=cex.lgnd)
    text(ladj, ytop-.18*max.y, label=bquote(paste("s = ", .(.fmtc(sL,digits.d)))),
        adj=1, col=col.lgnd, cex=cex.lgnd)

# scale for s-pooled, d, mdiff at top of graph
      mlow <- min(m1, m2)
      mhi  <- max(m1, m2)
      col.d.unit <- "gray30"
# connect first seg to top
      segments(mlow, max.y-.01*max.y, mlow, ytop, lwd=1, col=col.d.unit) 
# provide at least 2 labeled d units on sd scale at top
      max.i <- max(ceiling(abs(smd)), 2)
      for (i in 0:max.i) {  # sd scale at top
        x.i <- mlow+i*sw
# sd units
        segments(x.i, max.y+.025*max.y, x.i, ytop, col=col.d.unit, lwd=1)
# d units counted
        text(x.i, max.y+.01*max.y, labels=i)
# horiz bar connects endpoints
        segments(mlow, ytop, x.i, ytop, col=col.d.unit, lwd=4)
        last.coord.x <- x.i
      }
# connect last seg to top
    segments(last.coord.x, max.y+.025*max.y, last.coord.x, ytop, lwd=1, col=col.d.unit)
# print d value towards top
      text((m1+m2)/2, ytop-.07*max.y, label=.fmt(smd))
# horiz bar connects means
      segments(mlow, ytop-.09*max.y, mhi, ytop-.09*max.y, col=col.d.unit, lwd=2)
# print d towards top
      text((m1+m2)/2, ytop-.11*max.y, label="smd")

# print mdiff value towards bottom  
      text((m1+m2)/2, ybot+.11*max.y, label=.fmt(mdiff))
# horiz bar connects means
      segments(mlow, ybot+.09*max.y, mhi, ybot+.09*max.y, col=col.d.unit, lwd=2)
# print diff towards bottom
      text((m1+m2)/2, ybot+.07*max.y, label="md")

    # title area, above graph
    mtext(paste("TwoGroup Plot"), side=3, line=6.6, font=2)
    mtext(paste("Compare",Ynm,"for",Xnm,X1nm,"and",X2nm), side=3, line=5.6, font=3, cex=.8)
    mtext(bquote(paste("  Classic t-test of 0 mean diff:   t = ", .(.fmt(tvalue,3)), 
      ",  df = ", .(df), ",   p-value = ", .(.fmt(pvalue,3)))), side=3, 
      line=4.4, cex=.8, adj=0)
    mtext(bquote(paste("  ",.(clpct), " Confidence Interval for Mean Difference:  ",
      .(.fmt(lb,3)), " to ", .(.fmt(ub,3)))), side=3, line=3.3, cex=.8, adj=0)
    mtext(bquote(paste("  ",.(clpct), " Confidence Interval for Stnd Mean Diff:   ", 
      .(.fmt(deltaL,3)), " to ", .(.fmt(deltaU,3)))), side=3, line=2.1, cex=.8, adj=0)
    mtext(bquote(paste("s-within")), side=3, line=.9, 
          at=(mlow+(last.coord.x))/2, col="gray40", cex=.8)
    mtext(bquote(paste(.(round(sw,2)))), side=3, line=.2,
          at=(mlow+(last.coord.x))/2, col="gray40", cex=.8)

}

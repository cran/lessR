# Plot tails of normal and t for specified alpha
prob.t <- 
function(df, alpha=0.05, dig.dec=3, y.axis=FALSE,
         col.fill="aliceblue", col.tail="palevioletred4",
         col.nrm=gray(.7), col.t=gray(.08), ...) {

    
if (df < 2) { 
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "The parameter  df  must be 2 or larger.\n\n")
}

orig.params <- par(no.readonly=TRUE)
on.exit(par(orig.params))

xmin <- -5.5
xmax <- 5.5

x1min <- xmin
x2max <- xmax

par(mar=c(4, 3, 1, 2), mgp=c(2,.6,0))
#-------------
# t Dist
#-------------
x <- seq(xmin,xmax,length=200)
y <- dt(x, df=df)
plot(x,y, type="n", ylim=c(0,.42), axes=FALSE, lwd=2, las=1, main="", 
     xlab="Standard Errors from Zero", ylab="")

axis(side=1)
if (y.axis) {
  axis(side=2)
  ylab="Density"
  title(ylab=ylab)
}


usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], 
     col=rgb(249,249,252,maxColorValue=255), border="black")

# curve and fill
x1max <- qt(0.025, df=df)
x2min <- qt(0.025, df=df, lower.tail=FALSE)
lines(x,y, type="l", lwd=4, col=col.t, las=1, main="", xlab="z", ylab="")
polygon(c(x1max,x,x2min), c(0,y,0), col=col.fill, border="transparent")
# rgb(.91, .95, .98)

# Left Tail
x1max <- qt(alpha/2, df=df)
x <- seq(x1min,x1max,by=.01)
y <- dt(x,df=df)
polygon(c(x1min,x,x1max), c(0,y,0), col=col.tail, border=col.nrm)
lines(c(x1max,x1max), c(0,.19), col=col.tail)
text(x1max, .20, col=col.t, cex=1.1,
  format(sprintf("%.*f", dig.dec, x1max), width=5, justify="right", sep=""))

# Right Tail
x2min <- qt(1-alpha/2, df=df)
x <- seq(x2min,x2max,by=.01)
y <- dt(x,df=df)
polygon(c(x2min,x,x2max), c(0,y,0), col=col.tail, border=col.nrm)
lines(c(x2min,x2min), c(0,.19), col=col.tail)
text(x2min, .20, col=col.t, cex=1.1,
  format(sprintf("%.*f", dig.dec, x2min), width=5, justify="right", sep=""))

#-------------
# Normal Dist
#-------------
x1max <- qnorm(alpha/2)
x2min <- qnorm(1-alpha/2)
x <- seq(xmin,xmax,length=200)
y <- dnorm(x)
lines(x,y, type="l", lwd=2, col=col.nrm, las=1, main="", xlab="z", ylab="")
abline(h=0)

# fill
txtht <- 0.175
rlbl <- paste(toString(round(100*(1-alpha),0)), "%", sep="")
text(0, txtht, rlbl, cex=1.4, col=col.nrm)

# Left Tail
lines(c(x1max,x1max), c(0,.25), col=col.nrm)
text(x1max, .26, col=col.nrm, cex=1.1,
  format(sprintf("%.*f", dig.dec, x1max), width=5, justify="right", sep=""))

# Right Tail
lines(c(x2min,x2min), c(0,.25), col=col.nrm)
text(x2min, .26, col=col.nrm, cex=1.1,
  format(sprintf("%.*f", dig.dec, x2min), width=5, justify="right", sep=""))

#-------------
# Legend
#-------------
dflbl <- paste("t, df=",toString(df),sep="")
legend("topright", c("Normal", dflbl), lwd=c(1,1.5), col=c(col.nrm,col.t), box.lwd=.5, cex=.75)

}


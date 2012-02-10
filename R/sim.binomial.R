sim.binomial <-
function(n, prob=.5, show.title=TRUE, pdf.out=FALSE,
         show.flips=TRUE, col.grid="grey90", pause=FALSE) {

options(scipen=10)  # bias to avoid scientific notation

# plot the individual flips and the running mean
filename = paste("coinflips",toString(n),".pdf",sep="")
if (pdf.out) pdf(file=filename, width=5.5, height=2.25)

orig.params <- par(no.readonly=TRUE)
par(mar=c(3,3,1.5,3.5), mgp=c(1.75,.5,0))

plot(0, type="n", xlim=c(1,n), ylim=c(0,1), xlab="Number of Flips", 
     ylab="Estimate", cex.lab=.8, cex.axis=.7)

# color the plot region between the axes
usr = par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col="ghostwhite", border="black")

# grid lines
vy <- pretty(c(usr[3],usr[4]))
abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

abline(h=prob, col="lightsteelblue", lwd=2)

# do the n coin flips and calculate the running mean, ybar
ybar = numeric(n)
nHead = 0
sum = 0
y = rbinom(n, 1, prob)  # flip one coin n times
for (i in 1:n) {
  if (y[i]==1) nHead = nHead + 1
  sum = sum + y[i]
  ybar[i] = sum/i
}

mainlabel = paste("Sample Mean after", toString(n), "Coin Flips:", toString(round(ybar[n],3)), sep=" ")
if (show.title) title(main=mainlabel, cex.main=.85)
mtext(bquote(paste(" ", mu, "=", .(prob))), side=4, cex=.85, col="darkslateblue", las=2, at=c(prob))
mtext(bquote(paste(" ", .(nHead), " Heads")), side=4, cex=.7, las=2, at=c(1))
mtext(bquote(paste(" ", .(n-nHead), " Tails")), side=4, cex=.7, las=2, at=c(0))

if (!pause) {
  lines(ybar, type="l", lwd=3, col="coral3")
  if (show.flips) 
    points(1:n, y, col="lightsteelblue", pch=23, bg="darkblue", cex=.7)
}
else {
  if (pause) cat("\n>>> Press Enter to obtain the next sample <<< \n\n")
  for (i in 1:(n)) {
    if (show.flips)
      points(i, y[i], col="lightsteelblue", pch=23, bg="darkblue", cex=.7)
    invisible(readline())
    segments(i, ybar[i], i+1, ybar[i+1], lwd=3, col="coral3")
  }
}
if (pdf.out) dev.off()

par(orig.params)

}

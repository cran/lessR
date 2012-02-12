sim.mean <- 
function(nrep, n, mu=0, sigma=1, ylim.bound=NULL, 
         show.title=TRUE, show.pop=FALSE, show.data=TRUE, 
         col.grid="grey90", pdf.out=FALSE, pause=FALSE) {

if (sigma < 0) { 
  cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Standard deviation, sigma, cannot be negative.\n\n")
}
        
# data generation
data.raw <- rnorm(nrep*n, mu, sigma)
max.Y <- max(data.raw)
min.Y <- min(data.raw)
data.byrep <- matrix(data.raw, nrep, n)

# summary stats
Ymean <- apply(data.byrep, 1, mean)
Ysd <- apply(data.byrep, 1, sd)

# keep mu centered with symmetric upper and lower limits on y-axis
if (is.null(ylim.bound)) {
  l <- min.Y
  u <- max.Y
  max.dev <- max( abs(mu-l), abs(u-mu) )
  l <- mu - max.dev
  u <- mu + max.dev
}
else {
  l <- mu - ylim.bound
  u <- mu + ylim.bound
}


if (pdf.out)
  pdf(file=paste("meanSim", toString(round(100*(1-alpha),0)),".pdf",sep=""))

# plot 
plot(0, type = "n", xlim=c(1,nrep), ylim = c(l,u), xlab = "", ylab = "Sample Mean", 
     cex.main=.95, cex.axis=.8)

# colored plotting area
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col="ghostwhite", border="black")

# grid lines
vx <- pretty(c(usr[1],usr[2]))
vy <- pretty(c(usr[3],usr[4]))
abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

if (show.title) title(main = bquote(paste(mu, "=", .(mu), "  ", sigma, "=", .(sigma), "  ",
   "  n=", .(n))), cex.main=1)
if (show.pop) {
  mtext(bquote(paste(" ", mu)), side=4, cex=1.5, col="darkslateblue", las=2)
  abline(h=mu, col="darkslateblue", lwd=1.5) # horizontal centerline at mu
}

# directions
if (pause) cat("\n>>> Press Enter to obtain the next sample <<< \n\n")

# text output
cat("\nSample", "  Mean   ")
if (show.data) for (i in 1:n) cat(format(toString(i), width=8, justify="right", sep=""))
cat("\n")
dig.dec <- 3
max.ln <- 8
for (i in 1:nrep) {
  if (pause) invisible(readline())
  cat(format(i, width=5, justify="right", sep=""))
  cat(format(sprintf("%.*f", dig.dec, Ymean[i]), width=max.ln, justify="right", sep=""))
  cat("   ")
  if (show.data) for (j in 1:n) 
    cat(format(sprintf("%.*f", dig.dec, data.byrep[i,j]), width=max.ln, justify="right", sep=""))
  cat("\n")
  points(i, Ymean[i], pch=21, col="darkblue", bg="plum")
}


if (pause) cat("\n")

if (pdf.out) dev.off()

cat("\nStructure\n")
if (show.pop) {
  cat("Population mean, mu :", mu, "\n")
  cat("Pop std dev, sigma  :", sigma, "\n")
}
cat("Number of samples   :", nrep, "\n")
cat("Size of each sample :", n, "\n")

cat("\nAnalysis of Sample Means\n")
cat("   Mean:", format(sprintf("%.*f", dig.dec, mean(Ymean)), width=max.ln, justify="right", sep=""), "\n")
cat("Std Dev:", format(sprintf("%.*f", dig.dec, sd(Ymean)), width=max.ln, justify="right", sep=""), "\n")

cat("\n")
}

sim.CImean <- 
function(ns, n, mu=0, sigma=1, cl=0.95, 
         ylim.bound=NULL, show.data=FALSE, show.title=TRUE, 
         miss.only=FALSE, col.hit="gray40", col.miss="red",
         col.grid="grey90", pause=FALSE) {

if (sigma < 0) { 
  cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Standard deviation, sigma, cannot be negative.\n\n")
}

orig.params <- par(no.readonly=TRUE)
on.exit(par(orig.params))        

alpha <- 1-cl
tcut <- qt(1-alpha/2, df=n-1)
clpct <- round((cl)*100, 2)

# data generation
data.raw <- rnorm(ns*n, mu, sigma)
max.Y <- max(data.raw)
min.Y <- min(data.raw)
data.byrep <- matrix(data.raw, ns, n)

# summary stats
Ymean <- apply(data.byrep, 1, mean)
Ysd <- apply(data.byrep, 1, sd)

# confidence intervals
E = tcut * Ysd/sqrt(n)
lb <- Ymean - E
ub <- Ymean + E

# performance
miss <- sum((lb > mu) + (ub < mu))
missrate <- round((miss/ns)*100, 2)

# keep mu centered with symmetric upper and lower limits on y-axis
if (is.null(ylim.bound)) {
  if (!show.data) {
    l <- min(lb)
    u <- max(ub)
  }
  else {
    l <- min.Y
    u <- max.Y
  }
  max.dev <- max( abs(mu-l), abs(u-mu) )
  l <- mu - max.dev
  u <- mu + max.dev
}
else {
  l <- mu - ylim.bound
  u <- mu + ylim.bound
}


# plot setup
par(mar=c(2,2,1.75,2), mgp=c(1,.5,0))

plot(lb, type = "n", ylim = c(l,u), xlab = "", ylab = "", cex.main=.95, cex.axis=.8)
if (show.title) title(main = bquote(paste(mu, "=", .(mu), "  ", sigma, "=", .(sigma), "  ",
   "cl=", .(clpct), "%  n=", .(n))), cex.main=1)

mtext(bquote(paste(" ", mu)), side=4, cex=1.5, col="darkslateblue", las=2)

# color the plot region between the axes
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col="ghostwhite", border="black")

# grid lines
vy <- pretty(c(usr[3],usr[4]))
abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)

 # directions
if (pause) cat("\n>>> Press Enter to obtain the next sample <<< \n\n")

# plot confidence intervals
cat("\nSample", "  Mean", " StdDev", " StdErr", "  Error", "     LB", "     UB", "\n")
abline(h=mu, col="darkslateblue", lwd=1.5) # horizontal centerline at mu
dig.dec <- 3
max.ln <- 8
for (i in 1:ns) {
  if (pause) invisible(readline())
  if ( (mu>lb[i] && mu<ub[i]) ) linecol <- col.hit else linecol = col.miss
  if (show.data) points(rep(i,n), data.byrep[i,], pch=21, col="gray75", cex=.3)
  if ( !(miss.only && linecol==col.hit) ) {
    se <- Ysd[i]/sqrt(n)
    e <- tcut * se
    cat(format(i, width=5, justify="right", sep=""))
    cat(format(sprintf("%.*f", dig.dec, Ymean[i]), width=max.ln, justify="right", sep=""))
    cat(format(sprintf("%.*f", dig.dec, Ysd[i]), width=max.ln, justify="right", sep=""))
    cat(format(sprintf("%.*f", dig.dec, se), width=max.ln, justify="right", sep=""))
    cat(format(sprintf("%.*f", dig.dec, E[i]), width=max.ln, justify="right", sep=""))
    cat(format(sprintf("%.*f", dig.dec, lb[i]), width=max.ln, justify="right", sep=""))
    cat(format(sprintf("%.*f", dig.dec, ub[i]), width=max.ln, justify="right", sep=""))
    if (linecol == "red")  cat("  *** MISS ***")
    if (!pause) cat("\n")
  }
  segments(i, lb[i], i, ub[i], col=linecol, lwd=1.5)
  segments(i-.2, Ymean[i], i+.2, Ymean[i], col=linecol, lwd=1)
  segments(i-.2, lb[i], i+.2, lb[i], col=linecol, lwd=1)
  segments(i-.2, ub[i], i+.2, ub[i], col=linecol, lwd=1)
}
if (pause) cat("\n")

cat("\nStructure\n")
cat("Population mean, mu :", mu, "\n")
cat("Pop std dev, sigma  :", sigma, "\n")
cat("Number of samples   :", ns, "\n")
cat("Size of each sample :", n, "\n")
cat("Confidence level    :", cl, "\n")


cat("\nPerformance\n")
cat("Number of Misses: ", miss, "\n")
cat("Percent Misses  :", format(sprintf("%.*f", 2, missrate), width=5, justify="right", sep=""), "\n")

dig.dec <- dig.dec + 1
cat("\nAnalysis of Sample Means\n")
cat("   Mean:", format(sprintf("%.*f", dig.dec, mean(Ymean)), width=max.ln, justify="right", sep=""), "\n")
cat("Std Dev:", format(sprintf("%.*f", dig.dec, sd(Ymean)), width=max.ln, justify="right", sep=""), "\n")

cat("\n")

}

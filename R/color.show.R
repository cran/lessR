color.show <- 
function(file="colors.pdf", color=NULL) {

pdf(file=file)

par(mfrow=c(5,6), mgp=c(0,1,0))

if (is.null(color))
  clr <- colors()
else
  clr <- grep(color, colors(), value = TRUE)
  
h <- 1
for (i in 1:length(clr))
	barplot(h, col=clr[i], main=clr[i], sub=toString(col2rgb(clr[i])), 
		 cex.main=.95, axes=FALSE)

if (getwd() =="/")
  workdir <- "top level of your file system"
else
  workdir <- getwd()
cat("pdf file written in current working directory.\n")
cat("       ", file, "at:  ", workdir, "\n")

par(mfrow=c(1,1), mgp=c(3,1,0))

dev.off()

}

.plt.mat <-
function(x, coef=TRUE, fit="loess",
         col.fill=getOption("fill.pt"), col.stroke=getOption("stroke.pt"),
         col.fit=getOption("stroke.bar"), col.bg=getOption("bg"),
         col.box=getOption("box")) {

  if (is.null(fit)) fit <- "loess"

  n.var <- ncol(x)

  panel.smooth <- function (x, y, fit.line=fit, pch=par("pch"), cex=.76,
    col.pt=col.stroke, col.smooth=col.fit, span=2/3, iter=3, ...)
 {
    usr <- par("usr")          
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
    points(x, y, pch=21, col=col.pt, bg=col.fill, cex=cex)

    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (fit.line == "loess") {
        l.ln <- lowess(y[ok] ~ x[ok], f=span, iter=iter)
        lines(lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
      }
      else if (fit.line == "ls") {
        l.ln <- lm(y[ok] ~ x[ok])
        lines(x[ok], fitted(l.ln), col=col.smooth, ...)
      }
    }
  }

  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr=c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- .fmt(r, 2)
    txt <- paste(prefix, txt, sep="")
    if (missing(cex.cor)) cex.cor <- .9/strwidth(txt)
    cex.adj <- 2.5 - (0.18*n.var)  # adjust size of displayed r
    text(0.5, 0.5, txt, cex=cex.adj, col=col.stroke)  # or cex=cex.cor * r
  }

  # -----
  # begin

  if (coef)  # no missing data
    pairs(na.omit(x), lower.panel=panel.smooth, upper.panel=panel.cor)
  else
    pairs(na.omit(x), panel=panel.smooth)

}

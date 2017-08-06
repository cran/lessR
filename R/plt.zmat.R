.plt.mat <-
function(x, cor.coef=TRUE, fit="loess",
         col.fill=getOption("pt.fill"), col.color=getOption("pt.color"),
         col.fit=getOption("bar.color"), col.bg=getOption("panel.fill"),
         col.box=getOption("panel.color")) {


  if (is.null(fit)) fit <- "loess"

  if (getOption("sub.theme") == "black") col.color <- getOption("lab.color")

  n.var <- ncol(x)

  par(bg=getOption("window.fill"))

  panel.smooth <- function (x, y, fit.line=fit, pch=par("pch"), cex=.76,
    col.pt=col.color, col.smooth=col.fit, span=2/3, iter=3, ...)
 {
    usr <- par("usr")          
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
    cex.adj <- 0.86 - 0.045*n.var
    points(x, y, pch=21, col=col.pt, bg=col.fill, cex=cex.adj)

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
    rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
    par(usr=c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- .fmt(r, 2)
    txt <- paste(prefix, txt, sep="")
    cex.adj <- 2.25 - (0.10*n.var)  # adjust size of displayed r
    text(0.5, 0.5, txt, cex=cex.adj, col=col.color)  # or cex=cex.cor * r
  }

  text.diag <- function(x, y, nm, ...) {  # nm from calling routine
    usr <- par("usr")          
    rect(usr[1], usr[3], usr[2], usr[4], col=getOption("ellipse.fill"),
         border=col.box)
    #rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border=col.box)
    txt <-  nm  # nm from parameter list, so adjusts for each panel
    cex.adj <- 2.25 - (0.10*n.var)  # adjust size of displayed r
    #cex.adj <- ifelse (n.var < 8, 1.8, 1.5)  # adjust size of displayed r
    text(0.5, 0.5, txt, cex=cex.adj, col=getOption("lab.color"))
  }

  # -----
  # begin

  if (cor.coef)  # no missing data
    pairs(na.omit(x), lower.panel=panel.smooth, upper.panel=panel.cor,
      text.panel=text.diag)
  else
    pairs(na.omit(x), panel=panel.smooth, text.panel=text.diag)

}

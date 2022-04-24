.plt.mat <-
function(x, cor.coef=TRUE, fit="loess",
         col_fill=getOption("pt_fill"), col_color=getOption("pt_color"),
         col.fit=getOption("fit_color"), col.bg=getOption("panel_fill"),
         col.box=getOption("panel_color"),
         col_trans=getOption("trans_pt_fill"),
         pt.size=pt.size, size.miss=size.miss) {


  panel.smooth <- function(x, y, fit.line=fit, pch=par("pch"), cex=.76,
    col.pt=col_color, col.smooth=col.fit, span=2/3, iter=3, ...) {

    my.usr <- par("usr")          
    rect(my.usr[1], my.usr[3], my.usr[2], my.usr[4], col=col.bg, border=col.box)
    cex.adj <- ifelse(size.miss, 0.80 - 0.048*n.var, pt.size)
    points(x, y, pch=21, col=col.pt, bg=col_fill, cex=cex.adj)

    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (fit.line %in% c("loess", "lm")) {
        yo <- order(x[ok])  # need to order x, then have y follow
        x[ok] <- sort(x[ok])
        y[ok] <- y[ok][yo]
        if (fit.line == "loess") {
          l.ln <- loess(y[ok] ~ x[ok])
          lines(lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
        }
        else if (fit.line == "lm") {
          l.ln <- lm(y[ok] ~ x[ok])
          lines(x[ok], fitted(l.ln), col=col.smooth, ...)
        }
        f.ln <- fitted(l.ln, ...)
        p.ln <- predict(l.ln, se=TRUE) # get standard errors
        prb <- (1 - 0.95) / 2 # 0.95 confidence level
        up.ln <- f.ln + (qt(prb,length(x[ok])-1) * p.ln$se.fit)
        dn.ln <- f.ln - (qt(prb,length(x[ok])-1) * p.ln$se.fit)
        polygon(c(x[ok], rev(x[ok])), c(dn.ln, rev(up.ln)),
          col=getOption("se_fill"), border="transparent") #
      }
    }
  }


  panel.cor <- function(x, y, ...) {
    my.usr <- par("usr"); on.exit(par(usr = my.usr))
    rect(my.usr[1], my.usr[3], my.usr[2], my.usr[4], col=col.bg, border=col.box)
    par(usr=c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- .fmt(r, 2)
    # maybe use par("fin")[1] / n.var to get size of each cell
    cex.adj <- 1.274 - (0.033*n.var)  # adjust size of displayed r
    text(0.5, 0.5, txt, cex=cex.adj, col="black")  # or cex=cex.cor * r
  }


  text.diag <- function(x, y, nm, ...) {  # nm from calling routine
    my.usr <- par("usr")          
    rect(my.usr[1], my.usr[3], my.usr[2], my.usr[4], col=getOption("se_fill"),
         border=col.box)
    cex.adj <- 1.274 - (0.033*n.var)  # adjust size of displayed var name
    text(0.5, 0.5, nm, cex=cex.adj, col=getOption("lab_color"))
  }

  # -----
  # begin

  if (is.logical(fit)) if (fit) fit <- "loess"

  if (getOption("sub_theme") == "black")
     col_color <- getOption("lab_color")

  n.var <- ncol(x)

  par(bg=getOption("window_fill"))

  if (!is.null(col_trans))
    col_fill <- .maketrans(col_fill, (1-col_trans)*256)

  if (cor.coef)  # no missing data
    pairs(na.omit(x), lower.panel=panel.smooth, upper.panel=panel.cor,
      text.panel=text.diag)
  else
    pairs(na.omit(x), panel=panel.smooth, text.panel=text.diag)

}

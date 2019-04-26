.plt.add <- 
function(add, x1, x2, y1, y2,
         add_cex, add_lwd, add_lty, add_color, add_fill, add_trans, ...) { 

  n.obj <- length(add)
  n.stk <- length(add_color)
  n.fll <- length(add_fill)
  n.lty <- length(add_lty)
  n.lwd <- length(add_lwd)
  n.cex <- length(add_cex)
  n.trn <- length(add_trans)
  i.stk <- 0;  i.fll <- 0;  i.lty <- 0;  i.lwd <- 0;  i.cex <- 0;  i.trn <- 0
  x1i <- 0;  x2i <- 0;  y1i <- 0;  y2i <- 0;  # coordinate indexes

  usr <- par("usr") 

  for (i in 1:n.obj) {

    i.stk <- i.stk + 1;  if (i.stk > n.stk) i.stk <- 1
    i.fll <- i.fll + 1;  if (i.fll > n.fll) i.fll <- 1
    i.lty <- i.lty + 1;  if (i.lty > n.lty) i.lty <- 1
    i.lwd <- i.lwd + 1;  if (i.lwd > n.lwd) i.lwd <- 1
    i.cex <- i.cex + 1;  if (i.cex > n.cex) i.cex <- 1
    i.trn <- i.trn + 1;  if (i.trn > n.trn) i.trn <- 1

    if (!is.null(add_trans[i.trn])) {
      if (add[i] %in% c("text", "line", "v_line", "h_line", "arrow"))
        add_color[i.stk] <- .maketrans(add_color[i.stk], (1-add_trans[i.trn])*256)
      if (add[i] %in% c("rect"))
        add_fill[i.fll] <- .maketrans(add_fill[i.fll], (1-add_trans[i.trn])*256)
    }

    if (!(add[i] %in% c("rect", "line", "v_line", "h_line", "arrow"))) {  # text
      if (n.obj == 1) {
        xx <- x1; yy <- y1  # same object, mult locations
      }  
      else {
        x1i <- x1i + 1;  y1i <- y1i + 1
        xx <- x1[x1i];  yy <- y1[y1i]  # multiple objects, multiple locations
      }
      text(xx, yy, labels=add[i], cex=add_cex[i.cex], col=add_color[i.stk], ...)
    }

    else if (add[i] == "v_line") {
      if (n.obj == 1)
        xx <- x1  # same object, mult locations 
      else {
        x1i <- x1i + 1
        xx <- x1[x1i]  # multiple objects, multiple locations
      }
      segments(xx, usr[3], xx, usr[4], col=add_color[i.stk],
               lwd=add_lwd[i.lwd], lty=add_lty[i.lty], ...)
    }

    else if (add[i] == "h_line") {
      y1i <- y1i + 1
      yy <- y1[y1i]  # multiple objects, multiple locations
      if (n.obj == 1) yy <- y1  # same object, mult locations
      segments(usr[1], yy, usr[2], yy, col=add_color[i.stk],
               lwd=add_lwd[i.lwd], lty=add_lty[i.lty], ...)
    }

    else if (add[i] %in% c("line", "rect", "arrow")) {  # 4 coordinates
      if (n.obj == 1) {
        xx1 <- x1;  yy1 <- y1;  xx2 <- x2;  yy2 <- y2;
      } 
      else {
        x1i <- x1i + 1;  x2i <- x2i + 1;  y1i <- y1i + 1;  y2i <- y2i + 1
        xx1 <- x1[x1i];  xx2 <- x2[x2i];  yy1 <- y1[y1i];  yy2 <- y2[y2i]
      }
      if (add[i] == "line")
        segments(xx1, yy1, xx2, yy2, col=add_color[i.stk],
                 lwd=add_lwd[i.lwd], lty=add_lty[i.lty], ...)
      if (add[i] == "rect")
        rect(xx1, yy1, xx2, yy2, border=add_color[i.stk], col=add_fill[i.fll],
             lwd=add_lwd[i.lwd], lty=add_lty[i.lty], ...)
      if (add[i] == "arrow")
        arrows(xx1, yy1, xx2, yy2, col=add_color[i.stk], 
             lwd=add_lwd[i.lwd], lty=add_lty[i.lty], ...)
    }

  }  # end i in 1:n.obj

} 


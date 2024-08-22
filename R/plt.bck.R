.plt.bck <-
function(usr, axV, axT2, do.v=TRUE, do.h=TRUE) {

  # background color
  rect(usr[1], usr[3], usr[2], usr[4],
       col=getOption("panel_fill"), border="transparent")

  # grid lines
  grid_x_color <- ifelse(is.null(getOption("grid_x_color")),
    getOption("grid_color"), getOption("grid_x_color"))
  grid_y_color <- ifelse(is.null(getOption("grid_y_color")),
    getOption("grid_color"), getOption("grid_y_color"))

  grid_x_lwd <- ifelse(is.null(getOption("grid_x_lwd")),
    getOption("grid_lwd"), getOption("grid_x_lwd"))
  grid_y_lwd <- ifelse(is.null(getOption("grid_y_lwd")),
    getOption("grid_lwd"), getOption("grid_y_lwd"))

  grid_x_lty <- ifelse(is.null(getOption("grid_x_lty")),
    getOption("grid_lty"), getOption("grid_x_lty"))
  grid_y_lty <- ifelse(is.null(getOption("grid_y_lty")),
    getOption("grid_lty"), getOption("grid_y_lty"))

  if (do.v) if (grid_x_lwd > 0)
    abline(v=axV, col=grid_x_color, lwd=grid_x_lwd, lty=grid_x_lty)

  if (do.h) if (grid_y_lwd > 0)
    abline(h=axT2, col=grid_y_color, lwd=grid_y_lwd, lty=grid_y_lty)

  # box around plot
  rect(usr[1], usr[3], usr[2], usr[4],
    col="transparent", border=getOption("panel_color"),
    lwd=getOption("panel_lwd"), lty=getOption("panel_lty"))
}

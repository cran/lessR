.style.show <-
function() {

    cat("\nAvailable Themes\n")
    cat("----------------\n")
    cat("lightbronze", " dodgerblue", " darkred", " gray", " gold",
          " darkgreen", "\n",
        "blue", " red", " rose", " green", " purple", " sienna", " brown",
        " orange", " white", "\n", sep="")

    cat("\nAvailable Sub-themes\n")
    cat("--------------------\n")
    cat("default", "black", "no.y.axis", "\n\n")

    cat("THEME\n")
    cat("theme ........ Theme color .......", getOption("theme"), "\n")
    cat("sub.theme .... Sub-theme style ...", getOption("sub.theme"), "\n")

    cat("\n")
    cat("BACKGROUND\n")
    cat("window.fill .. Window fill color .........",
        .to_rgb(getOption("window.fill")), "\n")
    cat("panel.fill ... Panel fill color ..........",
        .to_rgb(getOption("panel.fill")), "\n")
    cat("panel.color .. Panel border color ....... ",
        .to_rgb(getOption("panel.color")), "\n")
    cat("panel.lwd .... Panel border line width .. ",
        .fmt(getOption("panel.lwd"), 1), "\n")
    cat("panel.lty .... Panel border line type ... ",
        getOption("panel.lty"), "\n")

    cat("\n")
    cat("DATA OBJECTS\n")
    cat("bar.fill ......... Bar fill color ............",
        .to_rgb(getOption("bar.fill")), "\n")
    cat("trans.bar.fill ... Bar fill transparency .....",
        .fmt(getOption("trans.bar.fill"), 2), "\n")
    cat("bar.color ........ Bar border color ..........",
        .to_rgb(getOption("bar.color")), "\n")
    cat("pt.fill .......... Point fill color ..........",
        .to_rgb(getOption("pt.fill")), "\n")
    cat("trans.pt.fill .... Point fill transparency .. ",
        .fmt(getOption("trans.pt.fill"), 2), "\n")
    cat("pt.color ......... Point border color ....... ",
        .to_rgb(getOption("pt.color")), "\n")
    cat("out.fill ......... Outlier point fill ....... ",
        .to_rgb(getOption("out.fill")), "\n")
    cat("out2.fill ........ Extreme outlier point fill ",
        .to_rgb(getOption("out2.fill")), "\n")
    cat("violin.fill ...... Violin fill color .........",
        .to_rgb(getOption("violin.fill")), "\n")
    cat("violin.color ..... Violin border color .......",
        .to_rgb(getOption("violin.color")), "\n")
    cat("box.fill ......... Boxplot fill color ........",
        .to_rgb(getOption("box.fill")), "\n")
    cat("box.color ........ Boxplot border color ......",
        .to_rgb(getOption("box.color")), "\n")
    cat("fit.color ........ Fit line color ........... ",
        .to_rgb(getOption("fit.color")), "\n")
    cat("se.fill .......... Stnd error fill color .... ",
        .to_rgb(getOption("se.fill")), "\n")
    cat("ellipse.fill ..... Ellipse fill color ....... ",
        .to_rgb(getOption("ellipse.fill")), "\n")
    cat("ellipse.color .... Ellipse border color ..... ",
        .to_rgb(getOption("ellipse.color")), "\n")
    cat("ellipse.lwd ...... Ellipse border color ..... ",
        .fmt(getOption("ellipse.lwd"), 2), "\n")
    cat("area.fill ........ Line chart area fill color ",
        .to_rgb(getOption("area.fill")), "\n")
    cat("bubble.text.color  Bubble text color .........",
        .to_rgb(getOption("bubble.text.color")), "\n")
    cat("segment.color .... Line segment color ........",
        getOption("segment.color"), "\n")
    cat("heat ............. Heat map color ............",
        .to_rgb(getOption("heat")), "\n")

    cat("\n")
    cat("AXES\n")
    cat("axis.color ..... Color of axes ..............",
        .to_rgb(getOption("axis.color")), "\n")
    cat("axis.x.color ... Color of x-axis ............",
        .to_rgb(getOption("axis.x.color")), "\n")
    cat("axis.y.color ... Color of y-axis ............",
        .to_rgb(getOption("axis.y.color")), "\n")

    cat("axis.lwd ....... Axis line width ............", 
      .fmt(getOption("axis.lwd"), 1), "\n")
    cat("axis.x.lwd ..... Axis line width ............", 
      .fmt(getOption("axis.x.lwd"), 1), "\n")
    cat("axis.y.lwd ..... Axis line width ............", 
      .fmt(getOption("axis.y.lwd"), 1), "\n")

    cat("axis.lty ....... Axis line type .............",
      getOption("axis.lty"), "\n")
    cat("axis.x.lty ..... Axis line type .............", 
      getOption("axis.x.lty"), "\n")
    cat("axis.y.lty ..... Axis line type .............", 
      getOption("axis.y.lty"), "\n")

    cat("axis.cex ....... x and y axis text size ...........",
        .fmt(getOption("axis.cex"), 2), "\n")
    cat("axis.x.cex ..... x-axis text size .................",
        .fmt(getOption("axis.x.cex"), 2), "\n")
    cat("axis.y.cex ..... y-axis text size .................",
        .fmt(getOption("axis.y.cex"), 2), "\n")
    cat("axis.text.color  x and y axis values text color ...",
        .to_rgb(getOption("axis.text.color")), "\n")
    cat("axis.x.text.color  x-axis values text color .......",
        .to_rgb(getOption("axis.x.text.color")), "\n")
    cat("axis.y.text.color  y-axis values text color .......",
        .to_rgb(getOption("axis.y.text.color")), "\n")
    cat("rotate.x ....... Rotation of x axis text ..........",
        .fmt(getOption("rotate.x"), 2), "\n")
    cat("rotate.y ....... Rotation of y axis text ..........",
        .fmt(getOption("rotate.y"), 2), "\n")
    cat("offset ......... Offset of values text from axis ..",
        .fmt(getOption("offset"), 2), "\n")
    cat("lab.color ...... Color of axis labels .............",
        .to_rgb(getOption("lab.color")), "\n")
    cat("lab.x.color .... Color of x-axis label ............",
        .to_rgb(getOption("lab.x.color")), "\n")
    cat("lab.y.color .... Color of y-axis label ............",
        .to_rgb(getOption("lab.y.color")), "\n")
    cat("lab.cex ........ Size of axis labels ..............",
        .fmt(getOption("lab.cex"),2), "\n")
    cat("main.color ..... Color of plot label ..............",
        getOption("main.color"), "\n")

    cat("\n")
    cat("GRID LINES\n")
    cat("grid.color .... Grid color ......................",
        .to_rgb(getOption("grid.color")), "\n")
    cat("grid.x.color .. Grid color, vertical ............",
        .to_rgb(getOption("grid.x.color")), "\n")
    cat("grid.y.color .. Grid color, horizontal ..........",
        .to_rgb(getOption("grid.y.color")), "\n")

    cat("grid.lwd ...... Grid line width .................",
        .fmt(getOption("grid.lwd"), 1), "\n")
    cat("grid.x.lwd .... Grid line width, vertical .......",
        .to_rgb(getOption("grid.x.lwd")), "\n")
    cat("grid.y.lwd .... Grid line width, horizontal .....",
        .to_rgb(getOption("grid.y.lwd")), "\n")

    cat("grid.lty ...... Grid line type ..................",
        getOption("grid.lty"), "\n")
    cat("grid.x.lty .... Grid line type, vertical ........",
        .to_str(getOption("grid.x.lty")), "\n")
    cat("grid.y.lty .... Grid line type, horizontal ......",
        .to_str(getOption("grid.y.lty")), "\n")

    cat("\n")
    cat("TRELLIS STRIP\n")
    cat("strip.fill ...... Trellis strip fill color .....",
        .to_rgb(getOption("strip.fill")), "\n")
    cat("strip.color ..... Trellis strip border color ...",
        .to_rgb(getOption("strip.color")), "\n")
    cat("strip.text.color  Trellis strip text color .....",
        .to_rgb(getOption("strip.text.color")), "\n")

    cat("\n")
    cat("ANNOTATION\n")
    cat("add.fill  .. Fill color of annotated figures ..",
        .to_rgb(getOption("add.fill")), "\n")
    cat("add.trans .. Transparency of fill .............",
        getOption("add.lty"), "\n")
    cat("add.color .. Color of annotated lines .........",
        getOption("add.color"), "\n")
    cat("add.cex  ... Size of annotated text ...........",
        .fmt(getOption("add.cex"), 2), "\n")
    cat("add.lwd  ... Line width of annotated lines ....",
        .fmt(getOption("add.lwd"), 1), "\n")
    cat("add.lty  ... Line type of annotated lines .....",
        getOption("add.lty"), "\n")

    cat("\n")
    cat("NON-GRAPHICAL\n")
    cat("quiet ..... Suppress console output for many functions ..",
        getOption("quiet"), "\n")
    cat("brief ..... Reduce console output for many functions ....",
        getOption("brief"), "\n")
    cat("suggest ... Suggestions for enhanced input ..............",
        getOption("suggest"), "\n")
    cat("width ..... Column width ................................",
        getOption("width"), "\n")
    cat("n.cat ..... Largest number of unique, equally spaced\n",
        "           integer values of a variable for which the\n",
        "           variable will be analyzed as categorical ....",
        getOption("n.cat"), "\n")
    cat("\n")

}

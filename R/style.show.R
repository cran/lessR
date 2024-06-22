.style.show <-
function() {

    cat("\nAvailable Themes\n")
    cat("----------------\n")
    cat("colors", "lightbronze",  "dodgerblue", "darkred", "gray", "gold",
        "darkgreen", "blue", "red", "rose", "slatered", "green", "purple",
        "sienna", "brown", "orange", "white", "light", "\n", fill=60)

    cat("\nAvailable Sub-themes\n")
    cat("--------------------\n")
    cat("default", "black", "wsj", "\n\n")

    cat("THEME\n")
    cat("theme ........ Theme color .......", getOption("theme"), "\n")
    cat("sub_theme .... Sub-theme style ...", getOption("sub_theme"), "\n")

    cat("\n")
    cat("BACKGROUND\n")
    cat("window_fill .. Window fill color .........",
        .to_rgb(getOption("window_fill")), "\n")
    cat("panel_fill ... Panel fill color ..........",
        .to_rgb(getOption("panel_fill")), "\n")
    cat("panel_color .. Panel border color ....... ",
        .to_rgb(getOption("panel_color")), "\n")
    cat("panel_lwd .... Panel border line width .. ",
        .fmt(getOption("panel_lwd"), 1), "\n")
    cat("panel_lty .... Panel border line type ... ",
        getOption("panel_lty"), "\n")

    cat("\n")
    cat("DATA OBJECTS\n")
    cat("bar_fill ......... Bar fill color ............",
        .to_rgb(getOption("bar_fill")), "\n")
    cat("trans_bar_fill ... Bar fill transparency .....",
        .fmt(getOption("trans_bar_fill"), 2), "\n")
    cat("bar_color ........ Bar border color ..........",
        .to_rgb(getOption("bar_color")), "\n")
    cat("labels ........... Form of bar or pie labels .",
        .to_str(getOption("labels")), "\n")
    cat("labels_color ..... Color of bar or pie labels .",
        .to_rgb(getOption("labels_color")), "\n")
    cat("labels_size ..... Size of labels on bars, pie ",
        .to_num(getOption("labels_cex")), "\n")
    cat("labels_digits .... Decimal digits on bars, pie ",
        .to_num(getOption("labels_digits")), "\n")
    cat("labels_position .... Position of labels ...... ..",
        .to_str(getOption("labels_position")), "\n")

    cat("\n")
    cat("pt_fill .......... Point fill color ..........",
        .to_rgb(getOption("pt_fill")), "\n")
    cat("trans_pt_fill .... Point fill transparency .. ",
        .fmt(getOption("trans_pt_fill"), 2), "\n")
    cat("pt_color ......... Point border color ....... ",
        .to_rgb(getOption("pt_color")), "\n")
    cat("out_fill ......... Outlier point fill ....... ",
        .to_rgb(getOption("out_fill")), "\n")
    cat("out_color ........ Outlier point color ...... ",
        .to_rgb(getOption("out_fill")), "\n")
    cat("out2_fill ........ Extreme outlier point fill ",
        .to_rgb(getOption("out2_fill")), "\n")
    cat("out2_color ....... Extreme outlier point color",
        .to_rgb(getOption("out2_fill")), "\n")
    cat("violin_fill ...... Violin fill color .........",
        .to_rgb(getOption("violin_fill")), "\n")
    cat("violin_color ..... Violin border color .......",
        .to_rgb(getOption("violin_color")), "\n")
    cat("box_fill ......... Boxplot fill color ........",
        .to_rgb(getOption("box_fill")), "\n")
    cat("box_color ........ Boxplot border color ......",
        .to_rgb(getOption("box_color")), "\n")
    cat("fit_color ........ Fit line_color ........... ",
        .to_rgb(getOption("fit_color")), "\n")
    cat("se_fill .......... Stnd error fill color .... ",
        .to_rgb(getOption("se_fill")), "\n")
    cat("ellipse_fill ..... Ellipse fill color ....... ",
        .to_rgb(getOption("ellipse_fill")), "\n")
    cat("ellipse_color .... Ellipse border color ..... ",
        .to_rgb(getOption("ellipse_color")), "\n")
    cat("ellipse_lwd ...... Ellipse border width ..... ",
        .fmt(getOption("ellipse_lwd"), 2), "\n")
    cat("bubble_text_color  Bubble text color .........",
        .to_rgb(getOption("bubble_text_color")), "\n")
    cat("segment_color .... Line segment color ........",
        getOption("segment_color"), "\n")
    cat("heat ............. Heat map color ............",
        .to_rgb(getOption("heat")), "\n")

    cat("\n")
    cat("AXES\n")
    cat("axis_color ..... Color of axes ..............",
        .to_rgb(getOption("axis_color")), "\n")
    cat("axis_x_color ... Color of x-axis ............",
        .to_rgb(getOption("axis_x_color")), "\n")
    cat("axis_y_color ... Color of y-axis ............",
        .to_rgb(getOption("axis_y_color")), "\n")

    cat("axis_lwd ....... Axis line width ............", 
      .to_num(getOption("axis_lwd")), "\n")
    cat("axis_x_lwd ..... Axis line width ............", 
      .to_num(getOption("axis_x_lwd")), "\n")
    cat("axis_y_lwd ..... Axis line width ............", 
      .to_num(getOption("axis_y_lwd")), "\n")

    cat("axis_lty ....... Line type of axes ..........",
      .to_str(getOption("axis_lty")), "\n")
    cat("axis_x_lty ..... Line type of x-axis ........", 
      .to_str(getOption("axis_x_lty")), "\n")
    cat("axis_y_lty ..... Line type of y-axis ........", 
      .to_str(getOption("axis_y_lty")), "\n")

    cat("axis_cex ....... x and y axis text size ...........",
        .to_num(getOption("axis_cex"), 2), "\n")
    cat("axis_x_cex ..... x-axis text size .................",
        .to_num(getOption("axis_x_cex"), 2), "\n")
    cat("axis_y_cex ..... y-axis text size .................",
        .to_num(getOption("axis_y_cex"), 2), "\n")
    cat("axis_text_color  x and y axis values text color ...",
        .to_rgb(getOption("axis_text_color")), "\n")
    cat("axis_x_text_color  x-axis values text color .......",
        .to_rgb(getOption("axis_x_text_color")), "\n")
    cat("axis_y_text_color  y-axis values text color .......",
        .to_rgb(getOption("axis_y_text_color")), "\n")
    cat("rotate_x ....... Rotation of x axis text ..........",
        .fmt(getOption("rotate_x"), 2), "\n")
    cat("rotate_y ....... Rotation of y axis text ..........",
        .fmt(getOption("rotate_y"), 2), "\n")
    cat("offset ......... Offset of values text from axis ..",
        .fmt(getOption("offset"), 2), "\n")

    cat("\n")
    cat("LABELS\n")
    cat("lab_color ...... Color of axis labels .............",
        .to_rgb(getOption("lab_color")), "\n")
    cat("lab_x_color .... Color of x-axis label ............",
        .to_rgb(getOption("lab_x_color")), "\n")
    cat("lab_y_color .... Color of y-axis label ............",
        .to_rgb(getOption("lab_y_color")), "\n")
    cat("lab_cex ........ Size of axis labels ..............",
        .to_num(getOption("lab_cex"),2), "\n")
    cat("lab_x_cex ...... Size of x-axis labels ............",
        .to_num(getOption("lab_x_cex"),2), "\n")
    cat("lab_y_cex ...... Size of y-axis labels ............",
        .to_num(getOption("lab_y_cex"),2), "\n")
    cat("main_color ..... Color of plot label ..............",
        getOption("main_color"), "\n")
    cat("main_cex ....... Size of plot title ...............",
        .to_num(getOption("main_cex"),2), "\n")

    cat("\n")
    cat("GRID LINES\n")
    cat("grid_color .... Grid color ......................",
        .to_rgb(getOption("grid_color")), "\n")
    cat("grid_x_color .. Grid color, vertical ............",
        .to_rgb(getOption("grid_x_color")), "\n")
    cat("grid_y_color .. Grid color, horizontal ..........",
        .to_rgb(getOption("grid_y_color")), "\n")

    cat("grid_lwd ...... Grid line width .................",
        .fmt(getOption("grid_lwd"), 1), "\n")
    cat("grid_x_lwd .... Grid line width, vertical .......",
        .to_rgb(getOption("grid_x_lwd")), "\n")
    cat("grid_y_lwd .... Grid line width, horizontal .....",
        .to_rgb(getOption("grid_y_lwd")), "\n")

    cat("grid_lty ...... Grid line type ..................",
        getOption("grid_lty"), "\n")
    cat("grid_x_lty .... Grid line type, vertical ........",
        .to_str(getOption("grid_x_lty")), "\n")
    cat("grid_y_lty .... Grid line type, horizontal ......",
        .to_str(getOption("grid_y_lty")), "\n")

    cat("\n")
    cat("TRELLIS STRIP\n")
    cat("strip_fill ...... Trellis strip fill color .....",
        .to_rgb(getOption("strip_fill")), "\n")
    cat("strip_color ..... Trellis strip border color ...",
        .to_rgb(getOption("strip_color")), "\n")
    cat("strip_text_color  Trellis strip text color .....",
        .to_rgb(getOption("strip_text_color")), "\n")

    cat("\n")
    cat("ANNOTATION\n")
    cat("add_fill  .. Fill color of annotated figures ..",
        .to_rgb(getOption("add_fill")), "\n")
    cat("add_trans .. Fill transparency ................",
        getOption("add_trans"), "\n")
    cat("add_color .. Color of annotated lines .........",
        getOption("add_color"), "\n")
    cat("add_cex  ... Size of annotated text ...........",
        .fmt(getOption("add_cex"), 2), "\n")
    cat("add_lwd  ... Line width of annotated lines ....",
        .fmt(getOption("add_lwd"), 1), "\n")
    cat("add_lty  ... Line type of annotated lines .....",
        getOption("add_lty"), "\n")

    cat("\n")
    cat("NON-GRAPHICAL\n")
    cat("quiet ..... Suppress console output for many functions ..",
        getOption("quiet"), "\n")
    cat("brief ..... Reduce console output for many functions ....",
        getOption("brief"), "\n")
    cat("suggest ... Suggestions for enhanced input ..............",
        getOption("suggest"), "\n")
    cat("note ...... Notes for enhanced input ....................",
        getOption("note"), "\n")
    cat("width ..... Column width ................................",
        getOption("width"), "\n")
    cat("n_cat ..... Largest number of unique, equally spaced\n",
        "           integer values of a variable for which the\n",
        "           variable will be analyzed as categorical ....",
        getOption("n_cat"), "\n")
    cat("\n")

}

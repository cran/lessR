style <-
function(
  theme=c("lightbronze", "darkred", "dodgerblue", "gray", "gold",
      "darkgreen", "blue", "red", "rose", "green", "purple", "sienna",
      "brown", "orange", "white"),
  sub.theme=c("default", "black", "no.y.axis"),

  device.fill=getOption("device.fill"),
  bg.fill=getOption("bg.fill"), bg.stroke=getOption("bg.stroke"),
  bg.lwd=getOption("bg.lwd"), bg.lty=getOption("bg.lty"),

  fill=NULL, bar.fill=getOption("bar.fill"), pt.fill=getOption("pt.fill"),
  trans=NULL, trans.bar.fill=getOption("trans.bar.fill"),
  trans.pt.fill=getOption("trans.pt.fill"),
  stroke=NULL, bar.stroke=getOption("bar.stroke"),
  pt.stroke=getOption("pt.stroke"),
  se.fill=getOption("se.fill"), ellipse.fill=getOption("ellipse.fill"),
  bubble.fill=getOption("bubble.fill"),
  bubble.text.stroke=getOption("bubble.text.stroke"),
  heat=getOption("heat"), 

  cex.axis=getOption("cex.axis"),
  axis.x.stroke=getOption("axis.x.stroke"),
  axis.y.stroke=getOption("axis.y.stroke"),
  values.stroke=getOption("values.stroke"),
  rotate.x=getOption("rotate.x"),
  rotate.y=getOption("rotate.y"),
  offset=getOption("offset"),

  grid.stroke=NULL,
  grid.x.stroke=getOption("grid.x.stroke"),
  grid.y.stroke=getOption("grid.y.stroke"),
  grid.lwd=getOption("grid.lwd"), grid.lty=getOption("grid.lty"),
  lab.stroke=getOption("lab.stroke"),
  lab.size=getOption("lab.size"),
  main.stroke=getOption("main.stroke"),
  segment.stroke=getOption("segment.stroke"),

  add.stroke=getOption("add.stroke"),
  add.cex=getOption("add.cex"),
  add.lwd=getOption("add.lwd"),
  add.lty=getOption("add.lty"),
  add.fill=getOption("pt.fill"),

  n.cat=getOption("n.cat"), suggest=getOption("suggest"),
  quiet=getOption("quiet"), brief=getOption("brief"),

  results=getOption("results"), explain=getOption("explain"),
  interpret=getOption("interpret"), document=getOption("document"), 
  code=getOption("code"),

  width=120, show=FALSE, ...) {


  miss.theme <- ifelse (missing(theme), TRUE, FALSE)

  miss.sub.theme <- ifelse (missing(sub.theme), TRUE, FALSE)
  sub.theme <- match.arg(sub.theme)

  miss.tr.bar.fill <- ifelse (missing(trans.bar.fill), TRUE, FALSE)

  dots <- list(...)  # check for deprecated parameters
  if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "colors") {
        cat("\n"); stop(call.=FALSE, "\n","------\n",
            "option  colors  is renamed  theme\n\n")
      }
    }
    if (names(dots)[i] == "ghost") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "invoke option  ghost  as style(sub.theme=\"black\")\n\n")
    }
    if (names(dots)[i] == "gray.black") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "now invoke style(\"gray\", sub.theme=\"black\")\n\n")
    }
  }


  # reset all parameters to start-up condition for new theme
  if (!miss.theme) {
    suppressPackageStartupMessages(.onAttach())
    miss.theme <- FALSE
    theme <- match.arg(theme) 
    options(theme=theme)
    options(trans.bar.fill=0.10)
    options(trans.pt.fill=0.40) 
  }
  else
    theme <- getOption("theme")

  if (!miss.sub.theme)
    options(sub.theme=sub.theme)
  else
    sub.theme <- getOption("sub.theme")


  # inheritance
  if (!missing(trans)) {
    trans.pt.fill <- trans;
    trans.bar.fill <- trans;
  }
  if (!missing(fill)) {
    pt.fill <- fill;
    bar.fill <- fill;
  }
  if (!missing(stroke)) {
    pt.stroke <- stroke;
    bar.stroke <- stroke;
  }
  if (!missing(grid.stroke)) {
    grid.x.stroke <- grid.stroke;
    grid.y.stroke <- grid.stroke;
  }

  # "off" is "transparent"
  if (bar.fill[1] == "off") bar.fill <- "transparent"
  if (pt.fill[1] == "off") pt.fill <- "transparent"
  if (bar.stroke[1] == "off") bar.stroke <- "transparent"
  if (pt.stroke[1] == "off") bar.stroke <- "transparent"
  if (se.fill[1] == "off") se.fill <- "transparent"
  if (ellipse.fill[1] == "off") ellipse.fill <- "transparent"
  if (bg.fill == "off") bg.fill <- "transparent"
  if (bubble.fill == "off") bubble.fill <- "transparent"
  if (grid.x.stroke == "off") grid.x.stroke <- "transparent"
  if (grid.y.stroke == "off") grid.y.stroke <- "transparent"
  if (grid.lwd == "off") grid.lwd <- "transparent"
  if (grid.lty == "off") grid.lty <- "transparent"
  if (device.fill == "off") device.fill <- "white"
  if (bg.stroke == "off") bg.stroke <- "transparent"
  if (bg.lwd == "off") bg.lwd <- "transparent"
  if (bg.lty == "off") bg.lty <- "transparent"
  if (lab.stroke == "off") lab.stroke <- "transparent"
  if (main.stroke == "off") main.stroke <- "transparent"
  if (axis.x.stroke == "off") axis.x.stroke <- "transparent"
  if (axis.y.stroke == "off") axis.y.stroke <- "transparent"
  if (segment.stroke == "off") segment.stroke <- "transparent"
  if (bubble.text.stroke == "off") bubble.text.stroke <- "transparent"
  if (add.cex == "off") add.cex <- "transparent"
  if (add.lwd == "off") add.lwd <- "transparent"
  if (add.lty == "off") add.lty <- "transparent"
  if (add.stroke == "off") add.stroke <- "transparent"
  if (add.fill == "off") add.fill <- "transparent"

# default transparency levels
  if (!is.null(trans.bar.fill)) {
    options(trans.bar.fill=trans.bar.fill)
    options(bar.fill = .maketrans(getOption("bar.fill"), .to256("trans.bar.fill")))
  }
  if (!is.null(trans.pt.fill)) {
    options(trans.pt.fill=trans.pt.fill)
    options(pt.fill = .maketrans(getOption("pt.fill"), .to256("trans.pt.fill")))
  }

  if (!is.null(bar.fill))
    if (bar.fill[1] == "transparent")
      options(bar.fill = bar.fill) 
    else
      options(bar.fill = .maketrans(bar.fill, .to256("trans.bar.fill")))
  if (!is.null(pt.fill))
    if (pt.fill[1] == "transparent")
      options(pt.fill = pt.fill) 
    else
      options(pt.fill = .maketrans(pt.fill, .to256("trans.pt.fill")))


  # set the options
  options(bar.stroke = bar.stroke) 
  options(pt.stroke = pt.stroke) 
  options(bubble.fill = bubble.fill) 

  options(device.fill=device.fill)
  options(bg.fill=bg.fill)
  options(bg.stroke=bg.stroke)

  options(grid.x.stroke=grid.x.stroke)
  options(grid.y.stroke=grid.y.stroke)
  options(grid.lwd=grid.lwd)
  options(grid.lty=grid.lty)
  options(grid.lty=grid.lty)

  options(bg.lwd=bg.lwd)
  options(bg.lty=bg.lty)
  options(lab.stroke=lab.stroke)
  options(main.stroke=main.stroke)
  options(axis.x.stroke=axis.x.stroke)
  options(axis.y.stroke=axis.y.stroke)
  options(segment.stroke=segment.stroke)
  options(bubble.text.stroke=bubble.text.stroke)

  options(cex.axis=cex.axis)
  options(values.stroke=values.stroke)
  options(rotate.x=rotate.x)
  options(rotate.y=rotate.y)
  options(offset=offset)

  options(add.cex=add.cex)
  options(add.lwd=add.lwd)
  options(add.lty=add.lty)
  options(add.stroke=add.stroke)
  options(add.fill=pt.fill)

  options(quiet=quiet)
  options(brief=brief)
  options(n.cat=n.cat)
  options(suggest=suggest)
  options(lab.size=lab.size)
  options(width=width)

  options(results=results)
  options(explain=explain)
  options(interpret=interpret)
  options(document=document)
  options(code=code)


  # ----------------------------------------------
  # functions

  make.color <- function(color1, color2) {
    options(device.fill = "white")
    options(bar.fill = .maketrans(color1, .to256("trans.bar.fill")))
    options(pt.fill = .maketrans(color1, .to256("trans.pt.fill")))
    options(bubble.fill = .maketrans(color1, .to256("trans.pt.fill")))
    options(bar.stroke = color2)
    options(pt.stroke = color2)
    options(se.fill = .maketrans(color1, 25)) 
    options(ellipse.fill = .maketrans(color1, 15))
    options(heat = color2)
    options(segment.stroke = color1)
    options(bubble.text.stroke = "black")
  }

  make.default <- function()  {
    options(bg.fill = "grey92")
    if (theme == "white") options(bg.fill = "white")
    options(device.fill = "white")
    options(grid.x.stroke = "white")
    options(grid.y.stroke = "white")
    options(lab.stroke = "black")
    options(main.stroke = "black")
  }

  prepare.black <- function() {
    options(device.fill = rgb(.015,.015,.015))
    options(bg.fill = rgb(.015,.015,.015))
    options(values.stroke = "gray80")
    options(grid.x.stroke = "gray25")
    options(grid.y.stroke = "gray25")
    options(bg.stroke = "gray80")
    options(lab.stroke = "gray80")
    options(main.stroke = "gray80")
    options(axis.x.stroke = "gray80")
    options(axis.y.stroke = "gray80")
    options(add.stroke = "gray80")
    options(heat = "gray30")
  }

  make.black <- function(color1) {
    if (!(theme %in% c("gray", "orange")  &&  sub.theme == "black")) {
      options(bar.fill = .maketrans(color1, .to256n(0.40)))
      options(bar.stroke = color1)
      options(pt.fill= .maketrans(color1, .to256n(0)))
      options(fit.stroke = color1)
      options(ellipse.fill = .maketrans(color1, 65))
      options(bubble.fill = .maketrans(color1, .to256n(0.40)))
      options(segment.stroke = color1)
      options(se.fill = .maketrans(color1, 35)) 
    }
    else if (theme ==  "gray"  &&  sub.theme == "black") { 
      options(bar.fill = .maketrans("gray55", .to256("trans.bar.fill")))
      options(bar.stroke = "gray20")
      options(pt.fill = .maketrans("gray75", .to256("trans.pt.fill")))
      options(pt.stroke = "gray95")
      options(ellipse.fill = .maketrans("gray55", 65))
      options(fit.stroke = "gray75")
      options(se.fill = .maketrans("gray55", 65)) 
      options(heat = "gray30")
      options(segment.stroke = "gray65")
      clr1 <- "gray55"
    }
    else if (theme ==  "orange"  &&  sub.theme == "black") {
      if (miss.tr.bar.fill) options(trans.bar.fill = .2)
      options(bar.fill = rgb(249,99,2, alpha=.to256("trans.bar.fill"),
            maxColorValue=256))
      options(pt.fill = rgb(249,99,2, alpha=.to256("trans.pt.fill"),
            maxColorValue=256))
      options(bubble.fill = rgb(249,99,2, alpha=.to256("trans.pt.fill"),
            maxColorValue=256))
      options(bar.stroke = "orange4")
      options(pt.stroke = rgb(209,87,3, maxColorValue=256))
      options(ellipse.fill = rgb(249,99,2, alpha=45, maxColorValue=256))
      options(fit.stroke = rgb(209,87,3, maxColorValue=256))
      options(heat="darkorange3")
      options(segment.stroke = rgb(249,99,2, maxColorValue=256))
      clr1 <- rgb(249,99,2, maxColorValue=256)
    }
  }

  make.no.y.axis  <- function() {
    options(axis.y.stroke = "transparent")
    gxs <- ifelse (getOption("device.fill") == "#040404", "white", "#040404")
    options(grid.y.stroke = gxs)
    options(grid.x.stroke = "transparent")
    options(grid.lty = "dotted")
    options(grid.lwd = 1)
  }
  # ----------------------------------------------


  # set colors
  if (theme == "dodgerblue") {clr1 <- "dodgerblue3"; clr2 <- "steelblue4"}
  if (theme == "sienna") {clr1 <- "sienna3"; clr2 <- "sienna4"}
  if (theme == "green") {clr1 <- "darkgreen"; clr2 <- "darkseagreen4"}
  if (theme == "darkgreen") {
    clr1 <- "darkgreen"; clr2 <- rgb(0,80,0,maxColorValue=256)
  }
  if (theme == "rose") {clr1 <- "rosybrown3"; clr2 <- "rosybrown4"}
  if (theme == "gold") {clr1 <- "goldenrod2"; clr2 <- "goldenrod4"}
  if (theme == "red") {clr1 <- "firebrick3"; clr2 <- "firebrick4"}
  if (theme == "darkred") {clr1 <- "darkred"; clr2 <-rgb(80,0,0,maxColorValue=256)}
  if (theme == "purple") {clr1 <- "purple1"; clr2 <- "purple4"}
  if (theme == "blue") {clr1 <- "royalblue1"; clr2 <- "royalblue4"}
  if (theme == "brown") {clr1 <- "rosybrown4"; clr2 <- "rosybrown3"}
  if (theme == "orange") {clr1 <- "orange"; clr2 <- "orange3"}
  if (theme == "white") {clr1 <- "white"; clr2 <- "black"}
  if (theme == "gray") {clr1 <- "gray25"}
  if (theme == "lightbronze") {clr1 <- rgb(247,242,230, maxColorValue=255)}

  # only run if theme is specified, resets all parameters
  if (!miss.theme) {
    theme <- getOption("theme")

  if (theme == "white") {
    options(device.fill = "white")
    options(grid.x.stroke = "gray90")
    options(grid.y.stroke = "gray90")
    options(bg.fill = "white")
    options(bubble.fill = "white")
    options(bubble.text.stroke = "black")
    options(ellipse.fill = .maketrans("gray55", 55))
    options(ellipse.stroke = "black")
    options(fit.stroke = "gray15")
    options(se.fill = .maketrans("gray10", 25)) 
  }

    else if (theme == "gray") {
      options(bar.fill = .maketrans("gray25", .to256("trans.bar.fill")))
      options(pt.fill = .maketrans("gray20", .to256("trans.pt.fill")))
      options(bubble.fill = .maketrans("gray20", .to256("trans.pt.fill")))
      options(bar.stroke = "gray60")
      options(pt.stroke = "black")
      options(ellipse.fill = .maketrans("gray35", 15))
      options(fit.stroke = "black")
      options(se.fill = .maketrans("gray0", 25)) 
      options(heat = "gray5")
      options(segment.stroke = "gray20")
    }

    else if (theme == "lightbronze") {
      options(device.fill = rgb(247,242,230, maxColorValue=255))
      options(bg.fill = "transparent")
      options(bg.stroke = rgb(222,217,205, maxColorValue=255))
      options(bar.fill = .maketrans("gray50", .to256("trans.bar.fill")))
      options(pt.fill = "gray20")
      options(bubble.fill = .maketrans("gray50", .to256("trans.bar.fill")))
      options(bar.stroke = "gray30")
      options(pt.stroke = "gray20")
      options(ellipse.fill = .maketrans("gray55", 55))
      options(ellipse.stroke = "gray55")
      options(fit.stroke = "gray15")
      options(se.fill = .maketrans("gray10", 25)) 
      options(heat = "gray30")
      options(main.stroke = "gray15")
      options(lab.stroke = "gray15")
      options(values.stroke = "gray15")
      options(axis.x.stroke = "gray15")
      options(axis.y.stroke = "gray15")
      options(segment.stroke = "gray15")
      options(bubble.text.stroke = rgb(247,242,230, maxColorValue=255))
      options(grid.x.stroke = rgb(222,217,205, maxColorValue=255))
      options(grid.y.stroke = rgb(222,217,205, maxColorValue=255))
      options(trans = 0)
     
    }

    else {
      make.color(clr1, clr2)
      if (!miss.sub.theme) if (sub.theme == "default") make.default()
    }

  }  # not miss theme

  if (!miss.sub.theme) {
    if (sub.theme == "no.y.axis") make.no.y.axis()
    if (sub.theme == "black") {
      prepare.black()
      make.black(clr1)
    }
  }


  if (show) {
    cat("\n")

    cat("THEME\n")
    cat("theme ........ Theme colors ......", getOption("theme"), "\n")
    cat("sub.theme .... Sub-theme style ...", getOption("sub.theme"), "\n")

    cat("\n")
    cat("BACKGROUND\n")
    cat("device.fill .. Window background color ..",
        col2rgb(getOption("device.fill")), "\n")
    cat("bg.fill ...... Plot background color ....",
        col2rgb(getOption("bg.fill")), "\n")
    cat("bg.stroke .... Plot border color ....... ",
        col2rgb(getOption("bg.stroke")), "\n")
    cat("bg.lwd ....... Plot border line width .. ",
        .fmt(getOption("bg.lwd"), 1), "\n")
    cat("bg.lty ....... Plot border line type ... ",
        getOption("bg.lty"), "\n")

    cat("\n")
    cat("DATA OBJECTS\n")
    cat("bar.fill ..... Bar fill color .......",
        col2rgb(getOption("bar.fill"), TRUE), "\n")
    cat("pt.fill ...... Point fill color .....",
        col2rgb(getOption("pt.fill"), TRUE), "\n")
    cat("trans.bar.fill Bar transparency .....",
        .fmt(getOption("trans.bar.fill"), 2), "\n")
    cat("trans.pt.fill  Point transparency .. ",
        .fmt(getOption("trans.pt.fill"), 2), "\n")
    cat("bar.stroke ... Bar stroke color .....",
        col2rgb(getOption("bar.stroke"), TRUE), "\n")
    cat("pt.stroke .... Point stroke color .. ",
        col2rgb(getOption("pt.stroke"), TRUE), "\n")
    cat("se.fill ...... Stnd error fill color ",
        col2rgb(getOption("se.fill"), TRUE), "\n")
    cat("ellipse.fill   Ellipse fill color .. ",
        col2rgb(getOption("ellipse.fill"), TRUE), "\n")
    cat("bubble.fill .. Bubble fill color ....",
        col2rgb(getOption("bubble.fill"), TRUE), "\n")
    cat("bubble.text.stroke Bubble text color ",
        col2rgb(getOption("bubble.text.stroke"), TRUE), "\n")
    cat("segment.stroke ... Color of line segments ....",
        getOption("segment.stroke"), "\n")
    cat("heat ......... Heat map color .......",
        col2rgb(getOption("heat")), "\n")

    cat("\n")
    cat("AXES\n")
    cat("axis.x.stroke ... Color of x-axis ..................",
        getOption("axis.x.stroke"), "\n")
    cat("axis.y.stroke ... Color of y-axis ..................",
        getOption("axis.y.stroke"), "\n")
    cat("cex.axis ..... x and y axis text size ................",
        .fmt(getOption("cex.axis"), 2), "\n")
    cat("values.stroke  x and y axis values text color ......",
        getOption("values.stroke"), "\n")
    cat("rotate.x ....  Rotation of x axis text ...............",
        .fmt(getOption("rotate.x"), 2), "\n")
    cat("rotate.y ....  Rotation of y axis text ...............",
        .fmt(getOption("rotate.y"), 2), "\n")
    cat("offset ....... Offset of values text from axis .......",
        .fmt(getOption("offset"), 2), "\n")
    cat("lab.stroke ... Color of axis labels ........",
        getOption("lab.stroke"), "\n")
    cat("lab.size ..... Size of axis labels .........",
        getOption("lab.size"), "\n")
    cat("main.stroke .. Color of plot label .........",
        getOption("main.stroke"), "\n")

    cat("\n")
    cat("GRID LINES\n")
    cat("grid.x.stroke  Grid color of vertical grid .....",
        col2rgb(getOption("grid.x.stroke")), "\n")
    cat("grid.y.stroke  Grid color of horizontal grid ...",
        col2rgb(getOption("grid.y.stroke")), "\n")
    cat("grid.lwd ..... Grid line width ...",
        .fmt(getOption("grid.lwd"), 1), "\n")
    cat("grid.lty ..... Grid line type ....",
        getOption("grid.lty"), "\n")

    cat("\n")
    cat("ANNOTATION\n")
    cat("add.fill  .. Fill color of annotated figures",
        getOption("add.fill"), "\n")
    cat("add.stroke   Color of annotated lines .........",
        getOption("add.stroke"), "\n")
    cat("add.cex  ... Size of annotated text .............",
        .fmt(getOption("add.cex"), 2), "\n")
    cat("add.lwd  ... Line width of annotated lines .......",
        .fmt(getOption("add.lwd"), 1), "\n")
    cat("add.lty  ... Line type of annotated lines ......",
        getOption("add.lty"), "\n")
    cat("\n")
    cat("NON-GRAPHICAL\n")
    cat("quiet ..... Suppress console output for many functions ..",
        getOption("quiet"), "\n")
    cat("brief ..... Reduce console output for many functions ... ",
        getOption("brief"), "\n")
    cat("suggest ... Suggestions for enhanced input ..............",
        getOption("suggest"), "\n")
    cat("width ..... Column width ................................",
        getOption("width"), "\n")
    cat("n.cat ..... Largest number of unique, equally spaced integer\n",
        "           values of the variable for which the variable will\n",
        "           be analyzed as categorical ... ",
        getOption("n.cat"), "\n")
    cat("\n")
  }

}

style <-
function(
  theme=c("lightbronze", "dodgerblue", "darkred", "gray", "gold",
      "darkgreen", "blue", "red", "rose", "green", "purple", "sienna",
      "brown", "orange", "white"),
  sub.theme=c("default", "black", "no.y.axis"),
  set=NULL, get=TRUE,

  window.fill=getOption("window.fill"),
  panel.fill=getOption("panel.fill"),
  panel.color=getOption("panel.color"),
  panel.lwd=getOption("panel.lwd"),
  panel.lty=getOption("panel.lty"),

  fill=NULL,
  bar.fill=getOption("bar.fill"),
  pt.fill=getOption("pt.fill"),
  trans=NULL,
  trans.bar.fill=getOption("trans.bar.fill"),
  trans.pt.fill=getOption("trans.pt.fill"),
  color=NULL,
  bar.color=getOption("bar.color"),
  pt.color=getOption("pt.color"),
  se.fill=getOption("se.fill"),
  ellipse.fill=getOption("ellipse.fill"),
  ellipse.color=getOption("ellipse.color"),
  ellipse.lwd=getOption("ellipse.lwd"),
  fit.color=getOption("fit.color"),
  fit.lwd=getOption("fit.lwd"),
  bubble.text.color=getOption("bubble.text.color"),
  heat=getOption("heat"), 
  segment.color=getOption("segment.color"),
  ID.color=getOption("ID.color"),
  area.fill=getOption("area.fill"),
  out.fill=getOption("out.fill"),
  out.color=getOption("out.color"),
  out2.fill=getOption("out2.fill"),
  out2.color=getOption("out2.color"),

  violin.fill=getOption("violin.fill"),
  violin.color=getOption("violin.color"),
  box.fill=getOption("box.fill"),
  box.color=getOption("box.color"),

  axis.color=getOption("axis.color"),
  axis.x.color=getOption("axis.x.color"),
  axis.y.color=getOption("axis.y.color"),
  axis.lwd=getOption("axis.lwd"),
  axis.x.lwd=getOption("axis.x.lwd"),
  axis.y.lwd=getOption("axis.y.lwd"),
  axis.lty=getOption("axis.lty"),
  axis.x.lty=getOption("axis.x.lty"),
  axis.y.lty=getOption("axis.y.lty"),
  axis.cex=getOption("axis.cex"),
  axis.x.cex=getOption("axis.x.cex"),
  axis.y.cex=getOption("axis.y.cex"),
  axis.text.color=getOption("axis.text.color"),
  axis.x.text.color=getOption("axis.x.text.color"),
  axis.y.text.color=getOption("axis.y.text.color"),
  rotate.x=getOption("rotate.x"),
  rotate.y=getOption("rotate.y"),
  offset=getOption("offset"),

  lab.color=getOption("lab.color"),
  lab.x.color=getOption("lab.x.color"),
  lab.y.color=getOption("lab.y.color"),
  lab.cex=getOption("lab.cex"),
  lab.x.cex=getOption("lab.x.cex"),
  lab.y.cex=getOption("lab.y.cex"),
  main.color=getOption("main.color"),
  main.cex=getOption("main.cex"),

  grid.color=getOption("grid.color"),
  grid.x.color=getOption("grid.x.color"),
  grid.y.color=getOption("grid.y.color"),
  grid.lwd=getOption("grid.lwd"),
  grid.x.lwd=getOption("grid.x.lwd"),
  grid.y.lwd=getOption("grid.y.lwd"),
  grid.lty=getOption("grid.lty"),
  grid.x.lty=getOption("grid.x.lty"),
  grid.y.lty=getOption("grid.y.lty"),

  strip.fill=getOption("strip.fill"),
  strip.color=getOption("strip.color"),
  strip.text.color=getOption("strip.text.color"),

  add.fill=getOption("add.fill"),
  add.trans=getOption("add.trans"),
  add.color=getOption("add.color"),
  add.cex=getOption("add.cex"),
  add.lwd=getOption("add.lwd"),
  add.lty=getOption("add.lty"),

  n.cat=getOption("n.cat"), suggest=getOption("suggest"),
  quiet=getOption("quiet"), brief=getOption("brief"),

  results=getOption("results"), explain=getOption("explain"),
  interpret=getOption("interpret"), document=getOption("document"), 
  code=getOption("code"),

  width=120, show=FALSE, ...) {

  if (nargs() == 0) {
     theme <- "lightbronze"
     cat("theme set to \"lightbronze\"\n\n")
  }

  miss.theme <- ifelse (missing(theme), TRUE, FALSE)

  miss.sub.theme <- ifelse (missing(sub.theme), TRUE, FALSE)
  sub.theme <- match.arg(sub.theme)
  miss.set <- ifelse (missing(set), TRUE, FALSE) 
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


  # ------------------------------------------------
  # reset values according to previously written set
  if (!is.null(set)) {

    theme <- set$theme
    sub.theme <- set$sub.theme

    window.fill <- set$window.fill
    panel.fill <- set$panel$fill
    panel.color <- set$panel$color
    panel.lwd <- set$panel$lwd
    panel.lty <- set$panel$lty

    bar.fill <- set$bar$fill
    trans.bar.fill <- set$bar$trans.fill
    bar.color <- set$bar$color
    pt.fill <- set$pt$fill
    trans.pt.fill <- set$pt$trans.fill
    pt.color <- set$pt$color
    out.fill <- set$pt$out.fill
    out.color <- set$pt$out.color
    out2.fill <- set$pt$out2.fill
    out2.color <- set$pt$out2.color


    violin.fill <- set$VBS$violin.fill
    violin.color <- set$VBS$violin.color
    box.fill <- set$VBS$box.fill
    box.color <- set$VBS$box.color

    ellipse.fill <- set$ellipse$fill
    ellipse.color <- set$ellipse$color
    ellipse.lwd <- set$ellipse$lwd

    fit.color <- set$fit.color
    fit.lwd <- set$fit.lwd
    se.fill <- set$se.fill
    bubble.text.color <- set$bubble.text.color
    heat <- set$heat 
    segment.color <- set$segment.color
    ID.color <- set$ID.color
    area.fill <- set$area.fill

    axis.color <- set$axis$color
    axis.x.color <- set$axis.x$color
    axis.y.color <- set$axis.y$color
    axis.lwd <- set$axis$lwd
    axis.x.lwd <- set$axis.x$lwd
    axis.y.lwd <- set$axis.y$lwd
    axis.lty <- set$axis$lty
    axis.x.lty <- set$axis.x$lty
    axis.y.lty <- set$axis.y$lty
    axis.cex <- set$axis$cex
    axis.x.cex <- set$axis.x$cex
    axis.y.cex <- set$axis.y$cex
    axis.text.color <- set$axis$text.color
    axis.x.text.color <- set$axis.x$text.color
    axis.y.text.color <- set$axis.y$text.color
    rotate.x <- set$rotate$x
    rotate.y <- set$rotate$y
    offset <- set$rotate$offset

    lab.color <- set$lab$color
    lab.x.color <- set$lab.x$color
    lab.y.color <- set$lab.y$color
    lab.cex <- set$lab$cex
    lab.x.cex <- set$lab.x$cex
    lab.y.cex <- set$lab.y$cex
    main.color <- set$main$color
    main.cex <- set$main$cex

    grid.color <- set$grid$color
    grid.x.color <- set$grid.x$color
    grid.y.color <- set$grid.y$color
    grid.lwd <- set$grid$lwd
    grid.x.lwd <- set$grid.x$lwd
    grid.y.lwd <- set$grid.y$lwd
    grid.lty <- set$grid$lty
    grid.x.lty <- set$grid.x$lty
    grid.y.lty <- set$grid.y$lty

    strip.fill <- set$strip$fill
    strip.color <- set$strip$color
    strip.text.color <- set$strip$text.color

    add.fill <- set$add$fill
    add.trans <- set$add$trans
    add.color <- set$add$color
    add.cex <- set$add$cex
    add.lwd <- set$add$lwd
    add.lty <- set$add$lty

    n.cat <- set$n.cat
    suggest <- set$suggest
    quiet <- set$quiet
    brief <- set$brief

    results <- set$output$results
    explain <- set$output$explain
    interpret <- set$output$interpret
    document <- set$output$document 
    code <- set$output$code
  }  # end not null set

        
  # reset all parameters to start-up condition for new theme
  if (!miss.theme) {
    suppressPackageStartupMessages(.onAttach())
    miss.theme <- FALSE
    theme <- match.arg(theme) 
    options(theme=theme)
    options(trans.bar.fill=0.10)
    options(trans.pt.fill=0.40) 
  }
  else if (miss.set)
    theme <- getOption("theme")

  if (!miss.sub.theme)
    options(sub.theme=sub.theme)
  else
    sub.theme <- getOption("sub.theme")


  # inheritance
  if (!missing(trans)) {
    trans.pt.fill <- trans
    trans.bar.fill <- trans
  }
  if (!missing(fill)) {
    pt.fill <- fill
    bar.fill <- fill
  }
  if (!missing(color)) {
    pt.color <- color
    bar.color <- color
  }
  if (!missing(axis.color)) {
    axis.x.color <- axis.color
    axis.y.color <- axis.color
  }
  if (!missing(axis.cex)) {
    axis.x.cex <- axis.cex
    axis.y.cex <- axis.cex
  }
  if (!missing(lab.color)) {
    lab.x.color <- lab.color
    lab.y.color <- lab.color
  }
  if (!missing(lab.cex)) {
    lab.x.cex <- lab.cex
    lab.y.cex <- lab.cex
  }
  if (!missing(grid.color)) {
    grid.x.color <- grid.color
    grid.y.color <- grid.color
  }
  if (!missing(grid.lwd)) {
    grid.x.lwd <- grid.lwd
    grid.y.lwd <- grid.lwd;
  }
  if (!missing(grid.lty)) {
    grid.x.lty <- grid.lty
    grid.y.lty <- grid.lty
  }

  # "off" is "transparent"
  bar.fill[which(bar.fill == "off")] <- "transparent"
  pt.fill[which(pt.fill == "off")] <- "transparent"
  bar.color[which(bar.color == "off")] <- "transparent"
  pt.color[which(pt.color == "off")] <- "transparent"
  violin.fill[which(violin.fill == "off")] <- "transparent"
  violin.color[which(violin.color == "off")] <- "transparent"
  box.fill[which(box.fill == "off")] <- "transparent"
  box.color[which(box.color == "off")] <- "transparent"
  se.fill[which(se.fill == "off")] <- "transparent"
  ellipse.fill[which(ellipse.fill == "off")] <- "transparent"
  ellipse.color[which(ellipse.color == "off")] <- "transparent"
  if (panel.fill == "off") panel.fill <- "transparent"
  if (!is.null(grid.x.color))
    if (grid.x.color == "off") grid.x.color <- "transparent"
  if (!is.null(grid.y.color))
    if (grid.y.color == "off") grid.y.color <- "transparent"
  if (grid.lwd == "off") grid.lwd <- 0
  if (grid.lty == "off") grid.lty <- "blank"
  if (window.fill == "off") window.fill <- "white"
  if (panel.color == "off") panel.color <- "transparent"
  if (panel.lwd == "off") panel.lwd <- "transparent"
  if (panel.lty == "off") panel.lty <- "transparent"
  if (lab.color == "off") lab.color <- "transparent"
  if (main.color == "off") main.color <- "transparent"
  if (!is.null(axis.x.color))
    if (axis.x.color == "off") axis.x.color <- "transparent"
  if (!is.null(axis.y.color))
    if (axis.y.color == "off") axis.y.color <- "transparent"
  if (!is.null(lab.x.color))
    if (lab.x.color == "off") lab.x.color <- "transparent"
  if (!is.null(lab.y.color))
    if (lab.y.color == "off") lab.y.color <- "transparent"
  if (segment.color == "off") segment.color <- "transparent"
  if (ID.color == "off") ID.color <- "transparent"
  if (area.fill == "off") area.fill <- "transparent"
  if (out.fill == "off") out.fill <- "transparent"
  if (out.color == "off") out.color <- "transparent"
  if (out2.fill == "off") out2.fill <- "transparent"
  if (out2.color == "off") out2.color <- "transparent"
  if (bubble.text.color == "off") bubble.text.color <- "transparent"
  add.color[which(add.color == "off")] <- "transparent"
  add.fill[which(add.fill == "off")] <- "transparent"

  bar.fill <- .color.range(bar.fill, 24)
  bar.color <- .color.range(bar.color, 24)
  pt.fill <- .color.range(pt.fill, 24)
  pt.color <- .color.range(pt.color, 24)
  add.fill <- .color.range(add.fill, 24)
  add.color <- .color.range(add.color, 24)

# default transparency levels
  if (!is.null(trans.bar.fill)) {
    options(trans.bar.fill=trans.bar.fill)
    options(bar.fill = .maketrans(getOption("bar.fill"), 
           .to256("trans.bar.fill")))
  }
  if (!is.null(trans.pt.fill)) {
    options(trans.pt.fill=trans.pt.fill)
    options(pt.fill = .maketrans(getOption("pt.fill"), .to256("trans.pt.fill")))
  }

  if (!is.null(bar.fill)) {
    if (bar.fill[1] == "transparent")
      options(bar.fill = bar.fill) 
    else
      options(bar.fill = .maketrans(bar.fill, .to256("trans.bar.fill")))
  }
  if (!is.null(pt.fill)) {
    if (pt.fill[1] == "transparent")
      options(pt.fill = pt.fill) 
    else
      options(pt.fill = .maketrans(pt.fill, .to256("trans.pt.fill")))
  }


  # ---------------
  # set the options

  options(theme = theme)
  options(sub.theme = sub.theme)

  options(bar.fill = bar.fill) 
  options(bar.color = bar.color) 
  options(pt.color = pt.color) 

  options(window.fill=window.fill)
  options(panel.fill=panel.fill)
  options(panel.color=panel.color)
  options(panel.lwd=panel.lwd)
  options(panel.lty=panel.lty)

  options(violin.fill=violin.fill)
  options(violin.color=violin.color)
  options(box.fill=box.fill)
  options(box.color=box.color)

  options(ellipse.fill=ellipse.fill)
  options(ellipse.color=ellipse.color)
  options(ellipse.lwd=ellipse.lwd)
  options(fit.color=fit.color)
  options(fit.lwd=fit.lwd)
  options(se.fill=se.fill)
  options(segment.color=segment.color)
  options(ID.color=ID.color)
  options(area.fill=area.fill)
  options(out.fill=out.fill)
  options(out.color=out.color)
  options(out2.fill=out2.fill)
  options(out2.color=out2.color)
  options(bubble.text.color=bubble.text.color)

  options(grid.color=grid.color)
  options(grid.x.color=grid.x.color)
  options(grid.y.color=grid.y.color)
  options(grid.lwd=grid.lwd)
  options(grid.x.lwd=grid.x.lwd)
  options(grid.y.lwd=grid.y.lwd)
  options(grid.lty=grid.lty)
  options(grid.x.lty=grid.x.lty)
  options(grid.y.lty=grid.y.lty)

  options(lab.color=lab.color)
  options(lab.x.color=lab.x.color)
  options(lab.y.color=lab.y.color)
  options(lab.cex=lab.cex)
  options(lab.x.cex=lab.x.cex)
  options(lab.y.cex=lab.y.cex)
  options(main.color=main.color)
  options(main.cex=main.cex)
  options(axis.color=axis.color)
  options(axis.x.color=axis.x.color)
  options(axis.y.color=axis.y.color)
  options(axis.lwd=axis.lwd)
  options(axis.x.lwd=axis.x.lwd)
  options(axis.y.lwd=axis.y.lwd)
  options(axis.lty=axis.lty)
  options(axis.x.lty=axis.x.lty)
  options(axis.y.lty=axis.y.lty)

  options(axis.cex=axis.cex)
  options(axis.x.cex=axis.x.cex)
  options(axis.y.cex=axis.y.cex)
  options(axis.text.color=axis.text.color)
  options(axis.x.text.color=axis.x.text.color)
  options(axis.y.text.color=axis.y.text.color)
  options(rotate.x=rotate.x)
  options(rotate.y=rotate.y)
  options(offset=offset)

  options(add.fill=add.fill)
  options(add.trans=add.trans)
  options(add.cex=add.cex)
  options(add.lwd=add.lwd)
  options(add.lty=add.lty)
  options(add.color=add.color)

  options(strip.fill=strip.fill)
  options(strip.color=strip.color)
  options(strip.text.color=strip.text.color)

  options(quiet=quiet)
  options(brief=brief)
  options(n.cat=n.cat)
  options(suggest=suggest)
  options(width=width)

  options(results=results)
  options(explain=explain)
  options(interpret=interpret)
  options(document=document)
  options(code=code)


  # ----------------------------------------------
  # functions

  make.color <- function(color1, color2) {
    options(window.fill = "white")
    options(panel.fill = "grey95")
    options(bar.fill = .maketrans(color1, .to256("trans.bar.fill")))
    options(pt.fill = .maketrans(color1, .to256("trans.pt.fill")))
    options(bar.color = color2)
    options(pt.color = color2)
    options(violin.fill = .maketrans(color1, 125))  # smaller, more trans 
    options(violin.color = "gray15") 
    options(box.fill = .maketrans(color1, 35)) 
    options(box.color = "gray15") 
    options(se.fill = .maketrans(color1, 25)) 
    options(ellipse.fill = .maketrans(color1, 15))
    if (ellipse.color[1] != "transparent")
      options(ellipse.color = .maketrans(color1, 200))
    options(heat = color2)
    options(segment.color = color1)
    options(bubble.text.color = "black")
    options(strip.fill = .maketrans(color1, 55)) 
    options(strip.color = color2) 
    options(strip.text.color = color2) 
  }

  make.default <- function()  {
    options(panel.fill = "grey95")
    if (theme == "white") options(panel.fill = "white")
    options(window.fill = "white")
    options(grid.x.color = "white")
    options(grid.y.color = "white")
    options(lab.color = "black")
    options(main.color = "black")
  }

  prepare.black <- function() {
    options(window.fill = rgb(.015,.015,.015))
    options(panel.fill = rgb(.015,.015,.015))
    options(grid.color = "gray25")
    options(panel.color = "gray80")
    options(segment.color = "gray65")
    options(lab.color = "gray55")
    options(main.color = "gray55")
    options(axis.x.color = "gray55")
    options(axis.y.color = "gray55")
    options(axis.text.color = "gray55")
    options(add.color = "gray55")
    options(heat = "gray30")
  }

  make.black <- function(color1) {
    if (sum(col2rgb(panel.fill)) < 370) {
      options(strip.color = .maketrans(color1, .to256n(0.40))) 
      options(strip.text.color = "gray65") 
    }
 
    else if (theme ==  "gray"  &&  sub.theme == "black") { 
      options(bar.fill = .maketrans("gray55", .to256("trans.bar.fill")))
      options(bar.color = "gray20")
      options(pt.fill = .maketrans("gray75", .to256("trans.pt.fill")))
      options(pt.color = .maketrans("gray75", .to256("trans.pt.fill")))
      options(violin.fill = .maketrans("gray85", 160))
      options(violin.color = "gray15") 
      options(box.fill = .maketrans("gray15", 35)) 
      options(box.color = "gray15") 
      options(ellipse.fill = .maketrans("gray55", 65))
      options(fit.color = "gray75")
      options(area.fill = .maketrans("gray55", .to256("trans.bar.fill"))) 
      options(se.fill = .maketrans("gray55", 65)) 
      options(heat = "gray30")
      options(strip.color = .maketrans(color1, .to256n(0.40))) 
      options(strip.text.color = "gray65") 
      clr1 <- "gray55"
    }
    else if (theme ==  "orange"  &&  sub.theme == "black") {
      if (miss.tr.bar.fill) options(trans.bar.fill = .2)
      options(bar.fill = rgb(249,99,2, alpha=.to256("trans.bar.fill"),
            maxColorValue=256))
      options(pt.fill = rgb(139,69,0, alpha=.to256("trans.pt.fill"),
            maxColorValue=256))
      options(bar.color = "orange4")
      options(pt.color = rgb(139,69,0, maxColorValue=256))
      options(ellipse.fill = rgb(249,99,2, alpha=45, maxColorValue=256))
      options(fit.color = rgb(209,87,3, maxColorValue=256))
      options(area.fill = rgb(249,99,2, alpha=.to256("trans.bar.fill"),
            maxColorValue=256)) 
      options(heat="darkorange3")
      options(segment.color = rgb(249,99,2, maxColorValue=256))
      clr1 <- rgb(249,99,2, maxColorValue=256)
    }
  }

  make.no.y.axis  <- function() {
    options(window.fill = getOption("panel.fill"))
    options(axis.y.color = "transparent")
    gxs <- ifelse (getOption("window.fill") == "#040404", "white", "#040404")
    options(grid.y.color = gxs)
    options(grid.x.color = "transparent")
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
  if (theme == "gold") {clr1 <- "goldenrod3"; clr2 <- "goldenrod4"}
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
      options(window.fill = "white")
      options(panel.fill = "white")
      options(bar.fill = "white")
      options(bar.color = "black")
      options(pt.fill = "white")
      options(pt.color = "black")
      options(bubble.text.color = "black")
      options(ellipse.fill = .maketrans("gray55", 55))
      options(ellipse.color = "black")
      options(fit.color = "gray15")
      options(violin.fill = "white") 
      options(violin.color = "black") 
      options(box.fill = "white") 
      options(box.color = "black") 
      options(area.fill = "black") 
      options(se.fill = .maketrans("gray10", 25)) 
      options(grid.color = "gray90")
      options(out.fill="black")
      options(out.color="black")
      options(out2.fill="gray25")
      options(out2.color="gray25")
    }

    else if (theme == "gray") {
      options(window.fill = "white")
      options(panel.fill = "grey95")
      options(bar.fill = .maketrans("gray25", .to256("trans.bar.fill")))
      options(pt.fill = .maketrans("gray20", .to256("trans.pt.fill")))
      options(bar.color = "gray60")
      options(pt.color = "black")
      options(violin.fill = .maketrans("gray55", 150))
      options(violin.color = "gray15") 
      options(box.fill = .maketrans("gray15", 35)) 
      options(box.color = "gray15") 
      options(ellipse.fill = .maketrans("gray35", 15))
      options(fit.color = "black")
      options(area.fill = .maketrans("gray25", .to256("trans.bar.fill"))) 
      options(se.fill = .maketrans("gray0", 25)) 
      options(heat = "gray5")
      options(segment.color = "gray20")
      options(grid.color = "white")
      options(out.fill="black")
      options(out.color="black")
      options(out2.fill="black")
      options(out2.color="black")
    }

    else if (theme == "lightbronze") {
      options(window.fill = rgb(247,242,230, maxColorValue=255))
      options(panel.fill = rgb(247,242,230, maxColorValue=255))
      #options(panel.fill = "transparent")
      options(panel.color = rgb(222,217,205, maxColorValue=255))
      options(bar.fill = .maketrans("gray50", .to256("trans.bar.fill")))  # 230
      options(bar.color = "gray30")
      options(pt.fill = "gray20")
      options(pt.color = "gray20")
      options(ellipse.fill = .maketrans("gray50", 50))
      options(ellipse.color = "gray20")
      options(se.fill = .maketrans("gray50", 30)) 
      options(violin.fill = .maketrans("gray50", 150)) 
      options(violin.color = "gray15") 
      options(box.fill = .maketrans("gray15", 35)) 
      options(box.color = "gray15") 
      options(strip.fill = .maketrans("gray50", 55)) 
      options(strip.color = "gray55") 
      options(strip.text.color = "gray15") 
      options(ellipse.color = "gray55")
      options(fit.color = "gray15")
      options(area.fill = .maketrans("gray50", .to256("trans.bar.fill")))
      options(heat = "gray30")
      options(main.color = "gray15")
      options(lab.color = "gray15")
      options(axis.color = "gray15")
      options(axis.text.color = "gray15")
      options(segment.color = "gray15")
      options(bubble.text.color = rgb(247,242,230, maxColorValue=255))
      options(grid.color = rgb(222,217,205, maxColorValue=255))
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
    .style.show()
  }

  # ---------------------------------------
  # create list of current parameter values

  if (get) {

    # create sub-lists

    panel <- list(
      fill = getOption("panel.fill"),
      color = getOption("panel.color"),
      lwd = getOption("panel.lwd"),
      lty = getOption("panel.lty")
    )

    bar <- list(
      fill = getOption("bar.fill"),
      trans.fill = getOption("trans.bar.fill"),
      color =getOption("bar.color")
    )

    pt <- list(
      fill = getOption("pt.fill"),
      trans.fill = getOption("trans.pt.fill"),
      color = getOption("pt.color"),
      out.fill=getOption("out.fill"),
      out.color=getOption("out.color"),
      out2.fill=getOption("out2.fill"),
      out2.color=getOption("out2.color")
    )

    VBS <- list(
      violin.fill = getOption("violin.fill"),
      violin.color = getOption("violin.color"),
      box.fill = getOption("box.fill"),
      box.color = getOption("box.color")
    )

    ellipse <- list(
      fill = getOption("ellipse.fill"),
      color = getOption("ellipse.color")
    )

    axis <- list(
      color = getOption("axis.color"),
      lwd = getOption("axis.lwd"),
      lty = getOption("axis.lty"),
      cex = getOption("axis.cex"),
      text.color = getOption("axis.text.color")
    )
    axis.x <- list(
      color = getOption("axis.x.color"),
      lwd = getOption("axis.x.lwd"),
      lty = getOption("axis.x.lty"),
      cex = getOption("axis.x.cex"),
      text.color = getOption("axis.x.text.color")
    )
    axis.y <- list(
      color = getOption("axis.y.color"),
      lwd = getOption("axis.y.lwd"),
      lty = getOption("axis.y.lty"),
      cex = getOption("axis.y.cex"),
      text.color = getOption("axis.y.text.color")
    )

    rotate <- list(
      x = getOption("rotate.x"),
      y = getOption("rotate.y"),
      offset = getOption("offset")
    )

    lab <- list(
      color = getOption("lab.color"),
      cex = getOption("lab.cex")
    )
    lab.x <- list(
      color = getOption("lab.x.color"),
      cex = getOption("lab.x.cex")
    )
    lab.y <- list(
      color = getOption("lab.y.color"),
      cex = getOption("lab.y.cex")
    )

    main <- list(
      color = getOption("main.color"),
      cex = getOption("main.cex")
    )

    grid <- list(
      color = getOption("grid.color"),
      lwd = getOption("grid.lwd"),
      lty = getOption("grid.lty")
    )
    grid.x <- list(
      color = getOption("grid.x.color"),
      lwd = getOption("grid.x.lwd"),
      lty = getOption("grid.x.lty")
    )
    grid.y <- list(
      color = getOption("grid.y.color"),
      lwd = getOption("grid.y.lwd"),
      lty = getOption("grid.y.lty")
    )

    strip <- list(
      fill = getOption("strip.fill"),
      color = getOption("strip.color"),
      text.color = getOption("strip.text.color")
    )

    add <- list(
      fill = getOption("add.fill"),
      trans = getOption("add.trans"),
      color = getOption("add.color"),
      cex = getOption("add.cex"),
      lwd = getOption("add.lwd"),
      lty = getOption("add.lty")
    )

    output <- list(
      results = getOption("results"),
      explain = getOption("explain"),
      interpret = getOption("interpret"),
      document = getOption("document"),
      code = getOption("code")
    )

    # create main list
    gp <- list(
      theme = getOption("theme"),
      sub.theme = getOption("sub.theme"),

      window.fill = getOption("window.fill"),
      panel = panel,

      bar = bar,
      pt = pt,

      VBS = VBS,
      ellipse = ellipse,

      fit.color = getOption("fit.color"),
      fit.lwd = getOption("fit.lwd"),
      se.fill = getOption("se.fill"),
      bubble.text.color = getOption("bubble.text.color"),
      heat = getOption("heat"),
      segment.color = getOption("segment.color"),
      ID.color=getOption("ID.color"),
      area.fill=getOption("area.fill"),

      axis = axis,
      axis.x = axis.x,
      axis.y = axis.y,
      rotate = rotate,

      lab = lab,
      lab.x = lab.x,
      lab.y = lab.y,
      main = main,

      grid = grid,
      grid.x = grid.x,
      grid.y = grid.y,

      strip = strip,

      add = add,

      n.cat = getOption("n.cat"),
      suggest = getOption("suggest"),
      quiet = getOption("quiet"),
      brief = getOption("brief"),

      output = output
      
    )
  }

  invisible(gp)

}

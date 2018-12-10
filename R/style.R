style <-
function(
  theme=c("colors", "lightbronze", "dodgerblue", "darkred", "gray",
      "gold", "darkgreen", "blue", "red", "rose", "green", "purple",
      "sienna", "brown", "orange", "white"),
  sub.theme=c("default", "black", "wsj"),
  set=NULL, get=FALSE, reset=TRUE,

  window.fill=getOption("window.fill"),
  panel.fill=getOption("panel.fill"),
  panel.color=getOption("panel.color"),
  panel.lwd=getOption("panel.lwd"),
  panel.lty=getOption("panel.lty"),

  fill=NULL,
  bar.fill=getOption("bar.fill"),
  bar.fill.discrete=getOption("bar.fill.discrete"),
  bar.fill.ordered=getOption("bar.fill.ordered"),
  trans=NULL,
  trans.bar.fill=getOption("trans.bar.fill"),
  color=NULL,
  bar.color=getOption("bar.color"),
  bar.color.ordered=getOption("bar.color.ordered"),
  bar.color.discrete=getOption("bar.color.discrete"),
  values=getOption("values"),
  values.color=getOption("values.color"), 
  values.cex=getOption("values.cex"),
  values.digits=getOption("values.digits"),
  values.pos=getOption("values.pos"),
 
  pt.fill=getOption("pt.fill"),
  trans.pt.fill=getOption("trans.pt.fill"),
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
     theme <- "colors"
     miss.theme <- FALSE    
     cat("theme set to \"colors\"\n\n")
  }
  else {
    miss.theme <- ifelse (missing(theme), TRUE, FALSE)
  }
  if (miss.theme)
    theme <- getOption("theme")
  else
    theme <- match.arg(theme)

  # reset: make change with new options, usually TRUE, but could be ...
  # set: save current options, get: get current options
  if (show || get) reset <- FALSE

  miss.set <- ifelse (missing(set), TRUE, FALSE) 
  miss.tr.bar.fill <- ifelse (missing(trans.bar.fill), TRUE, FALSE)

  miss.sub.theme <- ifelse (missing(sub.theme), TRUE, FALSE)
  if (sub.theme[1] == "colors") {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "\"colors\" now a theme, the default theme\n\n")
  }
  sub.theme <- match.arg(sub.theme)
  
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
  # restore values according to previously saved  set
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

    values <- set$bar$values
    values.color <- set$bar$values.color
    values.cex <- set$bar$values.cex
    values.digits <- set$bar$values.digits
    values.pos <- set$bar$values.pos
 
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
  if (reset) { 
    if (!miss.theme) {
      suppressPackageStartupMessages(.onAttach())
      miss.theme <- FALSE
      theme <- match.arg(theme) 
      options(theme=theme)
      options(trans.bar.fill=0.10)
      options(trans.pt.fill=0.00) 
    }
    else if (miss.set)
      theme <- getOption("theme")

    if (!miss.sub.theme)
      options(sub.theme=sub.theme)
    else
      sub.theme <- getOption("sub.theme")
  }


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
  bar.fill.discrete[which(bar.fill.discrete == "off")] <- "transparent"
  bar.fill.ordered[which(bar.fill.ordered == "off")] <- "transparent"
  pt.fill[which(pt.fill == "off")] <- "transparent"
  bar.color.discrete[which(bar.color.discrete == "off")] <- "transparent"
  bar.color.ordered[which(bar.color.ordered == "off")] <- "transparent"
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

  # see if a pre-defined color range, if not return the calling color
  bar.fill <- .color.range(bar.fill, 24, no.change=TRUE)
  bar.color <- .color.range(bar.color, 24, no.change=TRUE)
  pt.fill <- .color.range(pt.fill, 24, no.change=TRUE)
  pt.color <- .color.range(pt.color, 24, no.change=TRUE)
  add.fill <- .color.range(add.fill, 24, no.change=TRUE)
  add.color <- .color.range(add.color, 24, no.change=TRUE)

  # default transparency levels
  if (reset) {
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
  }


  # ---------------
  # set the options
  if (reset) {

    options(theme = theme)
    options(sub.theme = sub.theme)

    options(bar.fill = bar.fill) 
    options(bar.fill.discrete = bar.fill.discrete) 
    options(bar.fill.ordered = bar.fill.ordered) 
    options(bar.color.discrete = bar.color.discrete) 
    options(bar.color.ordered = bar.color.ordered) 
    options(pt.color = pt.color) 
    
    options(values=values)
    options(values.color=values.color)
    options(values.cex=values.cex)
    options(values.digits=values.digits)
    options(values.pos=values.pos)
    
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
  }
 

  # only run if theme is specified, resets all parameters


  # set colors
  if (theme == "colors") {clr1 <- "dodgerblue3"; clr2 <- "steelblue4"}
  if (theme == "lightbronze") {clr1 <- rgb(247,242,230, maxColorValue=255)}
  if (theme == "dodgerblue") {clr1 <- "dodgerblue3"; clr2 <- "steelblue4"}
  if (theme == "darkred") {clr1 <- "darkred"; clr2 <- rgb(80,0,0,
           maxColorValue=256)}
  if (theme == "gray") {clr1 <- "gray25"}
  if (theme == "gold") {clr1 <- "goldenrod3"; clr2 <- "goldenrod4"}
  if (theme == "darkgreen") {
    clr1 <- "darkgreen"; clr2 <- rgb(0,80,0,maxColorValue=256)
  }
  if (theme == "blue") {clr1 <- "royalblue1"; clr2 <- "royalblue4"}
  if (theme == "red") {clr1 <- "firebrick3"; clr2 <- "firebrick4"}
  if (theme == "rose") {clr1 <- "rosybrown3"; clr2 <- "rosybrown4"}
  if (theme == "green") {clr1 <- "darkgreen"; clr2 <- "darkseagreen4"}
  if (theme == "purple") {clr1 <- "purple1"; clr2 <- "purple4"}
  if (theme == "sienna") {clr1 <- "sienna3"; clr2 <- "sienna4"}
  if (theme == "brown") {clr1 <- "rosybrown4"; clr2 <- "rosybrown3"}
  if (theme == "orange") {clr1 <- "orange"; clr2 <- "orange3"}
  if (theme == "white") {clr1 <- "white"; clr2 <- "black"}


  if (!miss.theme) {

    if (theme == "white") {
      window.fill = "white"
      panel.fill = "white"
      bar.fill = "white"
      bar.fill.ordered = "white"
      bar.fill.discrete = "white"
      bar.color.discrete = "black"
      bar.color.ordered = "black"      
      values.color = "black"
      pt.fill = "white"
      pt.color = "black"
      bubble.text.color = "black"
      ellipse.fill = .maketrans("gray55", 55)
      ellipse.color = "black"
      fit.color = "gray15"
      violin.fill = "white"
      violin.color = "black"
      box.fill = "white"
      box.color = "black"
      area.fill = "black"
      se.fill = .maketrans("gray10", 40)
      grid.color = "gray85"
      out.fill = "black"
      out.color = "black"
      out2.fill = "gray25"
      out2.color = "gray25"
    }

    else if (theme == "gray") {
      window.fill = "white"
      panel.fill = "white"
      bar.fill = .maketrans("gray25", .to256("trans.bar.fill"))
      bar.fill.discrete = .maketrans("gray25",.to256("trans.bar.fill"))
      bar.fill.ordered = .maketrans("gray25", .to256("trans.bar.fill"))
      bar.color.discrete = "gray60"
      bar.color.ordered = "gray60"
      pt.fill = "gray20"
      trans.pt.fill = 0.00
      pt.color = "gray20"
      violin.fill=.maketrans("gray50", 40)
      violin.color = "gray15" 
      box.fill="gray75"
      box.color = "gray15" 
      ellipse.fill = .maketrans("gray35", 15)
      fit.color = "black"
      area.fill = .maketrans("gray25", .to256("trans.bar.fill"))
      se.fill = .maketrans("gray10", 40) 
      heat = "gray5"
      segment.color = "gray20"
      grid.color = "gray85"
      out.fill = "black"
      out.color = "black"
      out2.fill = "black"
      out2.color = "black"
    }

    else if (theme == "lightbronze") {
      window.fill = rgb(247,242,230, maxColorValue=255)
      panel.fill = rgb(247,242,230, maxColorValue=255)
      #panel.fill = "transparent"
      panel.color = rgb(222,217,205, maxColorValue=255)
      #bar.fill = .maketrans("gray50", .to256("trans.bar.fill")))  # 230
      bar.fill = rgb(123,140,150, maxColorValue=255)  
      bar.fill.ordered = rgb(123,140,150, maxColorValue=255)  
      bar.fill.discrete = rgb(123,140,150, maxColorValue=255)  
      bar.color.discrete = "transparent"
      bar.color.ordered = rgb(126,144,168, maxColorValue=255)
      pt.fill = rgb(70,80,90, maxColorValue=255)
      trans.pt.fill = 0.00
      pt.color = rgb(70,80,90, maxColorValue=255)
      ellipse.fill = .maketrans("gray50", 50)
      ellipse.color = "gray20"
      se.fill = .maketrans("gray10", 40) 
      violin.fill = rgb(144,165,175, maxColorValue=255)
      violin.color = "gray15" 
      box.fill = .maketrans("gray15", 35) 
      box.color = "gray15" 
      strip.fill = .maketrans("gray55")
      fit.color = "gray15"
      area.fill = .maketrans("gray50", .to256("trans.bar.fill"))
      heat = "gray30"
      main.color = "gray15"
      lab.color = "gray15"
      axis.color = "gray15"
      axis.text.color = "gray15"
      segment.color = "gray50" 
      strip.color = "gray55" 
      strip.text.color = "gray15" 
      ellipse.color = "gray15"
      bubble.text.color = rgb(247,242,230, maxColorValue=255)
      grid.color = rgb(222,217,205, maxColorValue=255)
      trans = 0
    }

    else if (theme == "colors") {
      panel.fill = "white"
      window.fill = getOption("panel.fill")
      bar.fill.discrete = "hues"
      bar.fill.ordered = rgb(144,165,175, maxColorValue=255)
      bar.color.discrete = "transparent"
      bar.color.ordered = rgb(126,144,168, maxColorValue=255)
      pt.fill = rgb(70,80,90, maxColorValue=255)
      trans.pt.fill = 0.00
      pt.color = rgb(70,80,90, maxColorValue=255)
      box.fill = getColors("hues")
      violin.fill = .maketrans(hcl(240,20,55), 90)
      area.fill = "gray50"
      grid.color = rgb(222,217,205, maxColorValue=255)
      values = "%"
    }

    else {  # process the other theme colors

      window.fill = "white"
      panel.fill = "grey99"
      bar.fill = .maketrans(clr1, .to256("trans.bar.fill"))
      bar.fill.discrete = .maketrans(clr1, .to256("trans.bar.fill"))
      bar.fill.ordered = .maketrans(clr1, .to256("trans.bar.fill"))
      pt.fill = .maketrans(clr1, .to256("trans.pt.fill"))
      bar.color.discrete = clr2
      bar.color.ordered = clr2
      pt.color = clr2
      if (theme %in% c("darkred", "red", "rose"))  # kludge until all HCL colors 
        box.fill <- hcl(0,40,55) 
      else if (theme %in% c("dodgerblue", "blue"))
        box.fill <- hcl(240,40,55)
      else if (theme %in% c("darkgreen", "green"))
        box.fill <- hcl(120,40,55)
      else {
        violin.fill = .maketrans(clr1, 125)  # smaller, more trans 
        box.fill = .maketrans(clr1, 35)
      }
      violin.color = "gray15"
      box.color = "gray15"
      se.fill = .maketrans(clr1, 40)
      ellipse.fill = .maketrans(clr1, 15)
      if (ellipse.color[1] != "transparent")
        ellipse.color = .maketrans(clr1, 200)
      heat = clr2
      segment.color = clr1
      bubble.text.color = "black"
      strip.fill = .maketrans(clr1, 55) 
      strip.color = clr2 
      strip.text.color = clr2 
    }
  }  # not miss theme 


  # sub.theme
  if (!miss.sub.theme) {

    if (sub.theme == "default") {
      panel.fill = "grey95"
      if (theme == "white") panel.fill = "white"
      window.fill = "white"
      grid.x.color = "white"
      grid.y.color = "white"
      lab.color = "black"
      main.color = "black"
    }

    else if (sub.theme == "wsj")  {
      if (!(theme %in% c("gray", "white"))) {
        window.fill = rgb(247,242,230, maxColorValue=255)
        panel.fill = rgb(247,242,230, maxColorValue=255)
      }
      else {
        window.fill = "gray93"
        panel.fill = "gray93"
      }
  #   window.fill = getOption("panel.fill")
      panel.color = "transparent"
      axis.y.color = "transparent"
      gxs <- ifelse (getOption("window.fill") == "#040404", "white", "#040404")
      grid.y.color = gxs
      grid.x.color = "transparent"
      grid.lty = "dotted"
      grid.lwd = 1
    }
   
    else if (theme ==  "gray"  &&  sub.theme == "black") {
      window.fill = "gray10"
      panel.fill = "gray10"
      panel.color = "gray80"
      trans.bar.fill = 0.55
      trans.pt.fill = 0.0
      bar.fill = .maketrans("gray55", .to256("trans.bar.fill"))
      bar.fill.discrete =
        .maketrans("gray55", .to256("trans.bar.fill"))
      bar.fill.ordered =
        .maketrans("gray55", .to256("trans.bar.fill"))
      bar.color.discrete = "gray20"
      bar.color.ordered = "gray20"
      pt.fill = .maketrans("gray75", .to256("trans.pt.fill"))
      pt.color = "gray90"
      violin.fill = .maketrans("gray85", 160)
      violin.color = "gray15"
      box.fill = .maketrans("gray15", 35)
      box.color = "gray15"
      area.fill = "gray70"
      ellipse.fill = .maketrans("gray55", 65)
      fit.color = "gray75"
      area.fill = .maketrans("gray55", .to256("trans.bar.fill"))
      se.fill = .maketrans("gray55", 65)
      heat = "gray30"
      strip.color = .maketrans(clr1, .to256n(0.40))
      strip.text.color = "gray65"
      segment.color = "gray65"
      lab.color = "gray85"
      main.color = "gray85"
      axis.x.color = "gray85"
      axis.y.color = "gray85"
      axis.text.color = "gray85"
      grid.color = "gray25"
      add.color = "gray55"
      values.color = "gray85"
      heat = "gray30"
      clr1 <- "gray55"
    }
  
    else if (sub.theme == "black") {

      window.fill = rgb(.015,.015,.015)
      panel.fill = rgb(.015,.015,.015)
      grid.color = "gray25"
      panel.color = "gray80"
      segment.color = "gray65"
      lab.color = "gray85"
      main.color = "gray85"
      axis.x.color = "gray85"
      axis.y.color = "gray85"
      axis.text.color = "gray85"
      add.color = "gray55"
      values.color = "gray85"
      heat = "gray30"

      if (sum(col2rgb(panel.fill)) < 370) {
        strip.color = .maketrans(clr1, .to256n(0.40))
        strip.text.color = "gray65"
      }

      else if (theme ==  "orange"  &&  sub.theme == "black") {
        if (miss.tr.bar.fill) trans.bar.fill = .05
        bar.fill.discrete = rgb(139,69,0, alpha=.to256("trans.bar.fill"),
              maxColorValue=256)
        bar.fill.ordered = rgb(139,69,0, alpha=.to256("trans.bar.fill"),
              maxColorValue=256)
        pt.fill = rgb(139,69,0, alpha=.to256("trans.pt.fill"),
              maxColorValue=256)
        bar.color.discrete = "orange4"
        bar.color.ordered = "orange4"
        pt.color = rgb(139,69,0, maxColorValue=256)
        ellipse.fill = rgb(249,99,2, alpha=45, maxColorValue=256)
        fit.color = rgb(209,87,3, maxColorValue=256)
        area.fill = rgb(249,99,2, alpha=.to256("trans.bar.fill"),
              maxColorValue=256)
        heat="darkorange3"
        segment.color = rgb(249,99,2, maxColorValue=256)
        clr1 <- rgb(249,99,2, maxColorValue=256)
      }
    }

  }  # end not miss sub.theme

  if (show) {
    .style.show()
  }


  # set the options
  if (reset) {

    options(theme = theme)
    options(sub.theme = sub.theme)

    options(bar.fill = bar.fill) 
    options(bar.fill.discrete = bar.fill.discrete) 
    options(bar.fill.ordered = bar.fill.ordered) 
    options(bar.color.discrete = bar.color.discrete) 
    options(bar.color.ordered = bar.color.ordered) 
    options(pt.color = pt.color) 
    
    options(values=values)
    options(values.color=values.color)
    options(values.cex=values.cex)
    options(values.digits=values.digits)
    options(values.pos=values.pos)
    
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
  }
  

  # ---------------------------------------
  # get current parameter values
  # create a list of sub-lists
  if (get) {

    panel <- list(
      fill = getOption("panel.fill"),
      color = getOption("panel.color"),
      lwd = getOption("panel.lwd"),
      lty = getOption("panel.lty")
    )

    bar <- list(
      fill = getOption("bar.fill"),
      bar.fill.discrete = getOption("bar.fill.discrete"),
      bar.fill.ordered = getOption("bar.fill.ordered"),
      trans.fill = getOption("trans.bar.fill"),
      color = getOption("bar.color"),
      values = getOption("values"),
      values.color = getOption("values.color"),
      values.cex = getOption("values.cex"),
      values.digits = getOption("values.digits"),
      values.pos = getOption("values.pos")
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
  }
  

  # ---------------------------------------
  # create list of current parameter values
  # create sub-lists

  panel <- list(
    fill = panel.fill,
    color = panel.color,
    lwd = panel.lwd,
    lty = panel.lty
  )


  bar <- list(
    fill = bar.fill,
    bar.fill.discrete = bar.fill.discrete,
    bar.fill.ordered = bar.fill.ordered,
    trans.fill = trans.bar.fill,
    color = bar.color,
    values = values,
    values.color = values.color,
    values.cex = values.cex,
    values.digits = values.digits,
    values.pos = values.pos
  )

  pt <- list(
    fill = pt.fill,
    trans.fill = trans.pt.fill,
    color = pt.color,
    out.fill=out.fill,
    out.color=out.color,
    out2.fill=out2.fill,
    out2.color=out2.color
  )

  VBS <- list(
    violin.fill = violin.fill,
    violin.color = violin.color,
    box.fill = box.fill,
    box.color = box.color
  )

  ellipse <- list(
    fill = ellipse.fill,
    color = ellipse.color
  )

  axis <- list(
    color = axis.color,
    lwd = axis.lwd,
    lty = axis.lty,
    cex = axis.cex,
    text.color = axis.text.color
  )
  axis.x <- list(
    color = axis.x.color,
    lwd = axis.x.lwd,
    lty = axis.x.lty,
    cex = axis.x.cex,
    text.color = axis.x.text.color
  )
  axis.y <- list(
    color = axis.y.color,
    lwd = axis.y.lwd,
    lty = axis.y.lty,
    cex = axis.y.cex,
    text.color = axis.y.text.color
  )

  rotate <- list(
    x = rotate.x,
    y = rotate.y,
    offset = offset
  )

  lab <- list(
    color = lab.color,
    cex = lab.cex
  )
  lab.x <- list(
    color = lab.x.color,
    cex = lab.x.cex
  )
  lab.y <- list(
    color = lab.y.color,
    cex = lab.y.cex
  )

  main <- list(
    color = main.color,
    cex = main.cex
  )

  grid <- list(
    color = grid.color,
    lwd = grid.lwd,
    lty = grid.lty
  )
  grid.x <- list(
    color = grid.x.color,
    lwd = grid.x.lwd,
    lty = grid.x.lty
  )
  grid.y <- list(
    color = grid.y.color,
    lwd = grid.y.lwd,
    lty = grid.y.lty
  )

  strip <- list(
    fill = strip.fill,
    color = strip.color,
    text.color = strip.text.color
  )

  add <- list(
    fill = add.fill,
    trans = add.trans,
    color = add.color,
    cex = add.cex,
    lwd = add.lwd,
    lty = add.lty
  )

  output <- list(
    results = results,
    explain = explain,
    interpret = interpret,
    document = document,
    code = code
  )


  # create main list
  gp <- list(
    theme = theme,
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

  invisible(gp)

}

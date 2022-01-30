style <-
function(
  theme=c("colors", "lightbronze", "dodgerblue", "darkred", "gray",
      "gold", "darkgreen", "blue", "red", "rose", "slatered", "green",
      "purple", "sienna", "brown", "orange", "white", "light"),
  sub_theme=c("default", "black", "wsj"),
  set=NULL, get=FALSE, reset=TRUE,

  window_fill=getOption("window_fill"),
  panel_fill=getOption("panel_fill"),
  panel_color=getOption("panel_color"),
  panel_lwd=getOption("panel_lwd"),
  panel_lty=getOption("panel_lty"),

  fill=NULL,
  bar_fill=getOption("bar_fill"),
  bar_fill_discrete=getOption("bar_fill_discrete"),
  bar_fill_ordered=getOption("bar_fill_ordered"),
  trans=NULL,
  trans_bar_fill=getOption("trans_bar_fill"),
  color=NULL,
  bar_color=getOption("bar_color"),
  bar_color_ordered=getOption("bar_color_ordered"),
  bar_color_discrete=getOption("bar_color_discrete"),
  values=getOption("values"),
  values_color=getOption("values_color"), 
  values_size=getOption("values_size"),
  values_digits=getOption("values_digits"),
  values_position=getOption("values_position"),
 
  pt_fill=getOption("pt_fill"),
  trans_pt_fill=getOption("trans_pt_fill"),
  pt_color=getOption("pt_color"),
  se_fill=getOption("se_fill"),
  ellipse_fill=getOption("ellipse_fill"),
  ellipse_color=getOption("ellipse_color"),
  ellipse_lwd=getOption("ellipse_lwd"),
  fit_color=getOption("fit_color"),
  fit_lwd=getOption("fit_lwd"),
  bubble_text_color=getOption("bubble_text_color"),
  segment_color=getOption("segment_color"),
  ID_color=getOption("ID_color"),
  out_fill=getOption("out_fill"),
  out_color=getOption("out_color"),
  out2_fill=getOption("out2_fill"),
  out2_color=getOption("out2_color"),

  violin_fill=getOption("violin_fill"),
  violin_color=getOption("violin_color"),
  box_fill=getOption("box_fill"),
  box_color=getOption("box_color"),

  axis_color=getOption("axis_color"),
  axis_x_color=getOption("axis_x_color"),
  axis_y_color=getOption("axis_y_color"),
  axis_lwd=getOption("axis_lwd"),
  axis_x_lwd=getOption("axis_x_lwd"),
  axis_y_lwd=getOption("axis_y_lwd"),
  axis_lty=getOption("axis_lty"),
  axis_x_lty=getOption("axis_x_lty"),
  axis_y_lty=getOption("axis_y_lty"),
  axis_cex=getOption("axis_cex"),
  axis_x_cex=getOption("axis_x_cex"),
  axis_y_cex=getOption("axis_y_cex"),
  axis_text_color=getOption("axis_text_color"),
  axis_x_text_color=getOption("axis_x_text_color"),
  axis_y_text_color=getOption("axis_y_text_color"),
  rotate_x=getOption("rotate_x"),
  rotate_y=getOption("rotate_y"),
  offset=getOption("offset"),

  lab_color=getOption("lab_color"),
  lab_x_color=getOption("lab_x_color"),
  lab_y_color=getOption("lab_y_color"),
  lab_cex=getOption("lab_cex"),
  lab_x_cex=getOption("lab_x_cex"),
  lab_y_cex=getOption("lab_y_cex"),
  main_color=getOption("main_color"),
  main_cex=getOption("main_cex"),

  grid_color=getOption("grid_color"),
  grid_x_color=getOption("grid_x_color"),
  grid_y_color=getOption("grid_y_color"),
  grid_lwd=getOption("grid_lwd"),
  grid_x_lwd=getOption("grid_x_lwd"),
  grid_y_lwd=getOption("grid_y_lwd"),
  grid_lty=getOption("grid_lty"),
  grid_x_lty=getOption("grid_x_lty"),
  grid_y_lty=getOption("grid_y_lty"),

  strip_fill=getOption("strip_fill"),
  strip_color=getOption("strip_color"),
  strip_text_color=getOption("strip_text_color"),

  add_fill=getOption("add_fill"),
  add_trans=getOption("add_trans"),
  add_color=getOption("add_color"),
  add_cex=getOption("add_cex"),
  add_lwd=getOption("add_lwd"),
  add_lty=getOption("add_lty"),

  n_cat=getOption("n_cat"), suggest=getOption("suggest"),
  notes=getOption("notes"),
  quiet=getOption("quiet"), brief=getOption("brief"),

  results=getOption("results"), explain=getOption("explain"),
  interpret=getOption("interpret"), document=getOption("document"), 
  code=getOption("code"),

  width=120, show=FALSE, ...) {

  # a dot in a parameter name to an underscore
  dots <- list(...)
  if (!is.null(dots)) if (length(dots) > 0) {
    for (i in 1:length(dots)) {
      if (names(dots)[i] == "values.cex")  values_size <- dots[[i]]
      if (grepl(".", names(dots)[i], fixed=TRUE)) {
        nm <- gsub(".", "_", names(dots)[i], fixed=TRUE)
        assign(nm, dots[[i]])
        get(nm)
      }
    }
  }

  if (nargs() == 0) {
     theme <- "colors"
     miss_theme <- FALSE    
     if (!quiet) cat("theme set to \"colors\"\n\n")
  }
  else {
    miss_theme <- ifelse (missing(theme), TRUE, FALSE)
  }
  if (miss_theme)
    theme <- getOption("theme")
  else
    theme <- match.arg(theme)

  # reset: make change with new options, usually TRUE, but could be ...
  # set: save current options, get: get current options
  if (show || get) reset <- FALSE


  miss_set <- ifelse (missing(set), TRUE, FALSE) 
  miss_tr.bar_fill <- ifelse (missing(trans_bar_fill), TRUE, FALSE)

  miss_sub_theme <- ifelse (missing(sub_theme), TRUE, FALSE)
  if (sub_theme[1] == "colors") {
  cat("\n"); stop(call.=FALSE, "\n","------\n",
      "\"colors\" now a theme, the default theme\n\n")
  }
  sub_theme <- match.arg(sub_theme)
  
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
          "invoke option  ghost  as style(sub_theme=\"black\")\n\n")
    }
    if (names(dots)[i] == "gray.black") {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "now invoke style(\"gray\", sub_theme=\"black\")\n\n")
    }
  }

  # ------------------------------------------------
  # restore values according to previously saved  set
  if (!is.null(set)) {

    theme <- set$theme
    sub_theme <- set$sub_theme

    window_fill <- set$window_fill
    panel_fill <- set$panel$fill
    panel_color <- set$panel$color
    panel_lwd <- set$panel$lwd
    panel_lty <- set$panel$lty

    bar_fill <- set$bar$fill
    trans_bar_fill <- set$bar$trans_fill
    bar_color <- set$bar$color

    values <- set$bar$values
    values_color <- set$bar$values_color
    values_size <- set$bar$values_size
    values_digits <- set$bar$values_digits
    values_position <- set$bar$values_position
 
    pt_fill <- set$pt$fill
    trans_pt_fill <- set$pt$trans_fill
    pt_color <- set$pt$color
    out_fill <- set$pt$out_fill
    out_color <- set$pt$out_color
    out2_fill <- set$pt$out2_fill
    out2_color <- set$pt$out2_color

    violin_fill <- set$VBS$violin_fill
    violin_color <- set$VBS$violin_color
    box_fill <- set$VBS$box_fill
    box_color <- set$VBS$box_color

    ellipse_fill <- set$ellipse$fill
    ellipse_color <- set$ellipse$color
    ellipse_lwd <- set$ellipse$lwd

    fit_color <- set$fit_color
    fit_lwd <- set$fit_lwd
    se_fill <- set$se_fill
    bubble_text_color <- set$bubble_text_color
    segment_color <- set$segment_color
    ID_color <- set$ID_color

    axis_color <- set$axis$color
    axis_x_color <- set$axis_x$color
    axis_y_color <- set$axis_y$color
    axis_lwd <- set$axis$lwd
    axis_x_lwd <- set$axis_x$lwd
    axis_y_lwd <- set$axis_y$lwd
    axis_lty <- set$axis$lty
    axis_x_lty <- set$axis_x$lty
    axis_y_lty <- set$axis_y$lty
    axis_cex <- set$axis$cex
    axis_x_cex <- set$axis_x$cex
    axis_y_cex <- set$axis_y$cex
    axis_text_color <- set$axis$text_color
    axis_x_text_color <- set$axis_x$text_color
    axis_y_text_color <- set$axis_y$text_color
    rotate_x <- set$rotate$x
    rotate_y <- set$rotate$y
    offset <- set$rotate$offset

    lab_color <- set$lab$color
    lab_x_color <- set$lab_x$color
    lab_y_color <- set$lab_y$color
    lab_cex <- set$lab$cex
    lab_x_cex <- set$lab_x$cex
    lab_y_cex <- set$lab_y$cex
    main_color <- set$main$color
    main_cex <- set$main$cex

    grid_color <- set$grid$color
    grid_x_color <- set$grid_x$color
    grid_y_color <- set$grid_y$color
    grid_lwd <- set$grid$lwd
    grid_x_lwd <- set$grid_x$lwd
    grid_y_lwd <- set$grid_y$lwd
    grid_lty <- set$grid$lty
    grid_x_lty <- set$grid_x$lty
    grid_y_lty <- set$grid_y$lty

    strip_fill <- set$strip$fill
    strip_color <- set$strip$color
    strip_text_color <- set$strip$text_color

    add_fill <- set$add$fill
    add_trans <- set$add$trans
    add_color <- set$add$color
    add_cex <- set$add$cex
    add_lwd <- set$add$lwd
    add_lty <- set$add$lty

    n_cat <- set$n_cat
    suggest <- set$suggest
    notes <- set$notes
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
    if (!miss_theme) {
      suppressPackageStartupMessages(.onAttach())
      bar_fill <- getOption("bar_fill")
      miss_theme <- FALSE
      theme <- match.arg(theme) 
      options(theme=theme)
      options(trans_bar_fill=0.10)
      options(trans_pt_fill=0.00) 
    }
    else if (miss_set)
      theme <- getOption("theme")

    if (!miss_sub_theme)
      options(sub_theme=sub_theme)
    else
      sub_theme <- getOption("sub_theme")
  }


  # inheritance
  if (!missing(trans)) {
    trans_pt_fill <- trans
    trans_bar_fill <- trans
  }
  if (!missing(fill)) {
    pt_fill <- fill
    bar_fill <- fill
  }
  if (!missing(color)) {
    pt_color <- color
    bar_color <- color
  }
  if (!missing(axis_color)) {
    axis_x_color <- axis_color
    axis_y_color <- axis_color
  }
  if (!missing(axis_cex)) {
    axis_x_cex <- axis_cex
    axis_y_cex <- axis_cex
  }
  if (!missing(lab_color)) {
    lab_x_color <- lab_color
    lab_y_color <- lab_color
  }
  if (!missing(lab_cex)) {
    lab_x_cex <- lab_cex
    lab_y_cex <- lab_cex
  }
  if (!missing(grid_color)) {
    grid_x_color <- grid_color
    grid_y_color <- grid_color
  }
  if (!missing(grid_lwd)) {
    grid_x_lwd <- grid_lwd
    grid_y_lwd <- grid_lwd;
  }
  if (!missing(grid_lty)) {
    grid_x_lty <- grid_lty
    grid_y_lty <- grid_lty
  }

  # "off" is "transparent"
  bar_fill[which(bar_fill == "off")] <- "transparent"
  bar_fill_discrete[which(bar_fill_discrete == "off")] <- "transparent"
  bar_fill_ordered[which(bar_fill_ordered == "off")] <- "transparent"
  pt_fill[which(pt_fill == "off")] <- "transparent"
  bar_color_discrete[which(bar_color_discrete == "off")] <- "transparent"
  bar_color_ordered[which(bar_color_ordered == "off")] <- "transparent"
  pt_color[which(pt_color == "off")] <- "transparent"
  violin_fill[which(violin_fill == "off")] <- "transparent"
  violin_color[which(violin_color == "off")] <- "transparent"
  box_fill[which(box_fill == "off")] <- "transparent"
  box_color[which(box_color == "off")] <- "transparent"
  se_fill[which(se_fill == "off")] <- "transparent"
  ellipse_fill[which(ellipse_fill == "off")] <- "transparent"
  ellipse_color[which(ellipse_color == "off")] <- "transparent"
  if (panel_fill == "off") panel_fill <- "transparent"
  if (!is.null(grid_x_color))
    if (grid_x_color == "off") grid_x_color <- "transparent"
  if (!is.null(grid_y_color))
    if (grid_y_color == "off") grid_y_color <- "transparent"
  if (grid_lwd == "off") grid_lwd <- 0
  if (grid_lty == "off") grid_lty <- "blank"
  if (window_fill == "off") window_fill <- "white"
  if (panel_color == "off") panel_color <- "transparent"
  if (panel_lwd == "off") panel_lwd <- "transparent"
  if (panel_lty == "off") panel_lty <- "transparent"
  if (lab_color == "off") lab_color <- "transparent"
  if (main_color == "off") main_color <- "transparent"
  if (!is.null(axis_x_color))
    if (axis_x_color == "off") axis_x_color <- "transparent"
  if (!is.null(axis_y_color))
    if (axis_y_color == "off") axis_y_color <- "transparent"
  if (!is.null(lab_x_color))
    if (lab_x_color == "off") lab_x_color <- "transparent"
  if (!is.null(lab_y_color))
    if (lab_y_color == "off") lab_y_color <- "transparent"
  if (segment_color == "off") segment_color <- "transparent"
  if (ID_color == "off") ID_color <- "transparent"
  if (out_fill == "off") out_fill <- "transparent"
  if (out_color == "off") out_color <- "transparent"
  if (out2_fill == "off") out2_fill <- "transparent"
  if (out2_color == "off") out2_color <- "transparent"
  if (bubble_text_color == "off") bubble_text_color <- "transparent"
  add_color[which(add_color == "off")] <- "transparent"
  add_fill[which(add_fill == "off")] <- "transparent"

  # see if a pre-defined color range, if not return the calling color
  if (length(bar_fill) == 0) bar_fill <- getOption("bar_fill_discrete")
  bar_fill <- .color_range(bar_fill, 24)
  if (length(bar_color) == 0) bar.color <- getOption("bar_color_discrete")
  bar_color <- .color_range(bar_color, 24)
  if (length(pt_fill) == 0) pt_fill <- getOption("pt_fill")
  pt_fill <- .color_range(pt_fill, 24)
  if (length(pt_color) == 0) pt_color <- getOption("pt_color")
  pt_color <- .color_range(pt_color, 24)
  if (length(add_fill) == 0) add_fill <- getOption("add_fill")
  if (length(add_color) == 0) add_color <- getOption("add_color")
    add_color <- .color_range(add_color, 24)

  # default transparency levels
  if (reset) {
    if (!is.null(trans_bar_fill)) {
      options(trans_bar_fill=trans_bar_fill)
      if (!is.null(bar_fill))
        options(bar_fill = .maketrans(bar_fill, 
                .to256("trans_bar_fill")))
    }
    if (!is.null(trans_pt_fill)) {
      options(trans_pt_fill=trans_pt_fill)
      options(pt_fill = .maketrans(getOption("pt_fill"), .to256("trans_pt_fill")))
    }

    if (!is.null(bar_fill)) {
      if (bar_fill[1] == "transparent")
        options(bar_fill = bar_fill) 
      else
        options(bar_fill = .maketrans(bar_fill, .to256("trans_bar_fill")))
    }
    if (!is.null(pt_fill)) {
      if (pt_fill[1] == "transparent")
        options(pt_fill = pt_fill) 
      else
        options(pt_fill = .maketrans(pt_fill, .to256("trans_pt_fill")))
    }
  }


  # ---------------
  # set the options
  if (reset) {

    options(theme = theme)
    options(sub_theme = sub_theme)

    options(bar_fill = bar_fill) 
    options(bar_fill_discrete = bar_fill_discrete) 
    options(bar_fill_ordered = bar_fill_ordered) 
    options(bar_color_discrete = bar_color_discrete) 
    options(bar_color_ordered = bar_color_ordered) 
    options(pt_color = pt_color) 
    
    options(values=values)
    options(values_color=values_color)
    options(values_size=values_size)
    options(values_digits=values_digits)
    options(values_position=values_position)
    
    options(window_fill=window_fill)
    options(panel_fill=panel_fill)
    options(panel_color=panel_color)
    options(panel_lwd=panel_lwd)
    options(panel_lty=panel_lty)

    options(violin_fill=violin_fill)
    options(violin_color=violin_color)
    options(box_fill=box_fill)
    options(box_color=box_color)

    options(ellipse_fill=ellipse_fill)
    options(ellipse_color=ellipse_color)
    options(ellipse_lwd=ellipse_lwd)
    options(fit_color=fit_color)
    options(fit_lwd=fit_lwd)
    options(se_fill=se_fill)
    options(segment_color=segment_color)
    options(ID_color=ID_color)
    options(out_fill=out_fill)
    options(out_color=out_color)
    options(out2_fill=out2_fill)
    options(out2_color=out2_color)
    options(bubble_text_color=bubble_text_color)

    options(grid_color=grid_color)
    options(grid_x_color=grid_x_color)
    options(grid_y_color=grid_y_color)
    options(grid_lwd=grid_lwd)
    options(grid_x_lwd=grid_x_lwd)
    options(grid_y_lwd=grid_y_lwd)
    options(grid_lty=grid_lty)
    options(grid_x_lty=grid_x_lty)
    options(grid_y_lty=grid_y_lty)

    options(lab_color=lab_color)
    options(lab_x_color=lab_x_color)
    options(lab_y_color=lab_y_color)
    options(lab_cex=lab_cex)
    options(lab_x_cex=lab_x_cex)
    options(lab_y_cex=lab_y_cex)
    options(main_color=main_color)
    options(main_cex=main_cex)
    options(axis_color=axis_color)
    options(axis_x_color=axis_x_color)
    options(axis_y_color=axis_y_color)
    options(axis_lwd=axis_lwd)
    options(axis_x_lwd=axis_x_lwd)
    options(axis_y_lwd=axis_y_lwd)
    options(axis_lty=axis_lty)
    options(axis_x_lty=axis_x_lty)
    options(axis_y_lty=axis_y_lty)

    options(axis_cex=axis_cex)
    options(axis_x_cex=axis_x_cex)
    options(axis_y_cex=axis_y_cex)
    options(axis_text_color=axis_text_color)
    options(axis_x_text_color=axis_x_text_color)
    options(axis_y_text_color=axis_y_text_color)
    options(rotate_x=rotate_x)
    options(rotate_y=rotate_y)
    options(offset=offset)

    options(add_fill=add_fill)
    options(add_trans=add_trans)
    options(add_cex=add_cex)
    options(add_lwd=add_lwd)
    options(add_lty=add_lty)
    options(add_color=add_color)

    options(strip_fill=strip_fill)
    options(strip_color=strip_color)
    options(strip_text_color=strip_text_color)

    options(quiet=quiet)
    options(brief=brief)
    options(n_cat=n_cat)
    options(suggest=suggest)
    options(notes=notes)
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
  if (theme == "slatered") {clr1 <- rgb(.64,.34,.39); clr2 <- "darkred"}
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
  if (theme == "light") {clr1 <- "white"; clr2 <- "black"}


  if (!miss_theme) {  # process the theme colors

    if (theme == "colors") {
      panel_fill = "white"
      window_fill = getOption("panel_fill")
      bar_fill_discrete = "hues"
      bar_fill_ordered = rgb(144,165,195, maxColorValue=255)
      bar_color_discrete = "transparent"
      bar_color_ordered = rgb(126,144,168, maxColorValue=255)
      pt_fill = rgb(50,78,92, maxColorValue=255)
      pt_color = rgb(50,78,92, maxColorValue=255)
      trans_bar_fill = 0.00
      trans_pt_fill = 0.00
      box_fill = "#419BD2"  # getColors("hues", output=FALSE)
      violin_fill = "#7485975A"
      ellipse_fill = .maketrans(hcl(50,20,55), 40)
      grid_color = rgb(222,217,205, maxColorValue=255)
      ID_color = "gray50"
      fit_color = rgb(92,64,50, maxColorValue = 255)
      values = "%"
    }

    else if (theme == "white") {
      window_fill = "white"
      panel_fill = "white"
      bar_fill = "white"
      bar_fill_ordered = "white"
      bar_fill_discrete = "white"
      bar_color_discrete = "black"
      bar_color_ordered = "black"      
      values_color = "black"
      pt_fill = "white"
      pt_color = "black"
      bubble_text_color = "black"
      ellipse_fill = .maketrans("gray55", 55)
      ellipse_color = "black"
      fit_color = "gray15"
      violin_fill = "white"
      violin_color = "black"
      box_fill = "white"
      box_color = "black"
      se_fill = .maketrans("gray10", 40)
      grid_color = "gray85"
      out_fill = "black"
      out_fill = "black"
      out_color = "black"
      out2_fill = "gray25"
      out2_color = "gray25"
    }

    else if (theme == "gray") {
      window_fill = "white"
      panel_fill = "white"
      bar_fill = .maketrans("gray25", .to256("trans_bar_fill"))
      bar_fill_discrete = .maketrans("gray35",.to256("trans_bar_fill"))
      bar_fill_ordered = .maketrans("gray35", .to256("trans_bar_fill"))
      bar_color_discrete = "gray60"
      bar_color_ordered = "gray60"
      pt_fill = "gray30"
      trans_pt_fill = 0.00
      pt_color = "gray30"
      violin_fill=.maketrans("gray50", 40)
      violin_color = "gray15" 
      box_fill="gray65"
      box_color = "gray15" 
      ellipse_fill = .maketrans("gray35", 15)
      fit_color = "black"
      se_fill = .maketrans("gray10", 40) 
      segment_color = "gray20"
      grid_color = "gray85"
      ID_color = "black"
      out_fill = "black"
      out_color = "black"
      out2_fill = "black"
      out2_color = "black"
    }

    else if (theme == "lightbronze") {
      window_fill = rgb(247,242,230, maxColorValue=255)
      panel_fill = rgb(247,242,230, maxColorValue=255)
      #panel_fill = "transparent"
      panel_color = rgb(222,217,205, maxColorValue=255)
      #bar_fill = .maketrans("gray50", .to256("trans_bar_fill")))  # 230
      bar_fill = rgb(123,140,150, maxColorValue=255)  
      bar_fill_ordered = rgb(123,140,150, maxColorValue=255)  
      bar_fill_discrete = rgb(123,140,150, maxColorValue=255)  
      bar_color_discrete = "transparent"
      bar_color_ordered = rgb(126,144,168, maxColorValue=255)
      pt_fill = rgb(70,80,90, maxColorValue=255)
      trans_pt_fill = 0.00
      pt_color = rgb(70,80,90, maxColorValue=255)
      ellipse_color = "gray20"
      se_fill = .maketrans("gray10", 40) 
      violin_fill = "#7485975A"
#     violin_fill = rgb(144,165,175, maxColorValue=255)
      violin_color = "gray15" 
      box_fill = .maketrans("gray15", 170) 
      box_color = "gray15" 
      ellipse_fill = .maketrans("gray50", 50)
      strip_fill = .maketrans("gray55")
      fit_color = "gray15"
      main_color = "gray15"
      lab_color = "gray15"
      axis_color = "gray15"
      axis_text_color = "gray15"
      segment_color = "gray50" 
      strip_color = "gray55" 
      strip_text_color = "gray15" 
      ellipse_color = "gray15"
      bubble_text_color = rgb(247,242,230, maxColorValue=255)
      grid_color = rgb(222,217,205, maxColorValue=255)
      ID_color = "gray15"
      trans = 0
    }

    else if (theme == "light") {
      se_fill = rgb(229,229,248,170,maxColorValue=255)
      ellipse_color = rgb(103,103,176,maxColorValue=255)
      ellipse_fill = "transparent"
      out_fill = rgb(193,36,36,maxColorValue=255)
      out_color = rgb(193,36,36,maxColorValue=255)
      pt_fill = rgb(60,170,225,maxColorValue=255)
      pt_color = rgb(70,78,161,maxColorValue=255)
      grid_color = rgb(229,229,248,150,maxColorValue=255)
      fit_color = "gray50"
    }

    else {  # process the other theme colors

      window_fill = "white"
      panel_fill = "grey99"
      bar_fill = .maketrans(clr1, .to256("trans_bar_fill"))
      violin_fill = "#7485975A"
#     violin_fill = .maketrans(clr1, 30)
      violin_color = "gray15"
      box_fill = .maketrans(clr1, 160)  # larger number, more opaque
#     box_fill = .maketrans(clr1, 65)
      box_color = "gray15"
      bar_fill_discrete = .maketrans(clr1, .to256("trans_bar_fill"))
      bar_fill_ordered = .maketrans(clr1, .to256("trans_bar_fill"))
      pt_fill = .maketrans(clr1, .to256("trans_pt_fill"))
      bar_color_discrete = clr2
      bar_color_ordered = clr2
      pt_fill <- clr1
      pt_color = clr2
      se_fill = .maketrans(clr1, 40)
      ellipse_fill = .maketrans(clr1, 15)
      if (ellipse_color[1] != "transparent")
        ellipse_color = .maketrans(clr1, 200)
      segment_color = clr1
      bubble_text_color = "black"
      strip_fill = .maketrans(clr1, 55) 
      strip_color = clr2 
      strip_text_color = clr2 
      ID_color = "gray50"
    }
  }  # not miss theme 


  # sub_theme
  if (!miss_sub_theme) {

    if (sub_theme == "default") {
      panel_fill = "grey95"
      if (theme == "white") panel_fill = "white"
      window_fill = "white"
      grid_x_color = "white"
      grid_y_color = "white"
      lab_color = "black"
      main_color = "black"
    }

    else if (sub_theme == "wsj")  {
      if (!(theme %in% c("gray", "white"))) {
        window_fill = rgb(247,242,230, maxColorValue=255)
        panel_fill = rgb(247,242,230, maxColorValue=255)
      }
      else {
        window_fill = "gray93"
        panel_fill = "gray93"
      }
  #   window_fill = getOption("panel_fill")
      panel_color = "transparent"
      axis_y_color = "transparent"
      gxs <- ifelse (getOption("window_fill") == "#040404", "white", "#040404")
      grid_y_color = gxs
      grid_x_color = "transparent"
      grid_lty = "dotted"
      grid_lwd = 1
    }
   
    else if (!miss_theme  &&  theme == "gray"  &&  sub_theme == "black") {
      window_fill = "gray10"
      panel_fill = "gray10"
      panel_color = "gray80"
      trans_bar_fill = 0.0
      trans_pt_fill = 0.0
      bar_fill = .maketrans("gray58", .to256("trans_bar_fill"))
      bar_fill_discrete =
        .maketrans("gray58", .to256("trans_bar_fill"))
      bar_fill_ordered =
        .maketrans("gray58", .to256("trans_bar_fill"))
      bar_color_discrete = "gray20"
      bar_color_ordered = "gray20"
      pt_fill = .maketrans("gray75", .to256("trans_pt_fill"))
      pt_color = "gray90"
      violin_fill = "gray80"
      violin_color = "gray15"
      box_fill = .maketrans("gray15", 180)
      box_color = "gray15"
      ellipse_fill = .maketrans("gray55", 65)
      fit_color = "gray75"
      se_fill = .maketrans("gray55", 65)
      ID_color = "gray90"
      strip_color = .maketrans(clr1, .to256n(0.40))
      strip_text_color = "gray65"
      segment_color = "gray65"
      lab_color = "gray85"
      main_color = "gray85"
      axis_x_color = "gray85"
      axis_y_color = "gray85"
      axis_text_color = "gray85"
      grid_color = "gray25"
      add_color = "gray55"
      values_color = "gray85"
      clr1 <- "gray55"
    }
  
    else if (sub_theme == "black") {

      window_fill = rgb(.015,.015,.015)
      panel_fill = rgb(.015,.015,.015)
      violin_fill = "gray80"
      violin_color = "gray15"
      grid_color = "gray25"
      ID_color = "white"
      panel_color = "gray80"
      segment_color = "gray65"
      lab_color = "gray85"
      main_color = "gray85"
      axis_x_color = "gray85"
      axis_y_color = "gray85"
      axis_text_color = "gray85"
      add_color = "gray55"
      values_color = "gray85"
      strip_text_color = "white"
      fit_color <- ifelse (theme == "light", "gray40", "gray75")

      if (theme == "colors") {
        ellipse_color <- "gray75"
        se_fill <- rgb(240,240,240, alpha=45, maxColorValue=256)
        pt_fill <- "royalblue1"
        pt_color <- "royalblue1"
      }

      if (sum(col2rgb(panel_fill)) < 370) {
        strip_color = .maketrans(clr1, .to256n(0.40))
        strip_text_color = "gray65"
      }
      else if (theme ==  "orange"  &&  sub_theme == "black") {
        if (miss_tr.bar_fill) trans_bar_fill = .05
        bar_fill_discrete = rgb(139,69,0, alpha=.to256("trans_bar_fill"),
              maxColorValue=256)
        bar_fill_ordered = rgb(139,69,0, alpha=.to256("trans_bar_fill"),
              maxColorValue=256)
        pt_fill = rgb(139,69,0, alpha=.to256("trans_pt_fill"),
              maxColorValue=256)
        bar_color_discrete = "orange4"
        bar_color_ordered = "orange4"
        pt_color = rgb(139,69,0, maxColorValue=256)
        ellipse_fill = rgb(249,99,2, alpha=45, maxColorValue=256)
#       fit_color = rgb(209,87,3, maxColorValue=256)
        segment_color = rgb(249,99,2, maxColorValue=256)
        ID_color = "orange4"
        clr1 <- rgb(249,99,2, maxColorValue=256)
      }
    }

  }  # end not miss sub_theme

  if (show) {
    .style.show()
  }


  # set the options
  if (reset) {

    options(theme = theme)
    options(sub_theme = sub_theme)

    options(bar_fill = bar_fill) 
    options(bar_fill_discrete = bar_fill_discrete) 
    options(bar_fill_ordered = bar_fill_ordered) 
    options(bar_color_discrete = bar_color_discrete) 
    options(bar_color_ordered = bar_color_ordered) 
    options(pt_fill = pt_fill) 
    options(pt_color = pt_color) 
    options(trans_bar_fill = trans_bar_fill)
    options(trans_pt_fill = trans_pt_fill)
    
    options(values=values)
    options(values_color=values_color)
    options(values_size=values_size)
    options(values_digits=values_digits)
    options(values_position=values_position)
    
    options(window_fill=window_fill)
    options(panel_fill=panel_fill)
    options(panel_color=panel_color)
    options(panel_lwd=panel_lwd)
    options(panel_lty=panel_lty)

    options(violin_fill=violin_fill)
    options(violin_color=violin_color)
    options(box_fill=box_fill)
    options(box_color=box_color)
    options(ellipse_fill=ellipse_fill)
    options(ellipse_color=ellipse_color)
    options(ellipse_lwd=ellipse_lwd)
    options(fit_color=fit_color)
    options(fit_lwd=fit_lwd)
    options(se_fill=se_fill)
    options(segment_color=segment_color)
    options(ID_color=ID_color)
    options(out_fill=out_fill)
    options(out_color=out_color)
    options(out2_fill=out2_fill)
    options(out2_color=out2_color)
    options(bubble_text_color=bubble_text_color)

    options(grid_color=grid_color)
    options(grid_x_color=grid_x_color)
    options(grid_y_color=grid_y_color)
    options(grid_lwd=grid_lwd)
    options(grid_x_lwd=grid_x_lwd)
    options(grid_y_lwd=grid_y_lwd)
    options(grid_lty=grid_lty)
    options(grid_x_lty=grid_x_lty)
    options(grid_y_lty=grid_y_lty)

    options(lab_color=lab_color)
    options(lab_x_color=lab_x_color)
    options(lab_y_color=lab_y_color)
    options(lab_cex=lab_cex)
    options(lab_x_cex=lab_x_cex)
    options(lab_y_cex=lab_y_cex)
    options(main_color=main_color)
    options(main_cex=main_cex)
    options(axis_color=axis_color)
    options(axis_x_color=axis_x_color)
    options(axis_y_color=axis_y_color)
    options(axis_lwd=axis_lwd)
    options(axis_x_lwd=axis_x_lwd)
    options(axis_y_lwd=axis_y_lwd)
    options(axis_lty=axis_lty)
    options(axis_x_lty=axis_x_lty)
    options(axis_y_lty=axis_y_lty)

    options(axis_cex=axis_cex)
    options(axis_x_cex=axis_x_cex)
    options(axis_y_cex=axis_y_cex)
    options(axis_text_color=axis_text_color)
    options(axis_x_text_color=axis_x_text_color)
    options(axis_y_text_color=axis_y_text_color)
    options(rotate_x=rotate_x)
    options(rotate_y=rotate_y)
    options(offset=offset)

    options(add_fill=add_fill)
    options(add_trans=add_trans)
    options(add_cex=add_cex)
    options(add_lwd=add_lwd)
    options(add_lty=add_lty)
    options(add_color=add_color)

    options(strip_fill=strip_fill)
    options(strip_color=strip_color)
    options(strip_text_color=strip_text_color)

    options(quiet=quiet)
    options(brief=brief)
    options(n_cat=n_cat)
    options(suggest=suggest)
    options(notes=notes)
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
      fill = getOption("panel_fill"),
      color = getOption("panel_color"),
      lwd = getOption("panel_lwd"),
      lty = getOption("panel_lty")
    )

    bar <- list(
      fill = getOption("bar_fill"),
      bar_fill_discrete = getOption("bar_fill_discrete"),
      bar_fill_ordered = getOption("bar_fill_ordered"),
      trans_fill = getOption("trans_bar_fill"),
      color = getOption("bar_color"),
      values = getOption("values"),
      values_color = getOption("values_color"),
      values_size = getOption("values_size"),
      values_digits = getOption("values_digits"),
      values_position = getOption("values_position")
    )

    pt <- list(
      fill = getOption("pt_fill"),
      trans_fill = getOption("trans_pt_fill"),
      color = getOption("pt_color"),
      out_fill=getOption("out_fill"),
      out_color=getOption("out_color"),
      out2_fill=getOption("out2_fill"),
      out2_color=getOption("out2_color")
    )

    VBS <- list(
      violin_fill = getOption("violin_fill"),
      violin_color = getOption("violin_color"),
      box_fill = getOption("box_fill"),
      box_color = getOption("box_color")
    )

    ellipse <- list(
      fill = getOption("ellipse_fill"),
      color = getOption("ellipse_color")
    )

    axis <- list(
      color = getOption("axis_color"),
      lwd = getOption("axis_lwd"),
      lty = getOption("axis_lty"),
      cex = getOption("axis_cex"),
      text_color = getOption("axis_text_color")
    )
    axis_x <- list(
      color = getOption("axis_x_color"),
      lwd = getOption("axis_x_lwd"),
      lty = getOption("axis_x_lty"),
      cex = getOption("axis_x_cex"),
      text_color = getOption("axis_x_text_color")
    )
    axis_y <- list(
      color = getOption("axis_y_color"),
      lwd = getOption("axis_y_lwd"),
      lty = getOption("axis_y_lty"),
      cex = getOption("axis_y_cex"),
      text_color = getOption("axis_y_text_color")
    )

    rotate <- list(
      x = getOption("rotate_x"),
      y = getOption("rotate_y"),
      offset = getOption("offset")
    )

    lab <- list(
      color = getOption("lab_color"),
      cex = getOption("lab_cex")
    )
    lab_x <- list(
      color = getOption("lab_x_color"),
      cex = getOption("lab_x_cex")
    )
    lab_y <- list(
      color = getOption("lab_y_color"),
      cex = getOption("lab_y_cex")
    )

    main <- list(
      color = getOption("main_color"),
      cex = getOption("main_cex")
    )

    grid <- list(
      color = getOption("grid_color"),
      lwd = getOption("grid_lwd"),
      lty = getOption("grid_lty")
    )
    grid_x <- list(
      color = getOption("grid_x_color"),
      lwd = getOption("grid_x_lwd"),
      lty = getOption("grid_x_lty")
    )
    grid_y <- list(
      color = getOption("grid_y_color"),
      lwd = getOption("grid_y_lwd"),
      lty = getOption("grid_y_lty")
    )

    strip <- list(
      fill = getOption("strip_fill"),
      color = getOption("strip_color"),
      text_color = getOption("strip_text_color")
    )

    add <- list(
      fill = getOption("add_fill"),
      trans = getOption("add_trans"),
      color = getOption("add_color"),
      cex = getOption("add_cex"),
      lwd = getOption("add_lwd"),
      lty = getOption("add_lty")
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
    fill = panel_fill,
    color = panel_color,
    lwd = panel_lwd,
    lty = panel_lty
  )


  bar <- list(
    fill = bar_fill,
    bar_fill_discrete = bar_fill_discrete,
    bar_fill_ordered = bar_fill_ordered,
    trans_fill = trans_bar_fill,
    color = bar_color,
    values = values,
    values_color = values_color,
    values_size = values_size,
    values_digits = values_digits,
    values_position = values_position
  )

  pt <- list(
    fill = pt_fill,
    trans_fill = trans_pt_fill,
    color = pt_color,
    out_fill=out_fill,
    out_color=out_color,
    out2_fill=out2_fill,
    out2_color=out2_color
  )

  VBS <- list(
    violin_fill = violin_fill,
    violin_color = violin_color,
    box_fill = box_fill,
    box_color = box_color
  )

  ellipse <- list(
    fill = ellipse_fill,
    color = ellipse_color
  )

  axis <- list(
    color = axis_color,
    lwd = axis_lwd,
    lty = axis_lty,
    cex = axis_cex,
    text_color = axis_text_color
  )
  axis_x <- list(
    color = axis_x_color,
    lwd = axis_x_lwd,
    lty = axis_x_lty,
    cex = axis_x_cex,
    text_color = axis_x_text_color
  )
  axis_y <- list(
    color = axis_y_color,
    lwd = axis_y_lwd,
    lty = axis_y_lty,
    cex = axis_y_cex,
    text_color = axis_y_text_color
  )

  rotate <- list(
    x = rotate_x,
    y = rotate_y,
    offset = offset
  )

  lab <- list(
    color = lab_color,
    cex = lab_cex
  )
  lab_x <- list(
    color = lab_x_color,
    cex = lab_x_cex
  )
  lab_y <- list(
    color = lab_y_color,
    cex = lab_y_cex
  )

  main <- list(
    color = main_color,
    cex = main_cex
  )

  grid <- list(
    color = grid_color,
    lwd = grid_lwd,
    lty = grid_lty
  )
  grid_x <- list(
    color = grid_x_color,
    lwd = grid_x_lwd,
    lty = grid_x_lty
  )
  grid_y <- list(
    color = grid_y_color,
    lwd = grid_y_lwd,
    lty = grid_y_lty
  )

  strip <- list(
    fill = strip_fill,
    color = strip_color,
    text_color = strip_text_color
  )

  add <- list(
    fill = add_fill,
    trans = add_trans,
    color = add_color,
    cex = add_cex,
    lwd = add_lwd,
    lty = add_lty
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
    sub_theme = getOption("sub_theme"),

    window_fill = getOption("window_fill"),
    panel = panel,

    bar = bar,
    pt = pt,

    VBS = VBS,
    ellipse = ellipse,

    fit_color = getOption("fit_color"),
    fit_lwd = getOption("fit_lwd"),
    se_fill = se_fill,
    bubble_text_color = getOption("bubble_text_color"),
    segment_color = getOption("segment_color"),
    ID_color=getOption("ID_color"),

    axis = axis,
    axis_x = axis_x,
    axis_y = axis_y,
    rotate = rotate,

    lab = lab,
    lab_x = lab_x,
    lab_y = lab_y,
    main = main,

    grid = grid,
    grid_x = grid_x,
    grid_y = grid_y,

    strip = strip,

    add = add,

    n_cat = getOption("n_cat"),
    suggest = getOption("suggest"),
    notes = getOption("notes"),
    quiet = getOption("quiet"),
    brief = getOption("brief"),

    output = output
    
  )

  return(invisible(gp))

}

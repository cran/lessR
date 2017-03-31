global <-
function(colors=c("blue", "gray", "rose", "green", "gold", "red", "darkred",
         "dodgerblue", "purple", "sienna", "brown", "orange.black",
         "gray.black", "white"),

         fill.bar=NULL, fill.pt=NULL,
         trans=NULL, trans.fill.bar=NULL, trans.fill.pt=NULL,
         stroke.bar=NULL, stroke.pt=NULL,
         se.fill=NULL, fill.ellipse=NULL,
         bg=NULL, grid=NULL, box=NULL,
         heat=NULL, ghost=NULL,

         n.cat=getOption("n.cat"), lab.size=getOption("lab.size"),
         suggest=getOption("suggest"),
         quiet=getOption("quiet"), brief=getOption("brief"),

         results=getOption("results"), explain=getOption("explain"),
         interpret=getOption("interpret"), document=getOption("document"), 
         code=getOption("code"),

         width=120, show=FALSE) {

  if (!is.null(fill.bar)) if (fill.bar[1] == "off")
    fill.bar <- "transparent"
  if (!is.null(fill.pt)) if (fill.pt[1] == "off")
    fill.pt <- "transparent"
  if (!is.null(stroke.bar)) if (stroke.bar[1] == "off")
    stroke.bar <- "transparent"
  if (!is.null(stroke.pt)) if (stroke.pt[1] == "off")
    stroke.bar <- "transparent"
  if (!is.null(se.fill)) if (se.fill[1] == "off")
    se.fill <- "transparent"
  if (!is.null(fill.ellipse)) if (fill.ellipse[1] == "off")
    fill.ellipse <- "transparent"
  if (!is.null(bg)) if (bg == "off") bg <- "transparent"
  if (!is.null(grid)) if (grid == "off") grid <- "transparent"
  if (!is.null(box)) if (box == "off") box <- "transparent"

  # default transparency levels

 if (!missing(colors)) {
    colors <- match.arg(colors) 
    options(colors=colors)
    #if (colors == "dodgerblue")
    options(trans.fill.bar=0.10)
    #else
      #options(trans.fill.bar=0.00)
    options(trans.fill.pt=0.40) 
  }
  if (!missing(trans)) {
    trans.fill.pt <- trans;
    trans.fill.bar <- trans;
  }

  if (!is.null(ghost)) if (ghost) {
    options(trans.fill.bar = 0.50)
    if (getOption("colors") == "dodgerblue")
      options(fill.bar = .maketrans("dodgerblue3", .to256("trans.fill.bar")))
    if (getOption("colors") == "blue")
      options(fill.bar =
                .maketrans("lightsteelblue3", .to256("trans.fill.bar")))
    if (getOption("colors") == "gray")
      options(fill.bar = .maketrans("gray30", .to256("trans.fill.bar")))
    if (getOption("colors") == "green")
      options(fill.bar = rgb(106,127,16,
                alpha=.to256("trans.fill.bar"), maxColorValue=256))
    if (getOption("colors") == "rose")
      options(fill.bar = rgb("rosybrown1",
                alpha=.to256("trans.fill.bar"), maxColorValue=256))
    if (getOption("colors") == "gold")
      options(fill.bar = .maketrans("goldenrod2", .to256("trans.fill.bar")))
    if (getOption("colors") == "red")
      options(fill.bar = .maketrans("firebrick2", .to256("trans.fill.bar")))
    if (getOption("colors") == "darkred")
      options(fill.bar = .maketrans("firebrick3", .to256("trans.fill.bar")))
    if (getOption("colors") == "purple")
      options(fill.bar = .maketrans("purple1", .to256("trans.fill.bar")))
    if (getOption("colors") == "sienna")
      options(fill.bar = .maketrans("sienna3", .to256("trans.fill.bar")))
    if (getOption("colors") == "orange.black")
      options(fill.bar = rgb(249,99,2, alpha=.to256("trans.fill.bar"),
                maxColorValue=256))
    if (getOption("colors") == "gray.black")
      options(fill.bar =  .maketrans("gray80", .to256("trans.fill.bar")))
  }
  else {
    colors <- getOption("colors")
  }

  if (!is.null(trans.fill.bar)) {
    options(trans.fill.bar=trans.fill.bar)
    options(fill.bar = .maketrans(getOption("fill.bar"), .to256("trans.fill.bar")))
  }
  if (!is.null(trans.fill.pt)) {
    options(trans.fill.pt=trans.fill.pt)
    options(fill.pt = .maketrans(getOption("fill.pt"), .to256("trans.fill.pt")))
  }

  if (!is.null(fill.bar))
    if (fill.bar[1] == "transparent")
      options(fill.bar = fill.bar) 
    else
      options(fill.bar = .maketrans(fill.bar, .to256("trans.fill.bar")))
  if (!is.null(fill.pt))
    if (fill.pt[1] == "transparent")
      options(fill.pt = fill.pt) 
    else
      options(fill.pt = .maketrans(fill.pt, .to256("trans.fill.pt")))

  if (!is.null(stroke.bar)) options(stroke.bar = stroke.bar) 
  if (!is.null(stroke.pt)) options(stroke.pt = stroke.pt) 

  if (!is.null(bg)) options(bg=bg)
  if (!is.null(grid)) options(grid=grid)
  if (!is.null(box)) options(box=box)

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

  make.color <- function(color1, color2) {

    if (is.null(fill.bar))
      options(fill.bar = .maketrans(color1, .to256("trans.fill.bar")))
    if (is.null(fill.pt))
      options(fill.pt = .maketrans(color1, .to256("trans.fill.pt")))
    if (is.null(stroke.bar)) options(stroke.bar = color2)
    if (is.null(stroke.pt)) options(stroke.pt = color2)
    if (is.null(se.fill)) options(se.fill = .maketrans(color1, 25)) 
    if (is.null(fill.ellipse))
      options(fill.ellipse = .maketrans(color1, 15))
    if (is.null(heat)) options(heat = color2)

  }


  if (!missing(colors)) {
  # rgb(20,97,172) is dodgerblue 3.5
    theme <- options("colors")

    if (!grepl(".black", theme, fixed=FALSE)) {
      if (is.null(bg)) options(bg = "grey92")
      if (is.null(grid)) options(grid = "white")
    }

    if (theme == "dodgerblue") make.color("dodgerblue3", "steelblue4")
    if (theme == "sienna") make.color("sienna3", "sienna4")
    if (theme == "green") make.color("darkseagreen3", "darkseagreen4")
    if (theme == "rose") make.color("rosybrown3", "rosybrown4")
    if (theme == "gold") make.color("goldenrod2", "goldenrod4")
    if (theme == "red") make.color("firebrick3", "firebrick4")
    if (theme == "darkred") make.color("darkred", rgb(80,0,0, maxColorValue = 256))
    if (theme == "purple") make.color("purple1", "purple4")
    if (theme == "blue") make.color("royalblue1", "royalblue4")
    if (theme == "brown") make.color("rosybrown4", "rosybrown3")
    if (theme == "white") {
      make.color("white", "black")
      if (is.null(bg)) options(bg = "white")
    } 

    if (theme == "gray") {
      if (is.null(fill.bar))
        options(fill.bar = .maketrans("gray25", .to256("trans.fill.bar")))
      if (is.null(fill.pt))
        options(fill.pt = .maketrans("gray20", .to256("trans.fill.pt")))
      if (is.null(stroke.bar)) options(stroke.bar = "gray60")
      if (is.null(stroke.pt)) options(stroke.pt = "black")
      if (is.null(se.fill)) options(se.fill = .maketrans("gray0", 25)) 
      if (is.null(fill.ellipse))
        options(fill.ellipse = .maketrans("gray35", 15))
      if (is.null(heat)) options(heat = "gray5")
    }

    if (theme == "orange.black") {
      if (is.null(fill.bar))
        options(fill.bar = rgb(249,99,2, alpha=.to256("trans.fill.bar"),
        maxColorValue=256))
      if (is.null(fill.pt))
        options(fill.pt = rgb(249,99,2, alpha=.to256("trans.fill.pt"),
        maxColorValue=256))
      if (is.null(stroke.bar))
        options(stroke.bar = rgb(209,87,3, maxColorValue=256))
      if (is.null(stroke.pt))
        options(stroke.pt = rgb(209,87,3, maxColorValue=256))
      if (is.null(fill.ellipse))
        options(fill.ellipse = rgb(249,99,2, alpha=55, maxColorValue=256))
      if (is.null(bg)) options(bg = rgb(.015,.015,.015))
      if (is.null(grid)) options(grid = rgb(100,100,100, maxColorValue=256))
      if (is.null(heat)) options(heat = "darkorange3")
    }
    if (theme == "gray.black") {
      if (is.null(fill.bar))
        options(fill.bar = .maketrans("gray55", .to256("trans.fill.bar")))
      if (is.null(fill.pt))
        options(fill.pt = .maketrans("gray75", .to256("trans.fill.pt")))
      if (is.null(stroke.bar)) options(stroke.bar = "gray20")
      if (is.null(stroke.pt)) options(stroke.pt = "gray95")
      if (is.null(fill.ellipse))
        options(fill.ellipse = .maketrans("gray55", 65))
      if (is.null(bg)) options(bg = rgb(.015,.015,.015))
      if (is.null(grid)) options(grid = "gray30")
      if (is.null(heat)) options(heat = "gray30")
    }
  }

  if (!missing(ghost)) {
    options(ghost=ghost)
    if (ghost) {
      options(bg = "black")
      options(grid = "transparent")
    }
  }

  if (show) {
    cat("\n")
    cat("Note: Colors are in rgb format\n",
        "      A 4th number specifies alpha transparency\n\n", sep="")
    cat("Note: Can use \"off\" to indicate transparent, eg: grid=\"off\"\n\n")

    cat("colors         Color:", getOption("colors"), "\n")
    cat("fill.bar   Bar fill color:",
          col2rgb(getOption("fill.bar"), TRUE), "\n")
    cat("fill.pt    Point fill color:",
          col2rgb(getOption("fill.pt"), TRUE), "\n")
    cat("trans.fill.bar Bar transparency:", getOption("trans.fill.bar"), "\n")
    cat("trans.fill.pt  Point transparency:", getOption("trans.fill.pt"), "\n")
    cat("stroke.bar Bar stroke color:",
           col2rgb(getOption("stroke.bar"), TRUE), "\n")
    cat("stroke.pt  Point stroke color:",
           col2rgb(getOption("stroke.pt"), TRUE), "\n")
    cat("fill.ellipse  Ellipse fill color:",
           col2rgb(getOption("fill.ellipse"), TRUE), "\n")
    cat("bg         Background color:",
           col2rgb(getOption("bg")), "\n")
    cat("grid       Grid color:",
           col2rgb(getOption("grid")), "\n")
    cat("box        Graph Border color:",
           col2rgb(getOption("box")), "\n")
    cat("heat       Heat map color:",
           col2rgb(getOption("heat")), "\n")
    if (is.null(ghost)) ghost <- FALSE
    cat("ghost          Ghost colors (black bck, no grid lines):",
           getOption("ghost"), "\n")
    cat("\n")
    cat("quiet     Suppress console output for many functions:",
           getOption("quiet"), "\n")
    cat("brief     Reduce console output for many functions:",
           getOption("brief"), "\n")
    cat("suggest   Display suggests for enhanced input:",
           getOption("suggest"), "\n")
    cat("n.cat     Number of categories for categorical variable:",
           getOption("n.cat"), "\n")
    cat("lab.size  x and y axis label size:", getOption("lab.size"), "\n")
    cat("width     Column width:", getOption("width"), "\n")
    cat("\n")
  }


}

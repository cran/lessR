set <-
function(colors=c("blue", "gray", "rose", "green", "gold", "red",
         "dodgerblue", "purple", "sienna", "orange.black",
         "gray.black", "white"),

         col.fill.bar=NULL, trans.fill.bar=NULL,
         col.fill.pt=NULL, trans.fill.pt=NULL,
         col.stroke.bar=NULL, col.stroke.pt=NULL, 
         col.bg=NULL, col.grid=NULL, col.heat=NULL, ghost=NULL,

         n.cat=getOption("n.cat"), quiet=getOption("quiet"),
         brief=getOption("brief"), width=120, show=FALSE) {

  to256 <- function(trans.level) trn <- (1-getOption(trans.level))*256

  if (!missing(colors)) {
    colors <- match.arg(colors) 
    options(colors=colors)
    options(trans.fill.bar=0.00)
    options(trans.fill.pt=0.66)
  }

  if (!is.null(ghost)) if (ghost) {
    options(trans.fill.bar = 0.66)
    if (getOption("colors") == "blue")
      options(col.fill.bar = .maketrans("lightsteelblue3", to256("trans.fill.bar")))
    if (getOption("colors") == "gray")
      options(col.fill.bar = .maketrans("gray30", to256("trans.fill.bar")))
    if (getOption("colors") == "green")
      options(col.fill.bar = rgb(106,127,16, alpha=to256("trans.fill.bar"), maxColorValue=256))
    if (getOption("colors") == "rose")
      options(col.fill.bar = rgb(245,213,210, alpha=to256("trans.fill.bar"), maxColorValue=256))
    if (getOption("colors") == "gold")
      options(col.fill.bar = .maketrans("goldenrod2", to256("trans.fill.bar")))
    if (getOption("colors") == "red")
      options(col.fill.bar = .maketrans("firebrick2", to256("trans.fill.bar")))
    if (getOption("colors") == "dodgerblue")
      options(col.fill.bar = .maketrans("dodgerblue3", to256("trans.fill.bar")))
    if (getOption("colors") == "purple")
      options(col.fill.bar = .maketrans("purple1", to256("trans.fill.bar")))
    if (getOption("colors") == "sienna")
      options(col.fill.bar = .maketrans("sienna3", to256("trans.fill.bar")))
    if (getOption("colors") == "orange.black")
      options(col.fill.bar = rgb(249,99,2, alpha=to256("trans.fill.bar"), maxColorValue=256))
    if (getOption("colors") == "gray.black")
      options(col.fill.bar =  .maketrans("gray80", to256("trans.fill.bar")))
  }
  else {
    colors <- getOption("colors")
  }

  if (!is.null(trans.fill.bar)) {
    options(trans.fill.bar=trans.fill.bar)
    options(col.fill.bar = .maketrans(getOption("col.fill.bar"), to256("trans.fill.bar")))
  }
  if (!is.null(trans.fill.pt)) {
    options(trans.fill.pt=trans.fill.pt)
    options(col.fill.pt = .maketrans(getOption("col.fill.pt"), to256("trans.fill.pt")))
  }

  if (!is.null(col.fill.bar))
    if (col.fill.bar == "transparent")
      options(col.fill.bar = col.fill.bar) 
    else
      options(col.fill.bar = .maketrans(col.fill.bar, to256("trans.fill.bar")))
  if (!is.null(col.fill.pt))
    if (col.fill.pt == "transparent")
      options(col.fill.pt = col.fill.pt) 
    else
      options(col.fill.pt = .maketrans(col.fill.pt, to256("trans.fill.pt")))

  if (!is.null(col.stroke.bar)) options(col.stroke.bar = col.stroke.bar) 
  if (!is.null(col.stroke.pt)) options(col.stroke.pt = col.stroke.pt) 

  if (!is.null(col.bg)) options(col.bg=col.bg)
  if (!is.null(col.grid)) options(col.grid=col.grid)

  options(quiet=quiet)
  options(brief=brief)
  options(n.cat=n.cat)
  options(width=width)


  if (!missing(colors)) {
    theme <- options("colors")
    if (theme == "blue") {
      if (is.null(col.fill.bar)) 
        options(col.fill.bar = .maketrans("lightsteelblue3", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("lightsteelblue3", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "slategray")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "darkblue")
      if (is.null(col.bg)) options(col.bg = "ghostwhite")
      if (is.null(col.grid)) options(col.grid = "gray90")
      if (is.null(col.heat)) options(col.heat = "darkblue")
    }
    if (theme == "gray") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("gray30", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("gray30", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "white")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "gray30")
      if (is.null(col.bg)) options(col.bg = "gray91")
      if (is.null(col.grid)) options(col.grid = "white")
      if (is.null(col.heat)) options(col.heat = "gray5")
    }
    if (theme == "green") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = rgb(106,127,16, alpha=to256("trans.fill.bar"),
        maxColorValue=256))
      if (is.null(col.fill.pt))
        options(col.fill.pt = rgb(106,127,16, alpha=to256("trans.fill.pt"),
        maxColorValue=256))
      if (is.null(col.stroke.bar))
        options(col.stroke.bar = rgb(71,67,52, maxColorValue=256))
      if (is.null(col.stroke.pt))
        options(col.stroke.pt = rgb(71,67,52,  maxColorValue=256))
      if (is.null(col.bg)) options(col.bg = rgb(230,220,143, maxColorValue=256))
      if (is.null(col.grid)) options(col.grid = rgb(96,53,29, alpha=50, maxColorValue=256))
      if (is.null(col.heat)) options(col.heat = "darkgreen")
    }
    if (theme == "rose") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = rgb(245,213,210, alpha=to256("trans.fill.bar"),
        maxColorValue=256))
      if (is.null(col.fill.pt))
        options(col.fill.pt = rgb(245,213,210, alpha=to256("trans.fill.pt"),
        maxColorValue=256))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "mistyrose4")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "mistyrose4")
      if (is.null(col.bg)) options(col.bg = "snow1")
      if (is.null(col.grid)) options(col.grid = "snow2")
      if (is.null(col.heat)) options(col.heat = "rosybrown4")
    }
    if (theme == "gold") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("goldenrod2", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("goldenrod2", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "goldenrod4")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "goldenrod4")
      if (is.null(col.bg)) options(col.bg = rgb(255,250,245, maxColorValue=256))
      if (is.null(col.grid)) options(col.grid = rgb(220,222,200, maxColorValue=256))
      if (is.null(col.heat)) options(col.heat = "goldenrod4")
    }
    if (theme == "red") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("firebrick2", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("firebrick2", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "firebrick4")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "firebrick4")
      if (is.null(col.bg)) options(col.bg=rgb(255,251,251, maxColorValue=256))
      if (is.null(col.grid)) if (is.null(col.grid)) options(col.grid="lavenderblush2")
      if (is.null(col.heat)) options(col.heat = "darkred")
    }
    if (theme == "dodgerblue") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("dodgerblue3", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("dodgerblue3", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "steelblue4")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "steelblue4")
      if (is.null(col.bg)) options(col.bg = rgb(232,234,236, maxColorValue=256))
      if (is.null(col.grid)) options(col.grid = "snow3")
      if (is.null(col.heat)) options(col.heat = "dodgerblue4")
    }
    if (theme == "purple") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("purple1", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("purple1", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "purple4")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "purple4")
      if (is.null(col.bg)) options(col.bg = "lavenderblush")
      if (is.null(col.grid)) options(col.grid = "lavenderblush3")
      if (is.null(col.heat)) options(col.heat = "purple4")
    }
    if (theme == "sienna") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("sienna3", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("sienna3", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "sienna4")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "sienna4")
      if (is.null(col.bg)) options(col.bg = "seashell1")
      if (is.null(col.grid)) options(col.grid = "seashell2")
      if (is.null(col.heat)) options(col.heat = "sienna3")
    }
    if (theme == "orange.black") {
      if (is.null(col.fill.bar)) options(col.fill.bar = rgb(249,99,2, alpha=to256("trans.fill.bar"),
        maxColorValue=256))
      if (is.null(col.fill.pt)) options(col.fill.pt = rgb(249,99,2, alpha=to256("trans.fill.pt"),
        maxColorValue=256))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = rgb(209,87,3, maxColorValue=256))
      if (is.null(col.stroke.pt)) options(col.stroke.pt = rgb(209,87,3, maxColorValue=256))
      if (is.null(col.bg)) options(col.bg = rgb(4,4,4, maxColorValue=256))
      if (is.null(col.grid)) options(col.grid = rgb(100,100,100, maxColorValue=256))
      if (is.null(col.heat)) options(col.heat = "darkorange3")
    }
    if (theme == "gray.black") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = .maketrans("gray55", to256("trans.fill.bar")))
      if (is.null(col.fill.pt))
        options(col.fill.pt = .maketrans("gray75", to256("trans.fill.pt")))
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "gray20")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "gray95")
      if (is.null(col.bg)) options(col.bg = "black")
      if (is.null(col.grid)) options(col.grid = "gray30")
      if (is.null(col.heat)) options(col.heat = "gray30")
    }
    if (theme == "white") {
      if (is.null(col.fill.bar))
        options(col.fill.bar = "white")
      if (is.null(col.fill.pt))
        options(col.fill.pt = "white")
      if (is.null(col.stroke.bar)) options(col.stroke.bar = "black")
      if (is.null(col.stroke.pt)) options(col.stroke.pt = "black")
      if (is.null(col.bg)) options(col.bg = "transparent")
      if (is.null(col.grid)) options(col.grid = "gray90")
      if (is.null(col.heat)) options(col.heat = "gray70")
    }
  }

  if (!missing(ghost)) {
    options(ghost=ghost)
    if (ghost) {
      options(col.bg = "black")
      options(col.grid = "transparent")
    }
  }

  if (show) {
    cat("Color:", getOption("colors"), "\n")
    cat("Bar fill color:", getOption("col.fill.bar"), "\n")
    cat("Point fill color:", getOption("col.fill.pt"), "\n")
    cat("Bar transparency:", getOption("trans.fill.bar"), "\n")
    cat("Point transparency:", getOption("trans.fill.pt"), "\n")
    cat("Bar stroke color:", getOption("col.stroke.bar"), "\n")
    cat("Point stroke color:", getOption("col.stroke.pt"), "\n")
    cat("Background color:", getOption("col.bg"), "\n")
    cat("Grid color:", getOption("col.grid"), "\n")
    cat("Heat map color:", getOption("col.heat"), "\n")
    if (is.null(ghost)) ghost <- FALSE
    cat("Ghost colors:", getOption("ghost"), "\n")
    cat("\n")
    cat("Suppress console output for many functions:", getOption("quiet"), "\n")
    cat("Reduce console output for many functions:", getOption("brief"), "\n")
    cat("Number of categories:", getOption("n.cat"), "\n")
    cat("Column width:", getOption("width"), "\n")
  }


}

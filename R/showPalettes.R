showPalettes <-
function(palette="hcl", n=12, border="transparent", file=NULL) {

  if (is.null(file)) {
    file <- paste(palette, ".pdf", sep="")
    pdf(file=file)
  }

  old.par <- par(no.readonly = TRUE)  # save all modifiable settings
  on.exit(par(old.par))
  par(mfrow=c(5,1), mgp=c(0,0,0))

  pal <- function(nm, col) {
    plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1),
         axes=FALSE, xlab=nm, ylab="")
    rect(0:(n-1)/n, 0, 1:n/n, 1, col=col, border=border)
  }

  if (border == "off") border <- "transparent"

  if (palette == "hcl") {
    pal("reds", getColors("reds", n=n))
    pal("rusts", getColors("rusts", n=n))
    pal("browns", getColors("browns", n=n))
    pal("olives", getColors("olives", n=n))
    pal("greens", getColors("greens", n=n))
    pal("emeralds", getColors("emeralds", n=n))
    pal("turquoises", getColors("turquoises", n=n))
    pal("aquas", getColors("aquas", n=n))
    pal("blues", getColors("blues", n=n))
    pal("purples", getColors("purples", n=n))
    pal("violets", getColors("violets", n=n))
    pal("magentas", getColors("magentas", n=n))
    pal("grays", getColors("grays", n=n))
    pal("hues", getColors("hues", n=n))
  }

  if (palette == "viridis") {
    pal("viridis", getColors("viridis", n=n))
    pal("cividis", getColors("cividis", n=n))
    pal("plasma", getColors("plasma", n=n))
    pal("spectral", getColors("spectral", n=n))
  }

  if (palette == "wesanderson") {
    if (!requireNamespace("wesanderson", quietly=TRUE)) {
      stop("Package \"wesanderson\" needed for these colors\n",
           "Please install it:  install.packages(\"wesanderson\")\n\n",
           call. = FALSE)
    }
    pal("BottleRocket1", getColors("BottleRocket1", n=n))
    pal("BottleRocket2", getColors("BottleRocket2", n=n))
    pal("Rushmore1", getColors("Rushmore1", n=n))
    pal("Rushmore", getColors("Rushmore", n=n))
    pal("Royal1", getColors("Royal1", n=n))
    pal("Royal2", getColors("Royal2", n=n))
    pal("Zissou1", getColors("Zissou1", n=n))
    pal("Darjeeling1", getColors("Darjeeling1", n=n))
    pal("Darjeeling2", getColors("Darjeeling2", n=n))
    pal("Chevalier1", getColors("Chevalier1", n=n))
    pal("FantasticFox1", getColors("FantasticFox1", n=n))
    pal("Moonrise1", getColors("Moonrise1", n=n))
    pal("Moonrise2", getColors("Moonrise2", n=n))
    pal("Moonrise3", getColors("Moonrise3", n=n))
    pal("Cavalcanti1", getColors("Cavalcanti1", n=n))
    pal("GrandBudapest1", getColors("GrandBudapest1", n=n))
    pal("GrandBudapest2", getColors("GrandBudapest2", n=n))
    pal("IsleofDogs1", getColors("IsleofDogs1", n=n))
    pal("IsleofDogs2", getColors("IsleofDogs2", n=n))
  }

  dev.off()
  .showfile(file, "file of R colors")

  }

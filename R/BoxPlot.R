BoxPlot <-
function(...) {

cat("\n")
cat("The BoxPlot function is deprecated\n")
cat("Use Plot instead, which, for a continuous variable,\n")
cat("  generates an integrated Violin/Box/Scatterplot, a VBS plot\n")
cat("\n")
cat("If only a box plot is wanted, add the parameter\n")
cat("      vbs.plot=\"b\"\n")
cat("to the function call to Plot\n")
cat("The default is vbs.plot=\"vbs\", that is, all three plots\n")
cat("\n")
cat("For a Trellis plot, multiple panels, one for each level of,\n")
cat("  a variable named X\n")
cat("Add the parameter by1=X, and by2 for two categorical variables\n")
cat("And the parameter by=X for multiple levels on the same panel\n")
cat("\n")
}



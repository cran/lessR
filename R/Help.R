Help <- 
function(topic=NULL) {


  help.more <-
    function(fname, yline) {
      h1 <- "Complete list of Help topics, enter:  Help()"
        h2 <- paste("For more help on a function, enter ? in front of its name:  ?", fname, sep="")

        if (getOption("colors") != "gray") 
    col.sep <- "lightsteelblue"
  else
    col.sep <- "gray50"
  lines(c(5,90), c(yline,yline), col=col.sep)
  text(0,yline-5, label=h1, adj=0)
  text(0,yline-10, label=h2, adj=0)
}


# set up plot window
set.up.plot <- 
function(nlines) {
  old.opt <- options()
  on.exit(options(old.opt))
  graphics.off()  # graphics get a clean start
  dev.new(width=5.5, height=5.5)
  par(mar=c(.5,.5,.5,.5), bg=rgb(255,253,250,maxColorValue=255),
                          fg=rgb(20,15,15,maxColorValue=255), cex=.8)
  plot.new()
  plot.window(xlim=c(0,100), ylim=c(0,100))
  if (!missing(nlines)) {
    ybot <- 100 - ((nlines+1) * 4)
    rect(-1, ybot, 95, 96, col=col.rect, lwd=.75, border=col.line)
  }
  options(old.opt)
}


if (missing(topic))
  topic <- NULL
else {
  ischar <- tryCatch(is.character(topic) && length(topic) == 1L, error = identity)
  if (inherits(ischar, "error")) ischar <- FALSE
  if (!ischar) {
    stopic <- deparse(substitute(topic))
    topic <- stopic
  }  # else already is a character, i.e., argument entered within quotes
}

# convert topic to all lowercase letters
if (!is.null(topic)) for (i in 1:nchar(topic)) {
  xc <- substr(topic,i,i)
  if (xc %in% LETTERS) substr(topic,i,i) <- letters[which(LETTERS==xc)] 
}

if (getOption("colors") != "gray") {
  col.rect <- getOption("col.fill.pt")
  col.line <- "lightsteelblue"
}
else {
  col.rect <- "gray90"
  col.line <- "gray30"
}

if (is.null(topic)) {

t0 <- "Help Topics for lessR"

fcsv <- bquote(paste(bold("Help(data)"), "  Create a data file from Excel or similar application."))
frw <- bquote(paste(bold("Help(Read)"), " and ", bold("Help(Write)"), "  Read or write data to or from a file."))
flib <- bquote(paste(bold("Help(library)"), "  Access libraries of functions called packages."))
ftrans <- bquote(paste(bold("Help(edit)"), "  Edit data and create new variables from existing variables."))
fsys <- bquote(paste(bold("Help(system)"), "  System level settings, such as a color theme for graphics."))

fhist <- bquote(paste(bold("Help(Histogram)"), "  Histogram, box plot, dot plot, density curve."))
fbar <- bquote(paste(bold("Help(BarChart)"), "  Bar chart, pie chart."))
fline <- bquote(paste(bold("Help(LineChart)"), "  Line chart, such as a run chart or time series chart."))
fplot <- bquote(paste(bold("Help(ScatterPlot)"), "  Scatterplot for one or two variables, a function plot."))

fstat <- bquote(paste(bold("Help(SummaryStats)"), "  Summary statistics for one or two variables."))
fone <- bquote(paste(bold("Help(one.sample)"), "  Analysis of a single sample of data."))
fmean <- bquote(paste(bold("Help(ttest)"), "  Compare two groups by their mean difference."))
faov <- bquote(paste(bold("Help(ANOVA)"), "  Compare mean differences for many groups."))
fpwr <- bquote(paste(bold("Help(power)"), "  Power analysis for the t-test."))
fcor <- bquote(paste(bold("Help(Correlation)"), "  Correlation analysis."))
freg <- bquote(paste(bold("Help(Regression)"), " and ", bold("Help(Logit)"), " Regression analysis, logit analysis."))
ffac <- bquote(paste(bold("Help(factor.analysis)"), "  Confirmatory and exploratory factor analysis."))

fprob <- bquote(paste(bold("Help(prob)"), "  Probabilities for normal and t-distributions."))
frnsm <- bquote(paste(bold("Help(random)"), " and ", bold("Help(sample)"), "  Create random numbers or samples."))

fpdf <- bquote(paste(bold("Help(help.to.pdf)"), "  Obtain a printable pdf of all of the contents."))
fpck <- bquote(paste(bold("Help(lessR)"), "  lessR manual and list of updates to current version."))

set.up.plot()
pos1 <- 93; pos2 <- 69; pos3 <- 49; pos4 <- 14; pos5 <- 7
text(50,100, label=t0, font=4)
text(0,pos1, label=fcsv, adj=0)
text(0,pos1-4, label=frw, adj=0)
text(0,pos1-8, label=flib, adj=0)
text(0,pos1-12, label=ftrans, adj=0)
text(0,pos1-16, label=fsys, adj=0)
lines(c(5,90), c(74,74), col=col.line)
text(0,pos2, label=fhist, adj=0)
text(0,pos2-4, label=fbar, adj=0)
text(0,pos2-8, label=fline, adj=0)
text(0,pos2-12, label=fplot, adj=0)
lines(c(5,90), c(53,53), col=col.line)
text(0,pos3, label=fstat, adj=0)
text(0,pos3-4, label=fone, adj=0)
text(0,pos3-8, label=fmean, adj=0)
text(0,pos3-12, label=faov, adj=0)
text(0,pos3-16, label=fpwr, adj=0)
text(0,pos3-20, label=fcor, adj=0)
text(0,pos3-24, label=freg, adj=0)
text(0,pos3-28, label=ffac, adj=0)
lines(c(5,90), c(18,18), col=col.line)
text(0,pos4, label=fprob, adj=0)
text(0,pos4-4, label=frnsm, adj=0)
lines(c(5,90), c(7,7), col=col.line)
#text(0,pos5, label=fagain, adj=0)
#text(0,pos5-4, label=fpdf, adj=0)
text(0,pos5-4, label=fpck, adj=0)

}


else if (topic %in% c("data", "file", "csv", "sav", "rda")) {
t0 <- "Data Files"

t1 <-
"R can read data files in many formats, including the csv format or
\"comma separated values\". A csv file is a text file with commas that
separate adjacent values in each row. Usually the variable names are in
the first row and each remaining row contains the data for one case, 
such as one person or one company, etc. Each column contains the data
for the corresponding variable.

MS Excel or other worksheet application can save data to a csv file. All 
numeric data should be displayed in the General format, so that the only
non-digit character for each numeric data value is a decimal point. The
The General format removes all dollar signs and commas, for example,
leaving only the pure number, stripped of any extra characters, which R 
will not properly read by default as a numeric data value.

To create the csv file from Excel, under the File option, do a Save As
and choose the csv format. With the free, open source LibreOffice Calc,
after the File option and then Save As, click the arrow in the left margin
towards the bottom labeled File type. From the available options, choose
Text CSV. Then click the Save button and then the OK button. Next, read
the csv data file into R, [see Help(Read)].

Another type of text data file is a fixed width format where each column
of data values is assigned a specific width. Usually there are no spaces
between the data values. R can write a data file [see Help(Write)], what
is called an native R data file with a default file type of .rda. Data
files written by the SPSS system have the default file type of .sav. By
 default both of these files can be read into R as well."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,54, label=t1, adj=0)

help.more("Read", 9)
}


else if (topic %in% c("rd", "read")) {
t0 <- "Read Data into R and Prepare for Analysis"

f1 <- bquote(paste(bold("Read, rd"), "  Read a data file into an R data frame called mydata."))

t1 <-
"Browse for a csv, native R or SPSS data file available on your local computer 
system and read the information into the data table (frame) named: mydata. Use
the function Read, or its abbreviation, rd. To browse, just add ().
    > mydata <- Read()
The  <-  is called the assignment operator and instructs R to take what was
read and assign it to the data table called mydata. This is the default name
that all the lessR data analysis functions assume when reading data for analysis.

Or, specify the file to be read with the name in quotes. The file can be a path
name to a data file available on the local computer system, or a file on the web.
    > mydata <- Read(\"http://web.pdx.edu/~gerbing/data/twogroup.csv\")
For web files, include the  http://.

To see how to create a comma separated values or csv data file: > Help(data)
To see how to create an R data file from within R, enter: > Help(write)

To read a text file where each column of data values is assigned a specific
width, add the widths option that specifies the width of each column
according to the order of the variables.  Enclose the list with the c
function for combine, here to read 3 variables with widths of 4, 1 and 2.
    > mydata <- Read(widths=c(4,1,2), col.names=c(\"ID\", \"Gender\", \"Age\"))

To read a tab-delimmited text file, add the option: sep=\"\\t\"
To read a text file with a , for a decimal, add the options: sep=\";\", dec=\",\"
"

set.up.plot(1)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
#lines(c(5,90), c(89,89), col=col.line)
text(0,48, label=t1, adj=0)

help.more("Read", 9)
}


else if (topic %in% c("wrt", "write")) {
t0 <- "Write Contents of Data Frame mydata into a Data File"

f1 <- bquote(paste(bold("Write, wrt"), "  Write a data file called mydata into an R data frame."))

t1 <-
"The name of the entire rectangular table of data, called a data frame in R, can 
be named mydata within R.  This is the default name of the data table assumed
by the lessR data analysis functions.

Here is how to write the contents of mydata to a csv data file with the name of 
mydata.csv.
    > Write()
The file is written to the default working directory.  The Write function displays
this location after writing the file.

Or, explicitly specify the file name.
    > Write(\"mybestdata\")
The file type of .csv is automatically appended to the file name.

To write a data file in native R format, use the type=\"R\" option, or the 
abbreviation for the function name  wrt.r.
    > wrt.r(\"mybestdata\")

The lessR Write function relies upon the R function write.table, which is
is quite general, with many options.  For more information, enter ?write.table."

set.up.plot(1)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
#lines(c(5,90), c(89,89), col=col.line)
text(0,55, label=t1, adj=0)

help.more("Write", 19)
}


else if (topic %in% c("library", "package", "install", "update"))  {
t0 <- "Contributed Packages"

f1 <- bquote(paste(bold("install.packages"), "  Download a contributed package."))
f2 <- bquote(paste(bold("library"), "  Load an installed package from the library into R for access."))
f3 <- bquote(paste(bold(update.packages), "  Update contributed packages to current versions."))

t1 <-
"All of R works with functions contained in specific packages. The distinction
is that some of those packages are included with the default installation of R, 
and are pre-loaded each time the application is run. Examples are the stat and 
the graphic packages. Other packages must be explicitly downloaded. 

The example here is for the contributed package lessR. Install one time 
only for a specific computer, with quotes.
    > install.packages(\"lessR\")

Each time the R application is started, including after the install, load the 
package from the library, without using quotes.
    > library(lessR)

To see the description of the package and a list of its functions,
    > library(help=lessR)

To access updated versions of all installed packages, 
    > update.packages()

To see a list of all installed packages in the library, 
    > library()"

set.up.plot(3)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
#lines(c(5,90), c(80,80), col=col.line)
text(0,47, label=t1, adj=0)

help.more("install.packages", 12)
}


else if (topic %in% c("edit", "transform", "trans", "factor", "recode", "rec", 
                      "subset", "subs"))  {
t0 <- "Edit Data"

f1 <- bquote(paste(bold("fix"), "  Use a graphical interface to edit data values, add or delete variables."))
f2 <- bquote(paste(bold("Transform"), "  Transform the values of a variable with a formula."))
f3 <- bquote(paste(bold("Recode"), "  Recode the values of a variable by specifying the new values."))
f4 <- bquote(paste(bold("factor"), "  Explicitly define the values of a categorical variable."))
f5 <- bquote(paste(bold("Subset"), "  Extract a subset of data, variables (columns) and/or rows."))

t1 <-
"The R function fix provides a graphical/mouse interface for editing data.
    > fix(mydata)

The lessR function Transform creates a new variable or rewrites over existing.
    > mydata <- Transform(SalaryDiv=Salary/1000)

The lessR function Recode, or just rec, changes individual values. Here change
the values of variable Scores from 1 to 4 to 10, 15, 20, and 25, respectively.
    > mydata <- Recode(Scores, old=c(1:4), new=c(10,15,20,25))

Use the R function factor to create a new variable with non-numeric categories.
Severity was encoded with a 1 for Mild, 2 for Moderate and 3 for Severe.
    > mydata <- Transform(ordered=TRUE, Severity.f= 
                 factor(Severity, levels=c(1,2,3), labels=c(\"Mild\", \"Mod\", \"Severe\")))
Here the values of the new variable are also ordered, from Mild to Severe. 

Extract subsets of data from a data frame with the lessR Subset function.
    > mydata <- Subset(rows=Gender==\"M\", columns=c(Years, Salary))
The data frame, mydata, now consists only of data for Males limited to
the variables Years and Salary. To just display a subset, drop the mydata <-.
"

set.up.plot(5)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
text(0,78, label=f5, adj=0)
#lines(c(5,90), c(78,78), col=col.line)
text(0,40, label=t1, adj=0)

help.more("Recode", 7)
}


else if (topic == "system") {
t0 <- "System Level Settings"

f1 <- bquote(paste(bold("set"), "  lessR function to access to lessR system settings such as a color theme."))
f2 <- bquote(paste(bold("options"), "  Standard R function to access system settings."))

t1 <-
"The lessR function set provides system settings for the lessR system, as well
as some of the more commonly used general R settings. One option is to set
the color theme for the graphics functions. Aspects of these plots can be
customized individually, but the color theme provides a theme of related colors 
for a graph.  The default color theme is colors=\"blue\", with other possibilities of
\"gray\", \"green\", \"gold\", \"rose\", \"red\", \"dodgerblue\", \"purple\", \"sienna\",
\"orange.black\" and \"gray.black\". Here all subsequent graphics are in gray scale.
    > set(colors=\"gray\") 
The transparency level of bars and plotted points is set with the trans.fill.bar and 
trans.fill.pt options. Set ghost=\"TRUE\" to get transparent bars against a black
background with no grid lines.

Levels of a categorical variable may be encoded with numerical digits, such 
as 0 for Male and 1 for Female. R is obliged to interpret numerical variables 
as numeric.  One option is to redefine these variables as factors [see Help(edit)].
Another option is the value of the lessR option n.cat.
    > set(n.cat=3)
Here any variable with just 3 unique values or less is interpreted as a categorical
variable.  The default value of n.cat is 0, that is, turned off.

To see all available standard R options, enter the following.
    > options()
"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(80,80), col=col.line)
text(0,46, label=t1, adj=0)

help.more("set", 9)
}


else if (topic %in% c("histogram", "hs", "hst", "hist", "boxplot", "box", "bx", 
  "dotplot", "dp", "dot", "density", "dn", "dens")) {
t0 <- "Histogram, etc."

f1 <- bquote(paste(bold("Histogram, hs"), "  Histogram."))
f2 <- bquote(paste(bold("Density, dn"), "  Density curve over histogram."))
f3 <- bquote(paste(bold("BoxPlot, bx"), "  Box plot."))
f4 <- bquote(paste(bold("DotPlot, dp"), "  Dot plot."))

t1 <-
"These functions graph a distribution of data values for a continuous variable
such as Time. Replace Y in these examples with the actual variable name.

A histogram, or hs, based on the current color theme, such as the default \"blue\".
    > Histogram(Y)

Specify the gray scale color theme, a title, and a label for the x axis.
    > set(colors=\"gray\")
    > Histogram(Y, main=\"My Title\", xlab=\"Y (mm)\")

Specify bins, starting at 60 with a bin width of 10.
    > Histogram(Y, bin.start=60, bin.width=10)

Density curve superimposed on the underlying histogram, abbreviated dn.
    > Density(Y)

Box plot, abbreviated bx.
    > BoxPlot(Y)

Dot plot, a scatterplot of one variable, abbreviated dp.
    > DotPlot(Y)
"

set.up.plot(4)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
#lines(c(5,90), c(78,78), col=col.line)
text(0,43, label=t1, adj=0)

help.more("Histogram",9)
}


else if (topic %in% c("barchart", "bc", "piechart", "pc", "pareto")) {
t0 <- "BarChart, PieChart and Pareto Chart"

f1 <- bquote(paste(bold("BarChart, bc"), "  Bar chart of the values of one or more categorical variables."))
f2 <- bquote(paste(bold("PieChart, pc"), "  Pie chart of the values of a categorical variable."))
f3 <- bquote(paste(bold("pareto.chart"), "  Produce a Pareto chart."))

t1 <-
"The generic variable in the examples below is generally a categorical variable Y, 
called a factor. Replace with the actual name of the variable in a specific analysis. 

Default bar chart with lessR function BarChart, or bc, as well as the frequency
table, for one or two variables.
    > BarChart(Y)
    > BarChart(Y, by=X)
    
With lessR function PieChart or pc, generate a pie chart and associated frequencies.
    > PieChart(Y)
    
The pareto.chart function is part of the external library called gcc. To view an 
explanation of dealing with libraries, enter Help(\"libraries\"). Default input 
Pareto chart follows, which works from the counts. This function is not from lessR,
so the name of the variable must be preceded by the data frame name and a $. 
    > library(gcc)
    > Ycount <- table(mydata$Y)
    > pareto.chart(Ycount)
"

set.up.plot(3)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
#lines(c(5,90), c(81,81), col=col.line)
text(0,50, label=t1, adj=0)

help.more("BarChart", 20)
}


else if (topic %in% c("linechart", "lc")) {
t0 <- "Line Chart"

f1 <- bquote(paste(bold("LineChart, lc"), "  A line chart, such as a run chart or time series chart."))

t1 <-
"The lessR function LineChart, or lc, generates a line chart, in color, with 
values ordered along some dimension such as time. If the data do not have a 
pronounced trend, a centerline is automatically provided.
    > LineChart(Y)
Also provided is a list of all the runs in the data.

The line chart becomes a time series chart with times/dates on the horizontal
axis.  Use the time.start and time.by options.
    > LineChart(Y, time.start=\"2005/09/01\", time.by=\"month\")

The graphic functions can access a wide range of graphics parameters, such 
as the size of the margins, the annotations, the line width, etc. These additional 
options are explained in the help files for the R functions par, title, points 
and lines. 

Color themes are available with the colors option, which can be invoked
from a specific call to LineChart or system wide for all graphics output with the 
function set. In this example, all subsequent graphics output is in gray scale.
    > set(colors=\"gray\")
    > LineChart(Y)
"

set.up.plot(1)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
#lines(c(5,90), c(90,90), col=col.line)
text(0,53, label=t1, adj=0)

help.more("LineChart", 19)
}


else if  (topic %in% c("scatterplot", "sp", "plot", "scatter")) {
t0 <- "Scatterplot"

f1 <- bquote(paste(bold("ScatterPlot, sp"), "  A scatterplot for one or two variables."))

t1 <-
"ScatterPlot, or sp, generates a scatter plot for either one or two variables.
The points have a default transparency, which can be set from completely
transparent to oblique.  The plot uses the current color theme.

This example is the default scatterplot for variables named X and Y.
    > ScatterPlot(X, Y)
If the values of X are sorted, a function plot is generated instead so that the
points are not individually displayed and are connected by line segments.

Here generate a one dimensional scatterplot, that is, a dot plot, for a
variable named Y. 
    > ScatterPlot(Y)
Can also generate the same plot with the function name of DotPlot or dp.

ScatterPlot can also provide a for plotting two variables with different
symbols and/or colors for each level of a third variable.
    > ScatterPlot(X, Y, by=Z)

Color themes are available with the colors option, from a call to ScatterPlot
or system wide for all graphics output with the function set. In this example,
all subsequent graphics are with the green color theme, with no transparency.
    > set(colors=\"green\", trans.pts=0)
    > ScatterPlot(X, Y)"

set.up.plot(1)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
#lines(c(5,90), c(90,90), col=col.line)
text(0,53, label=t1, adj=0)

help.more("Plot", 12)
}


else if  (topic %in% c("summarystats", "ss", "standard score", "z-score", "scale")) {
t0 <- "Summary Statistics"

f1 <- bquote(paste(bold("SummaryStats, ss"), "  Summarize the values of a variable."))
f2 <- bquote(paste(bold("scale"), "  Standardize the values of a variable."))

t1 <-
"Summarize the variable Y with lessR SummaryStats, or just ss.  If numerical, 
sample size, number of  missing data values, mean, sd, skew, kurtosis, minimum,
maximum, quartiles and interquartile range are provided. If categorical, cell 
counts and proportions, plus the chi-square test are provided.
    > SummaryStats(Y)
A version for abbreviated output also exists.
    > ss.brief(Y)

Or summarize all numerical and non-numerical variables in the data frame mydata.
    > SummaryStats()
    
For a numerical variable Y, provide an optional grouping variable, X, to
summarize at each level of the grouping variable. Or, if Y is categorical, a 
cross- tabulation table is generated.
    > SummaryStats(Y, by=X)

The R scale function generates the standard scores for variable Y. To access a
variable with an R function, provide the data frame name and a $ as a prefix.
    > z <- scale(mydata$Y)"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(85,85), col=col.line)
text(0,52, label=t1, adj=0)
help.more("SummaryStats", 20)
}


else if (topic %in% c("one.sample", "one sample", "proportion", "prop")) {
t0 <- "Inference for a Single Variable"

f1 <- bquote(paste(bold("ttest, tt"), "  Inference for a mean."))
f2 <- bquote(paste(bold("binom.test"), "  Inference for a proportion from exact binomial probability."))
f3 <- bquote(paste(bold("prop.test"), "  Inference for a proportion from approximate normal probability."))

t1 <-
"These inference tests analyze the mean of a numeric variable or the proportion 
of a value of a categorical variable. These tests provide a hypothesis test 
and a confidence interval.

This example uses the lessR function ttest, or tt, to evaluate a variable named
Y and a null hypothesis of mu=100.
    > ttest(Y, mu0=100)

This example uses ttest to do the analysis from the sample statistics.
    > ttest(n=20, m=47.2, s=8.5, mu0=50)
    
Here test for a fair coin after getting 53 out of 100 Heads. The R function
binom.test is based on the exact binomial distribution.  The R prop.test
function returns a chi-square value based on the normal approximation of the
binomial.
    > binom.test(53,100, p=.5)
    > prop.test(53,100, p=.5)

The prop.test function can be specified with or without the Yate's correction for 
continuity factor. The default is to include the correction."

set.up.plot(3)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
#lines(c(5,90), c(82,82), col=col.line)
text(0,49, label=t1, adj=0)

help.more("ttest", 14)
}


else if (topic %in% c("ttest", "t-test", "tt")) {
t0 <- "Compare Two Group Means"

f1 <- bquote(paste(bold("ttest, tt"), "  An enhanced version of t.test to compare two group means."))
f2 <- bquote(paste(bold("Model, model"), "  The t-test if Y is numerical and X has two values."))

t1 <-
"When responses to a variable are organized into two or more groups, compare
the group means with a t-test.  For example, suppose the response variable is 
Salary and the grouping variable is Gender, with two values, M and F.

Here the numerical response variable is named Y and the grouping variable, 
also called a factor, is named X, which must have exactly two values.
    >  ttest(Y ~ X)
When the tilde, ~, expresses the relationship between two or more variables, 
R refers to this expression as a formula, read as: Y is described by X.
Can also specify with the more general function model.
    > Model(Y ~ X)

To do a separate analysis of Y for each group, specify  separate=TRUE  in
the call to ttest to create the vectors group1 and group2.
    > Histogram(group1)

Sometimes the data for a t-test are arranged so that the responses for 
each group, Y, already are in separate columns called vectors. Here calculate 
the t-test directly from two vectors called Y1 and Y2.
    > ttest(Y1, Y2)

Or, directly from summary statistics.
    > ttest(n=34, m=8.92, s=1.67, Ynm=\"Time\")"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(86,86), col=col.line)
text(0,47, label=t1, adj=0)

help.more("ttest", 8)
}

else if (topic %in% c("anova", "av")) {
t0 <- "Compare Means of Two or More Groups"

f1 <- bquote(paste(bold("ANOVA, av"), "  Analysis of variance to compare two or more group means."))
f2 <- bquote(paste(bold("Model, model"), "  ANOVA if explanatory variables are categorical."))
t1 <-
"When responses to a variable are organized into exactly two groups, either the 
t-test function, ttest, or the lessR analysis of variance function, ANOVA, or simply
av, can compare the group means. With more than two groups, ANOVA is
required. Here the numerical response variable is named Y and the grouping
variable, or factor, is X, which may have more than two discrete values. 
    > ANOVA(Y ~ X)
or
    > Model(Y ~ X)
This is called one-way ANOVA because there is only a single factor, X.

If the ANOVA with more than two levels is significant, then a post-hoc
examination of the mean differences with a controlled error rate will help
uncover where the differences occurred. The ANOVA function relies upon the
Tukey HSD procedure.  Both tabular and plotted output are obtained.

For a randomized block ANOVA invoke a blocking factor with a + .
    > ANOVA(Y ~ X + Blck)

For a two way between groups ANOVA, specify two factors with a * .
    > ANOVA(Y ~ X1 * X2)
"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(85,85), col=col.line)
text(0,51, label=t1, adj=0)

help.more("aov", 17)
}



else if (topic == "power") {
t0 <- "Power"

f1 <- bquote(paste(bold("ttestPower, ttp"), "  Power analysis of the t-test."))

t1 <-
"The lessR function, ttestPower, uses the standard R function, power.t.test, to 
calculate a range of power values and automatically provide a power curve. 

To obtain a power curve with power.t.test requires setting up the range of
alternative mean or mean difference values, usually by trial and error, 
invoking ttestPower, saving the results, and then invoking the plot function,
including the labeling of each axis. Then to analyze related results such 
as power at a different sample size, the ttestPower function must be run
 several more times. 

The enhanced function, ttestPower, does all of this automatically for one 
or two sample t-tests, and also plots the power curve in color. This example is 
for the default power curve for a sample size of 20 in each group and 
a within-group or pooled standard deviation of 5.
    > ttestPower(n=20, s=5)
Related analysis is also provided."

set.up.plot(1)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
#lines(c(5lessR ,90), c(89,89), col=col.line)
text(0,62, label=t1, adj=0)

help.more("ttp", 33)
}


else if (topic %in% c("correlation", "cr", "cor", "corr")) {
t0 <- "Correlation and Related Graphics"

f1 <- bquote(paste(bold("Correlation, cr"), "  Correlations between two or more variables."))
f3 <- bquote(paste(bold("ScatterPlot, sp"), "  Graphics, generate a scatterplot for two or more variables."))

t1 <-
"The lessR function Correlation, or cr, can compute a correlation for two variables.
Or for a data frame, mydata by default, the correlation matrix is computed, with
pairwise deletion of missing data by default. A heat map and scatter plot matrix
can also be generated with  graphics=TRUE. The matrix is displayed and also is
stored as mycor such as for a subsequent factor analysis. 

The lessR function, ScatterPlot, or just sp, displays a scatterplot for two variables
or a scatterplot matrix for a data frame. The corresponding correlation or
correlation matrix is also displayed. See Help(ScatterPlot) for more information.

This example is for the correlation coefficient, inference and scatterplot for two 
numerical variables, X and Y, both the correlational analysis by itself, and also
with the scatterplot.
    > Correlation(X,Y)
    > Plot(X,Y)
The brief form for the correlation analysis for two variables also exists.
    > cr.brief(X,Y)

Or, analyze many correlations at once, such as for Y, X1, X2 and X3 in 
the data frame called mydata.
    > mynew <- subset(mydata, select=c(Y,X1:X3))
    > Correlation(mynew)
"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f3, adj=0)
#lines(c(5,90), c(83,83), col=col.line)
text(0,48, label=t1, adj=0)

help.more("Correlation", 12)
}


else if (topic %in% c("regression", "reg")) {
t0 <- "Linear Models and Regression"

f1 <- bquote(paste(bold("Regression, reg"), "  Regression analysis."))
f2 <- bquote(paste(bold("Model, model"), "  Regression analysis if the variables are numerical."))

t1 <-
"The function Regression preforms a regression analysis and stores the 
results in an R object called lm.out, which is available for further analysis. 
This example specifies a multiple regression model with a response variable
named Y and two predictor variables, X1 and X2.
     > Regression(Y ~ X1 + X2)
The standard R formula function specifies the model, which uses the tilde, ~,
to mean 'depends on', and then the plus sign, +, to separate terms.

If all the variables in the model are numerical, then a call to the general
function Model will, in turn, call the Regression function.
     > Model(Y ~ X1 + X2)
The Model function also applies to the analysis of other linear models.

The output of Regression is comprehensive, including a predictor subset
analysis, residuals, prediction intervals, and graphics for error diagnostics.
For only one predictor variable, a scatterplot of the data with included
regression line and prediction and confidence intervals is also provided by
default. A scatterplot matrix is produced for multiple predictor variables.

The abbreviated form of the function is reg, such as
     > reg(Y ~ X1 + X2)
Also obtain abbreviated output from, for example,
     > reg.brief(Y ~ X1 + X2)"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(87,87), col=col.line)
text(0,48, label=t1, adj=0)

help.more("Regression", 9)
}


else if (topic %in% c("logit", "lr")) {
t0 <- "Logit Regression Analysis"

f1 <- bquote(paste(bold("Logit, lr"), "  Logit regression analysis."))
f2 <- bquote(paste(bold("Model, model"), "  Logit analysis if a binary response variable."))

t1 <-
"The function Logit preforms a logit analysis and stores the results in
in an R object called lm.out, which is available for further analysis. 
This example specifies a multiple regression model with a response variable
named Y, with values 0 and 1, and two predictor variables, X1 and X2.
    > Logit(Y ~ X1 + X2)
The standard R formula function specifies the model, which uses the tilde, ~,
to mean 'depends on', and then the plus sign, +, to separate terms.

If the response variable has values of 0 and 1, and all the predictor variables
are numerical, then Model will, in turn, call the Logit function.
    > Model(Y ~ X1 + X2)
The Model function also applies to the analysis of other linear models.

The abbreviated form of the function is lr, such as
     > lr(Y ~ X1 + X2)
"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(87,87), col=col.line)
text(0,56, label=t1, adj=0)

help.more("Logit", 30)
}


else if (topic %in% c("factor.analysis", "fa", "factors", "corcfa", "cfa", "corefa", 
                     "efa", "corScree", "scree")) {
t0 <- "Confirmatory and Exploratory Factor Analysis"

f1 <- bquote(paste(bold("corCFA, cfa"), "  Confirmatory factor analysis."))
f2 <- bquote(paste(bold("corEFA, efa"), "  Exploratory factor analysis."))
f3 <- bquote(paste(bold("corRead, rd.cor"), "  Read a correlation matrix."))
f4 <- bquote(paste(bold("corScree, scree"), "  Scree plot of eigenvalues of the correlation matrix."))
f5 <- bquote(paste(bold("corReorder, reord"), "  Reorder the variables in the correlation matrix."))

t1 <-
"Several lessR functions analyze data in the form of a correlation matrix, by
default called: mycor.  Read mycor with corRead, often with the lessR function,
to, used to name a string of sequential variables (items) with the same prefix.
    > corRead(names=to(\"m\",20))
Here browse for the file that contains the matrix and name the 20 variables
from m01, m02 to m20. Can also compute mycor with the Correlation function.

The function corCFA, or just cfa, does a confirmatory factor analysis of a
multiple indicator measurement model. Specify each group of items by listing
the factor name, Fn for the nth factor, and the corresponding variable names, 
with items separated by commas and a sequence specified with a colon.
    > cfa(F1=V1:V3, F2=V4:V6)

Accomplish exploratory factor analysis with CorEFA, or just efa, which relies
upon a call to the standard R function factanal with the matrix mycor. Here 
extract two factors from mycor.  
    > efa(n.fact=2)
The output includes a specification of the multiple indicator measurement model
derived from the analysis, plus the associated corCFA code to analyze.
"

set.up.plot(5)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
text(0,78, label=f5, adj=0)
#lines(c(5,90), c(75,75), col=col.line)
text(0,40, label=t1, adj=0)

help.more("cfa", 8)
}


else if (topic %in% c("prob", "norm", "pt", "qnorm", "prob.tcut")) {
t0 <- "Probabilities for Normal and t-distributions"

f1 <- bquote(paste(bold("prob.norm"), "  Normal distribution probability over a range of values."))
f2 <- bquote(paste(bold("pt"), "  Probability for a t-distribution related to a specified t-value."))
f3 <- bquote(paste(bold("qnorm"), "  Quantile for a normal distribution."))
f4 <- bquote(paste(bold("prob.tcut"), "  Cutoff values for a t-distribution based on corresponding quantiles."))


t1 <-
"By default, the lessR function prob.norm, provides the corresponding probability
of obtaining a randomly sampled normal value, Y, in a range of specified values, as
well as a plot of the normal curve. The R function pt provides the corresponding
probability for the t-distribution.

Upper tail probability for t=1.627, df=24:  > pt(1.627, df=24, lower.tail=FALSE)
Two-tailed p-value for  t=1.627, df=24:     > 2*pt(1.627, df=24, lower.tail=FALSE)
Probability and curve for a value between 80 and 120 for, mu=100, sigma=15: 
    > prob.norm(lo=80, hi=120, mu=100, sigma=15)

The quantile functions are the inverse of the probability functions. For a given 
probability or area under the curve, the corresponding quantile is the 
corresponding value of the distribution, Y or t. Usually for t, get the cutoff value.

The lessR prob.tcut also provides a graph of the cutoff t-value. Here for  df=24.
    > prob.tcut(df=24)
    
Value from the standard normal distribution that cuts off the top 2.5% of the 
distribution.  Without specifying mu and sigma, the respective defaults are 0 and 1.
    > qnorm(0.025, lower.tail=FALSE)"


set.up.plot(4)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
#lines(c(5,90), c(78,78), col=col.line)
text(0,45, label=t1, adj=0)

help.more("prob.norm", 11)
}


else if (topic %in% c("random", "rnorm", "rbinom")) {
t0 <- "Normal and Binomial Random Values"

f1 <- bquote(paste(bold("rnorm"), "  Generate randomly sampled values from a normal distribution."))
f2 <- bquote(paste(bold("rbinom"), "  Generate randomly sampled values from a binomial distribution."))

t1 <-
"R can generate simulated sampling from many different population
distributions, including the normal and the binomial.

This example generates 50 randomly sampled values from the standard normal 
distribution, with a default mu of 0 and sigma of 1.
    > rnorm(50)
    
The generated data can be stored for further analysis.  Here, generate 100 
values from a normal distribution with a mean of 50 and a standard deviation 
of 10, store in the vector Y, and then display the resulting histogram.
    > Y <- rnorm(100, mean=50, sd=10)
    > Histogram(Y)
    
The binomial distribution describes the process of a binary outcome over 
many different trials, such as flipping a coin.  In this example, flip a fair 
coin 20 times with a probability of a Head at 0.5.  Then repeat this set of 20 
flips 10 times to get the number of Heads obtained on each set of 20 flips.
    > rbinom(10, 20, .5)"

set.up.plot(2)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
#lines(c(5,90), c(85,85), col=col.line)
text(0,54, label=t1, adj=0)

help.more("rnorm", 21)
}


else if (topic == "sample") {
t0 <- "Generate Random Samples"

f1 <- bquote(paste(bold("sample"), "  Generate random samples."))

t1 <-
"To use the sample function, first specify the population from which to randomly 
sample. The population can be defined from the values of a specified variable,
or the values can be directly listed. Next specify the size to specify the
number of elements to sample. By default, sampling is done without replacement,
each value in the population can only appear once in the resulting sample. 

The following randomly samples 5 values of the variable Y without replacement.
    > sample(Y, size=5)
    
If the size of the resulting list of sample values is larger than the available 
number of values from which to sample, then sampling must be done with 
replacement. To allow sampling with replacement, invoke replace=TRUE.
    > Y <- sample(c(\"Head\",\"Tail\"), size=10, replace=TRUE)
Here 10 coin flips are simulated, yielding 10 values of Head or Tail. The
values are stored in the vector Y for further analysis, such as BarChart(Y).
 
The following randomly samples 10 numbers from the first 100 integers, without 
replacement.
    > sample(1:100, size=10)"

set.up.plot(1)
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
#lines(c(5,90), c(90,90), col=col.line)
text(0,57, label=t1, adj=0)

help.more("sample", 22)
}


else if (topic == "help.to.pdf") {
pdf("R_help.pdf")

t1 <- "Contents of the Help Files for R Function Help()"
t2 <- "from the R Contributed Package:"
t3 <- "lessR"
t4 <- "David W. Gerbing"
t5 <- "School of Business Administration"
t6 <- "Portland State University"
t7 <- "Version 2.4, July 10, 2012"
#set.up.plot()
  plot.new()
  plot.window(xlim=c(0,100), ylim=c(0,100))
text(50,84, label=t1)
text(50,80, label=t2)
text(50,76, label=t3)
text(50,58, label=t4)
text(50,54, label=t5)
text(50,50, label=t6)
text(50,24, label=t7)

#Help()
Help("data")
Help("Read")
Help("Write")
Help("library")
Help("transform")
Help("system")
Help("Histogram")
Help("BarChart")
Help("Plot")
Help("LineChart")
Help("SummaryStats")
Help("one.sample")
Help("ttest")
Help("ANOVA")
Help("power")
Help("Correlation")
Help("Regression")
Help("factor.analysis")
Help("prob")
Help("random")
Help("sample")
dev.off()

if (getwd() =="/")
  workdir <- "top level of your file system"
else
  workdir <- getwd()
cat("PDF file of Help contents located at current working directory.\n")
cat("   R_help.pdf at: ", workdir, "\n")

}



else if (topic == "lessr") {

  help(package="lessR")

}


else {
cat("
Value ", topic," for Help not recognized.\n
Complete list of Help topics, enter:  Help()\n
\n")

}

}

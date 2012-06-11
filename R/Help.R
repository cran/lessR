Help <- 
function(topic=NULL) {


help.more <-
function(fname, yline) {
  h1 <- "Complete list of Help topics, enter:  Help()"
  h2 <- paste("For more help on a function, enter ? in front of its name:  ?", fname, sep="")
  lines(c(5,90), c(yline,yline), col="lightsteelblue")
  text(0,yline-5, label=h1, adj=0)
  text(0,yline-10, label=h2, adj=0)
}


# set up plot window
set.up.plot <- 
function() {
  old.opt <- options()
  on.exit(options(old.opt))
  graphics.off()  # graphics get a clean start
  dev.new(width=5.5, height=5.5)
  par(mar=c(.5,.5,.5,.5), bg=rgb(255,253,250,maxColorValue=255),
                          fg=rgb(20,15,15,maxColorValue=255), cex=.8)
  plot.new()
  plot.window(xlim=c(0,100), ylim=c(0,100))
  options(old.opt)
}


col.line <- "lightsteelblue"

if (is.null(topic)) {

t0 <- "Help Topics, lessR v2.3"

fcsv <- bquote(paste(bold("Help(\"data\")"), "  Create data file from Excel or similar application."))
fread <- bquote(paste(bold("Help(\"Read\")"), "  Read data from a file."))
fwrite <- bquote(paste(bold("Help(\"Write\")"), "  Write data to a file."))
flib <- bquote(paste(bold("Help(\"library\")"), "  Access libraries of functions called packages."))

fhist <- bquote(paste(bold("Help(\"Histogram\")"), "  Histogram, box plot, dot plot, density curve."))
fbar <- bquote(paste(bold("Help(\"BarChart\")"), "  Bar chart, pie chart."))
fplot <- bquote(paste(bold("Help(\"Plot\")"), "  Scatterplot for one or two variables, line chart."))
frun <- bquote(paste(bold("Help(\"RunChart\")"), "  Run chart or time series chart."))

fstat <- bquote(paste(bold("Help(\"SummaryStats\")"), "  Summary statistics for one or two variables."))
fone <- bquote(paste(bold("Help(\"one.sample\")"), "  Analysis of a single sample of data."))
fmean <- bquote(paste(bold("Help(\"ttest\")"), "  Compare two groups by their mean difference."))
faov <- bquote(paste(bold("Help(\"ANOVA\")"), "  Compare mean differences for many groups."))
fpwr <- bquote(paste(bold("Help(\"power\")"), "  Power analysis for the t-test."))
fcor <- bquote(paste(bold("Help(\"Correlation\")"), "  Correlation analysis."))
freg <- bquote(paste(bold("Help(\"Regression\")"), "  Regression analysis."))

fprob <- bquote(paste(bold("Help(\"prob\")"), "  Probabilities for normal and t-distributions."))
frand <- bquote(paste(bold("Help(\"random\")"), "  Generate random numbers."))
fsamp <- bquote(paste(bold("Help(\"sample\")"), "  Generate random samples."))

fagain <- bquote(paste(bold("Help()"), "  Repeat this page of help topics."))
fpdf <- bquote(paste(bold("Help(\"help.to.pdf\")"), "  Obtain a printable pdf of all of the contents."))
fpck <- bquote(paste(bold("Help(\"lessR\")"), "  lessR manual and list of updates to current version."))

set.up.plot()
pos1 <- 93; pos2 <- 73; pos3 <- 53; pos4 <- 23; pos5 <- 8
text(50,100, label=t0, font=4)
text(0,pos1, label=fcsv, adj=0)
text(0,pos1-4, label=fread, adj=0)
text(0,pos1-8, label=fwrite, adj=0)
text(0,pos1-12, label=flib, adj=0)
lines(c(5,90), c(77,77), col=col.line)
text(0,pos2, label=fhist, adj=0)
text(0,pos2-4, label=fbar, adj=0)
text(0,pos2-8, label=fplot, adj=0)
text(0,pos2-12, label=frun, adj=0)
lines(c(5,90), c(57,57), col=col.line)
text(0,pos3, label=fstat, adj=0)
text(0,pos3-4, label=fone, adj=0)
text(0,pos3-8, label=fmean, adj=0)
text(0,pos3-12, label=faov, adj=0)
text(0,pos3-16, label=fpwr, adj=0)
text(0,pos3-20, label=fcor, adj=0)
text(0,pos3-24, label=freg, adj=0)
lines(c(5,90), c(26,26), col=col.line)
text(0,pos4, label=fprob, adj=0)
text(0,pos4-4, label=frand, adj=0)
text(0,pos4-8, label=fsamp, adj=0)
lines(c(5,90), c(11,11), col=col.line)
text(0,pos5, label=fagain, adj=0)
#text(0,pos5-4, label=fpdf, adj=0)
text(0,pos5-4, label=fpck, adj=0)

}


else if (topic == "data") {
t0 <- "Data Files"

t1 <-
"R can read data files in the csv, or \"comma separated values\", format, text 
files with commas separating adjacent values in each row. Usually the variable 
names are in the first row and each remaining row contains the data for one
observation, such as one person or one company, etc. Each column of the 
worksheet contains the data for the corresponding variable.

One way to create a csv data file is with MS Excel or other worksheet application. 
All numeric data should be displayed in the General format, so that the only 
non-digit character for each numeric data value is a decimal point. The General 
format removes all dollar signs and commas, for example, leaving only the pure 
number, stripped of any extra characters, which R will not properly read by default 
as a numeric data value.

To create the csv file from a worksheet, under the File option, do a Save As and 
choose the csv format.

Next, read the csv data file into R, [see Help(\"read\")]. However, using a 
worksheet such as Excel and R are complementary procedures.  R can do 
extensive data transformations, such as sorting and much else, but so can Excel, 
and often more directly, without the need for programming.  Given the simplicity 
of transferring data from Excel to R, it is often useful to move back and forth 
between the two systems on a regular basis."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,59, label=t1, adj=0)

help.more("Read", 20)
}


else if (topic == "Read") {
t0 <- "Read Data into R and Prepare for Analysis"

f1 <- bquote(paste(bold("Read, rad"), "  Read a data file into an R data frame called mydata, and more."))

t1 <-
"Browse for a csv, native R or SPSS data file available on the local computer system.
    > Read()

Or, browse using the short form,
    > rad()

Or, specify the file to be read. The file can be a path name to a data file 
available on the local computer system, or to a file on the web.
    > rad(\"http://web.pdx.edu/~gerbing/data/twogroup.csv\")

To see how to create a csv data file, enter: Help(\"create.data.file\")

The name of the entire rectangular matrix of data, called a data frame in R, is 
specifically named \"mydata\" within R when created by the function Read. Make 
sure to distinguish between the name of the data frame, mydata, and the names 
of the individual variables, columns, contained within the data frame."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,56, label=t1, adj=0)

help.more("Read", 26)
}


else if (topic == "Write") {
t0 <- "Write Contents of Data Frame mydata into a Data File"

f1 <- bquote(paste(bold("Write, wrt"), "  Write a data file into an R data frame called mydata, and more."))

t1 <-
"The name of the entire rectangular matrix of data, called a data frame in R, can 
be named \"mydata\" within R.  This is also the name of the data frame given 
by the lessR function Read that reads the data.

Here is how to write the contents of mydata to a csv data file with the name of 
mydata.csv.
    > Write()

Or, explicitly specify the file name.
    > Write(\"mybestdata\")
The file type of .csv is automatically appended to the file name.

To write a data file in native R format, use the type=\"R\" option.

The function write.table is quite general, with many options.  For more 
information, enter ?write.table"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,62, label=t1, adj=0)

help.more("Read", 34)
}


else if (topic == "library") {
t0 <- "Contributed Packages"

f1 <- bquote(paste(bold("install.packages"), "  Download a contributed package."))
f2 <- bquote(paste(bold("library"), "  Load an installed package from the library into R for access."))
f3 <- bquote(paste(bold(update.packages), "  Update contributed packages to current versions."))

t1 <-
"The example here is for the contributed package lessR. Install one time 
only for a specific computer, with quotes.
    > install.packages(\"lessR\")

Each time the R application is started, including after the install, load the 
package from the library, without using quotes.
    > library(lessR)

To see the description of the package and a list of its functions,
    > library(help=lessR)

To access new versions of all installed packages, 
    > update.packages()

All of R works with functions contained in specific packages. The distinction is 
that some of those packages are included with the default installation of R, and 
are pre-loaded each time the application is run. Examples are the stat package 
and the graphic package. To see a list of all installed packages in the library, 
    > library()"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(80,80), col=col.line)
text(0,46, label=t1, adj=0)

help.more("install.packages", 12)
}


else if (topic == "Histogram") {
t0 <- "Histogram, etc."

f1 <- bquote(paste(bold("Histogram, hst"), "  Histogram."))
f2 <- bquote(paste(bold("Density, dens"), "  Density curve over histogram."))
f3 <- bquote(paste(bold("BoxPlot, bx"), "  Box plot."))
f4 <- bquote(paste(bold("Plot, plt"), "  Dot plot."))

t1 <-
"Replace Y in these examples with the actual variable name.

An enhanced histogram, including the default color scheme of \"blue\".
    > Histogram(Y)

Specify a title, labels for the x axis, and the gray scale color scheme.
    > Histogram(Y, main=\"My Title\", xlab=\"Y (mm)\", colors=\"gray\")

Specify bins, starting at 60 with a bin width of 10.
    > Histogram(Y, bin.start=60, bin.width=10)

Density curve superimposed on the underlying histogram.
    > Density(Y)

Box plot.
    > BoxPlot(Y)

Dot plot, a scatterplot for one variable.
    > Plot(Y)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
lines(c(5,90), c(78,78), col=col.line)
text(0,45, label=t1, adj=0)

help.more("Histogram", 12)
}


else if (topic == "BarChart") {
t0 <- "BarChart, etc."

f1 <- bquote(paste(bold("BarChart, bc"), "  Count the values of one or more categorical variables."))
f2 <- bquote(paste(bold("PieChart, pc"), "  Count the values of one or more categorical variables."))
f3 <- bquote(paste(bold("pareto.chart"), "  Produce a Pareto chart."))

t1 <-
"The generic variable in the examples below is generally a categorical variable Y, 
called a factor. Replace with the actual name of the variable in a specific analysis. 

Default bar chart, as well as the frequency table, for one or two variables.
    > BarChart(Y)
    > BarChart(Y, by=X)
    
Or, a pie chart and the frequencies.
    > PieChart(Y)
    
The pareto.chart function is part of the external library called gcc. To view an 
explanation of dealing with libraries, enter Help(\"libraries\"). Default input 
Pareto chart follows, which works from the counts. 
    > library(gcc)
    > pareto.chart(Ycount)
"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(81,81), col=col.line)
text(0,50, label=t1, adj=0)

help.more("BarChart", 22)
}


else if (topic == "Plot") {
t0 <- "Scatterplot"

f1 <- bquote(paste(bold("Plot, plt"), "  A dot plot for one variable and",
                                      " a scatterplot for two variables."))

t1 <-
"Plot can produce a wide range of plots, with access to color enhancement. 
Choices include dot plots, scatter plots and line plots. 

This example is the default scatterplot, in color, for variables named X and Y.
    > Plot(X,Y)
If the values of X are sorted, a function plot is generated instead so that the
points are not individually displayed and are connected by line segments.

Here a run chart is generated, in color, for a variable named Y. If the data do not 
have a pronounced trend, an added centerline is automatically provided.
    > Plot(Y)

These graphic functions can access a wide range of graphics parameters, such 
as the size of the margins, the annotations, the line width, etc. These additional 
options are explained in the help files for functions par, title, points and lines. 

Also, color themes are available with the colors option, which can be invoked
from a specific call to Plot or system wide for all graphics output with the 
function set. In this example, all subsequent graphics output is in gray scale.
    > set(colors=\"gray\")
    > Plot(X, Y)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(90,90), col=col.line)
text(0,53, label=t1, adj=0)

help.more("Plot", 16)
}


else if (topic == "RunChart") {
t0 <- "Run Chart"

f1 <- bquote(paste(bold("RunChart, rc"), "  A run chart or time series chart."))

t1 <-
"Here a run chart is generated, in color, for a variable named Y. If the data do not 
have a pronounced trend, an added centerline is automatically provided.
    > RunChart(Y)

These graphic functions can access a wide range of graphics parameters, such 
as the size of the margins, the annotations, the line width, etc. These additional 
options are explained in the help files for functions par, title, points and lines. 

Also, color themes are available with the colors option, which can be invoked
from a specific call to Plot or system wide for all graphics output with the 
function set. In this example, all subsequent graphics output is in gray scale.
    > set(colors=\"gray\")
    > RunChart(X, Y)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(90,90), col=col.line)
text(0,64, label=t1, adj=0)

help.more("Plot", 38)
}


else if (topic == "SummaryStats") {
t0 <- "Summary Statistics"

f1 <- bquote(paste(bold("SummaryStats, ss"), "  summarize the values of a variable"))
f2 <- bquote(paste(bold("scale"), "  standardize"))

t1 <-
"Summarize the variable Y.  If numerical, mean, sd, median, etc. If categorical,
cell counts and proportions.
    > SummaryStats(Y)

Or summarize all numerical and non-numerical variables in the data frame mydata.
    > SummaryStats()
    
Or, can apply the describe function to a single variable, Y, with an optional grouping
variable, X, to summarize the numerical variable at each level of the other variable.
    > SummaryStats(Y, by=X)

The following generates the standard scores for variable Y.
    > z <- scale(Y)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,58, label=t1, adj=0)
help.more("SummaryStats", 30)
}


else if (topic == "one.sample") {
t0 <- "Inference for a Single Variable"

f1 <- bquote(paste(bold("ttest, tt"), "  Inference for a mean."))
f2 <- bquote(paste(bold("binom.test"), "  Inference for a proportion from exact binomial probability."))
f3 <- bquote(paste(bold("prop.test"), "  Inference for a proportion from approximate normal probability."))

t1 <-
"These inference tests analyze the mean of a numeric variable or the proportion 
of a value of a categorical variable. These tests provide a hypothesis test 
and a confidence interval.

This example is for a variable named Y and a null hypothesis of mu=100.
    > ttest(Y, mu0=100)
    
These examples are for testing for a fair coin after getting 53 out of 100 Heads.
    > binom.test(53,100, p=.5)
    > prop.test(53,100, p=.5)

The prop.test function can be specified with or without the Yate's correction for 
continuity factor. The default is to include the correction."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(82,82), col=col.line)
text(0,58, label=t1, adj=0)

help.more("ttest", 33)
}


else if (topic == "ttest") {
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
    > model(Y ~ X)

To do a separate analysis of Y for each group, the variables group1 and
group2 are automatically created when running ttest.
    > Histogram(group1)

Sometimes the data for a t-test are arranged so that the responses for 
each group, Y, already are in separate columns called vectors. Here calculate 
the t-test directly from two vectors called Y1 and Y2.
    > ttest(Y1, Y2)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(86,86), col=col.line)
text(0,50, label=t1, adj=0)

help.more("ttest", 13)
}

else if (topic == "ANOVA") {
t0 <- "Compare Means of Two or More Groups"

f1 <- bquote(paste(bold("ANOVA, av"), "  Analysis of variance to compare two or more group means."))
f2 <- bquote(paste(bold("Model, model"), "  ANOVA if explanatory variables are categorical."))
t1 <-
"When responses to a variable are organized into exactly two groups, either the 
t-test or analysis of variance, ANOVA, can compare the group means. With more 
than two groups, ANOVA is required. The function ANOVA works only in formula mode.
Here the numerical response variable is named Y and the grouping variable, or 
factor, is X, which may have more than two discrete values.
    > ANOVA(Y ~ X)
or
    > Model(Y ~ X)
This is called one-way ANOVA because there is only a single factor, X.

If the ANOVA with more than two levels is significant, then a post-hoc examination 
of the mean differences with a controlled error rate will help uncover where the 
differences occurred. The ANOVA function relies upon Tukey's HSD procedure.  
Both tabular and plotted output are obtained.
"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,55, label=t1, adj=0)

help.more("aov", 25)
}



else if (topic == "power") {
t0 <- "Power"

f1 <- bquote(paste(bold("ttestPower, ttp"), "  Power analysis of the t-test."))

t1 <-
"The function, ttestPower, uses the standard R function, ttestPower, to 
calculate a range of power values and automatically provide a power curve. 

To accomplish this analysis otherwise requires setting up the range of alternative 
mean or mean difference values, usually by trial and error, invoking ttestPower, 
saving the results, and then invoking the plot function, including the labeling 
of each axis. Then to analyze related results such as power at a different
sample size, the ttestPower function must be run several more times. 

The enhanced function, ttestPower, does all of this automatically for one 
or two sample t-tests, and also plots the power curve in color. This example is 
for the default power curve plotted in color for a sample size of 20 in each group 
and a within-group or pooled standard deviation of 5.
    > ttestPower(n=20, s=5)
Related analysis is also provided."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,62, label=t1, adj=0)

help.more("ttp", 34)
}


else if (topic == "Correlation") {
t0 <- "Correlation and Related Graphics"

f1 <- bquote(paste(bold("Correlation, cr"), "  Correlations between two or more variables."))
f3 <- bquote(paste(bold("Plot"), "  Graphics, generate a scatterplot for two or more variables."))

t1 <-
"Correlation can compute correlations for a single pair of variables, 
or for all numeric variables in the data frame, such as Correlation(mydata) 
for a data frame named mydata. 

The graphic function, Plot, displays a scatterplot for two variables 
or a scatterplot matrix for a data frame.

This example is for the correlation coefficient, inference and scatterplot for two 
numerical variables, X and Y.
    > Correlation(X,Y)
    > Plot(X,Y)
Plot also provides the brief form of the correlation analysis for two variables.

Or, analyze many correlations at once, such as for Y, X1, X2 and X3 in 
the data frame called mydata.
    > mynew <- subset(mydata, select=c(Y,X1:X3))
    > Correlation(mynew)
"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f3, adj=0)
lines(c(5,90), c(83,83), col=col.line)
text(0,50, label=t1, adj=0)

help.more("Plot", 19)
}


else if (topic == "Regression") {
t0 <- "Linear Models and Regression"

f1 <- bquote(paste(bold("Regression, reg"), "  Regression analysis."))
f2 <- bquote(paste(bold("Model, model"), "  Regression analysis if variables numerical."))

t1 <-
"The function Regression preforms a regression analysis and stores the 
results in an R object called lm.out, which is available for further analysis. 
This example is for a multiple regression with a response variable named Y and 
predictor variables X1 and X2.
    > Regression(Y ~ X1 + X2)
The function uses the standard R specification for the defining formula of the 
model, of which the details are accessed by entering:  ?formula

If all the variables in the model are numerical, then a call to Model will,
in turn, call the Regression function.
    > Model(Y ~ X1 + X2)
The Model function also works for the analysis of other linear models.

The output of Regression is comprehensive, including a predictor subset
analysis, residuals and prediction intervals. If there is only one predictor 
variable, a scatterplot of the data with included regression line and 
prediction and confidence intervals is also provided by default. A scatterplot 
matrix is produced for multiple predictor variables."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(87,87), col=col.line)
text(0,56, label=t1, adj=0)

help.more("Regression", 24)
}


else if (topic == "prob") {
t0 <- "Probabilities for Normal and t-distributions"

f1 <- bquote(paste(bold("prob.norm"), "  Normal distribution probability over a range of values."))
f2 <- bquote(paste(bold("pt"), "  Probability for a t-distribution related to a specified t-value."))
f3 <- bquote(paste(bold("qnorm"), "  Quantile for a normal distribution."))
f4 <- bquote(paste(bold("qnt.t"), "  Quantile for a t-distribution."))


t1 <-
"By default, prob.norm or pt provides the corresponding probability of obtaining
a randomly sampled value, Y or t, in a range of specified values.

Upper tail probability for t=1.627, df=24:  > pt(1.627, df=24, lower.tail=FALSE)
Two-tailed p-value for  t=1.627, df=24:     > 2*pt(1.627, df=24, lower.tail=FALSE)
Prob for a value between 80 and 120 for, mu=100, sigma=15: 
    > prob.norm(lo=80, hi=120, mu=100, sigma=15)

The quantile functions are the inverse of the probability functions. For a given 
probability or area under the curve, the corresponding quantile is the 
corresponding value of the distribution, Y or t.

t-value that cuts off the top 2.5% of the t-distribution for df=24.
    > qnt.t(df=24)
    
Value from the standard normal distribution that cuts off the top 2.5% of the 
distribution.  Without specifying mu and sigma, the respective defaults are 0 and 1.
    > qnorm(0.025, lower.tail=FALSE)"


set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
lines(c(5,90), c(78,78), col=col.line)
text(0,45, label=t1, adj=0)

help.more("prob.norm", 13)
}


else if (topic == "random") {
t0 <- "Normal and Binomial Random Values"

f1 <- bquote(paste(bold("rnorm"), "  Generate randomly sampled values from a normal distribution."))
f2 <- bquote(paste(bold("rbinom"), "  Generate randomly sampled values from a binomial distribution."))

t1 <-
"R can generate simulated sampling from many different distributions, including 
the normal and the binomial.

This example generates 50 randomly sampled values from the standard normal 
distribution, with a default mu of 0 and sigma of 1.
    > rnorm(50)
    
This generated data can be stored for further analysis.  Here, generate 100 
values from a normal distribution with a mean of 50 and a standard deviation 
of 10, store in the vector Y, and then display the resulting histogram.
    > Y <- rnorm(100, mean=50, sd=10)
    > hst(Y)
    
The binomial distribution describes the process of a binary outcome over 
many different trials, such as flipping a coin.  In this example, flip a fair 
coin 20 times with a probability of a Head at 0.5.  Then repeat this set of 20 
flips 10 times to get the number of Heads obtained on each set of 20 flips.
    > rbinom(10, 20, .5)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,51, label=t1, adj=0)

help.more("rnorm", 17)
}


else if (topic == "sample") {
t0 <- "Generate Random Samples"

f1 <- bquote(paste(bold("sample"), "  Generate random samples."))

t1 <-
"To use the sample function, first specify the population from which to randomly 
sample. The population can be defined from the values of a specified variable, or 
the values can be directly listed. Next use the size option to specify the number 
of elements to sample. By default, sampling is done without replacement, each 
value in the population can only appear once in the resulting sample. To allow 
sampling with replacement, invoke the replace=TRUE option.

The following randomly samples 5 values of the variable Y without replacement.
    > sample(Y, size=5)
    
If the size of the resulting list of sample values is larger than the available 
number of values from which to sample, then sampling must be done with 
replacement.
    > sample(c(\"Group1\",\"Group2\"), size=10, replace=TRUE)
    
Here, 10 numbers are randomly sampled from the first 100 integers, without 
replacement.
    > sample(1:100, 10)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(88,88), col=col.line)
text(0,57, label=t1, adj=0)

help.more("sample", 25)
}


else if (topic == "help.to.pdf") {
pdf("R_help.pdf")

t1 <- "Contents of the Help Files for R Function Help()"
t2 <- "from the R Contributed Package:"
t3 <- "lessR"
t4 <- "David W. Gerbing"
t5 <- "School of Business Administration"
t6 <- "Portland State University"
t7 <- "Version 2.3, June 1, 2012"
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
Help("Histogram")
Help("BarChart")
Help("Plot")
Help("RunChart")
Help("SummaryStats")
Help("one.sample")
Help("ttest")
Help("ANOVA")
Help("power")
Help("Correlation")
Help("Regression")
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



else if (topic == "lessR") {

  help(package="lessR")

}


else {
cat("
Value ", topic," for Help not recognized.\n
Complete list of Help topics, enter:  Help()\n
PDF file of all Help topics, enter:  Help(\"help.to.pdf\")
\n")

}

}

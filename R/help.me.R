help.me <- 
function(topic=NULL) {


help.more <-
function(fname, yline) {
h1 <- "Complete list of help.me topics, enter:  help.me()   or   help.me(\"help.to.pdf\")"
h2 <- paste("For more help on a function, enter ? in front of its name:  ?", fname, sep="")
lines(c(5,90), c(yline,yline), col="lightsteelblue")
text(0,yline-5, label=h1, adj=0)
text(0,yline-10, label=h2, adj=0)
}


# set up plot window
set.up.plot <- 
function() {
par(mar=c(.5,1,.5,.5), bg=rgb(255,253,250,max=255), fg=rgb(20,15,15,max=255))
plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
}


col.line <- "lightsteelblue"

if (is.null(topic)) {
if (sys.nframe() == 1)  # not nested in a call from help.to.pdf
  cat("To obtain a printable pdf of all of the contents, enter:  help.me(\"help.to.pdf\")\n")

t0 <- "Topics for help.me"

fcsv <- bquote(paste(bold("help.me(\"data\")"), "  Create csv data file from Excel or other worksheet apps."))
fread <- bquote(paste(bold("help.me(\"read\")"), "  Read an external data file in csv format."))
fwrite <- bquote(paste(bold("help.me(\"write\")"), "  Write data to an external data file in csv format."))
flib <- bquote(paste(bold("help.me(\"library\")"), "  Access libraries of functions called packages."))

fprob <- bquote(paste(bold("help.me(\"prob\")"), "  Probabilities for normal and t-distributions."))
frand <- bquote(paste(bold("help.me(\"random\")"), "  Generate random numbers."))
fsamp <- bquote(paste(bold("help.me(\"sample\")"), "  Generate random samples."))

fhist <- bquote(paste(bold("help.me(\"histogram\")"), "  Histogram of a numeric variable."))
fbar <- bquote(paste(bold("help.me(\"bar.chart\")"), "  Bar chart of a categorical variable."))
fplot <- bquote(paste(bold("help.me(\"plot\")"), "  Run chart, scatterplot, graph of a function."))

fstat <- bquote(paste(bold("help.me(\"stats\")"), "  Summary statistics."))
fone <- bquote(paste(bold("help.me(\"one.sample\")"), "  Analysis of a single sample of data."))
fmean <- bquote(paste(bold("help.me(\"two.samples\")"), "  Compare groups by their mean difference."))
faov <- bquote(paste(bold("help.me(\"many.samples\")"), "  Compare mean differences for many groups."))
fprop <- bquote(paste(bold("help.me(\"props\")"), "  Compare proportions across two or more groups."))
fpwr <- bquote(paste(bold("help.me(\"power\")"), "  Power analysis for the t-test."))
fcor <- bquote(paste(bold("help.me(\"cor\")"), "  Correlation analysis."))
freg <- bquote(paste(bold("help.me(\"reg\")"), "  Regression analysis."))

set.up.plot()
pos1 <- 93; pos2 <- 73; pos3 <- 58; pos4 <- 42
text(50,100, label=t0, font=4)
text(0,pos1, label=fcsv, adj=0)
text(0,pos1-4, label=fread, adj=0)
text(0,pos1-8, label=fwrite, adj=0)
text(0,pos1-12, label=flib, adj=0)
lines(c(5,90), c(77,77), col=col.line)
text(0,pos2, label=fprob, adj=0)
text(0,pos2-4, label=frand, adj=0)
text(0,pos2-8, label=fsamp, adj=0)
lines(c(5,90), c(62,62), col=col.line)
text(0,pos3, label=fhist, adj=0)
text(0,pos3-4, label=fbar, adj=0)
text(0,pos3-8, label=fplot, adj=0)
lines(c(5,90), c(46,46), col=col.line)
text(0,pos4, label=fstat, adj=0)
text(0,pos4-4, label=fone, adj=0)
text(0,pos4-8, label=fmean, adj=0)
text(0,pos4-12, label=faov, adj=0)
text(0,pos4-16, label=fprop, adj=0)
text(0,pos4-20, label=fpwr, adj=0)
text(0,pos4-24, label=fcor, adj=0)
text(0,pos4-28, label=freg, adj=0)
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

Next, read the csv data file into R, [see help.me(\"read\")]. However, using a 
worksheet such as Excel and R are complementary procedures.  R can do 
extensive data transformations, such as sorting and much else, but so can Excel, 
and often more directly, without the need for programming.  Given the simplicity 
of transferring data from Excel to R, it is often useful to move back and forth 
between the two systems on a regular basis."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,59, label=t1, adj=0)

help.more("rad", 20)
}


else if (topic == "read") {
t0 <- "Read Data into R and Prepare for Analysis"

f1 <- bquote(paste(bold("rad"), "  Read a csv data file into an R data frame called mydata, and more."))

t1 <-
"Browse for a csv data file available on the local computer system.
    > rad()

Or, specify the file to be read. The file can be a path name to a data file 
available on the local computer system, or to a file on the web.
    > rad(\"http://web.pdx.edu/~gerbing/data/twogroup.csv\")

The function, rad, which stands for Read, Attach and Display, sequentially 
invokes four different, standard R functions: read.csv, attach, head and tail.

To see how to create a csv data file, enter: help.me(\"create.data.file\")

The name of the entire rectangular matrix of data, called a data frame in R, is 
specifically named \"mydata\" within R when created by the function rad. Make 
sure to distinguish between the name of the data frame, mydata, and the names 
of the individual variables, columns, contained in the data frame."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,60, label=t1, adj=0)

help.more("rad", 30)
}


else if (topic == "write") {
t0 <- "Write Contents of Data Frame mydata into a csv Text File"

f1 <- bquote(paste(bold("out"), "  Write a csv data file into an R data frame called mydata, and more."))
f2 <- bquote(paste(bold("write.table"), "  General R statement to write the contents of an R object to a file."))

t1 <-
"The name of the entire rectangular matrix of data, called a data frame in R, can 
be named \"mydata\" within R.  This is also the name of the data frame given 
by the complementary function rad that reads the data.

Here is how to write the contents of mydata to a csv data file with the name of 
mydata.csv.
    > out()

Or, explicitly specify the file name.
    > out(\"mybestdata.csv\")

The function write.table is quite general, with many options.  For more information, 
enter ?write.table"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,62, label=t1, adj=0)

help.more("rad", 38)
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


else if (topic == "prob") {
t0 <- "Probabilities for Normal and t-distributions"

f1 <- bquote(paste(bold("pnorm"), "  Probability for a normal distribution related to a specified value, Y."))
f2 <- bquote(paste(bold("pt"), "  Probability for a t-distribution related to a specified t-value."))
f3 <- bquote(paste(bold("qnorm"), "  Quantile for a normal distribution."))
f4 <- bquote(paste(bold("qt"), "  Quantile for a t-distribution."))


t1 <-
"By default, pnorm or pt provides the corresponding probability of obtaining a 
randomly sampled value, Y or t, in the lower tail of the specified distribution: 
the probability of a value smaller than or equal to the specified value.  This is 
usually the desirable result for a negative value of Y or t.  For a positive value, 
obtain the corresponding upper-tail value by adding the option: lower.tail=FALSE.

Upper tail probability for t=1.627, df=24:  > pt(1.627, df=24, lower.tail=FALSE)
Two-tailed p-value for  t=1.627, df=24:     > 2*pt(1.627, df=24, lower.tail=FALSE)
Lower tail prob for Y=94, mu=100, sigma=15: > pnorm(94, mean=100, sd=15)

The quantile functions are the inverse of the probability functions. For a given 
probability or area under the curve, the corresponding quantile is the 
corresponding value of the distribution, Y or t.

t-value that cuts off the top 2.5% of the t-distribution for df=24.
    > qt(0.025, df=24, lower.tail=FALSE)
    
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
text(0,44, label=t1, adj=0)

help.more("pt", 9)
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
    > hist(Y)
    
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


else if (topic == "histogram") {
t0 <- "Histogram"

f1 <- bquote(paste(bold("hist"), "  Histogram function."))
f2 <- bquote(paste(bold("color.hist"), "  An enhanced version of hist."))

t1 <-
"The generic variable in the examples below is Y. Replace with the actual name of 
the variable in a specific analysis.

An enhanced histogram, including color by default, is produced from
    > color.hist(Y)

In this histogram, specify a title, labels for the x and y axes, and a color.
    > color.hist(Y, main=\"My Title\", xlab=\"Y (mm)\", ylab=\"Counts\", col=\"seagreen3\")

Here, manually specify bins, starting at 60, going to 100, with bin width of 10.
    > color.hist(Y, breaks=seq(60,100,10))"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(86,86), col=col.line)
text(0,66, label=t1, adj=0)

help.more("hist", 46)
}


else if (topic == "bar.chart") {
t0 <- "bar.chart"

f1 <- bquote(paste(bold("table"), "  Count the values of one or more categorical variables."))
f2 <- bquote(paste(bold("barplot"), "  Produce a bar chart."))
f3 <- bquote(paste(bold("pareto.chart"), "  Produce a Pareto chart."))

t1 <-
"The generic variable in the examples below is a categorical variable Y, called a 
factor. Replace with the actual name of the variable in a specific analysis. 

The key is to first use the table function to provide the counts of each value or 
level of Y. Then construct the bar chart or Pareto chart from the table.

First use table function to get counts.
    > Ycount <- table(Y) 

Default bar chart plus a color.
    > barplot(Ycount, count=\"plum\")

The pareto.chart function is part of the external library called gcc. To view an 
explanation of dealing with libraries, enter help.me(\"libraries\"). Default input 
Pareto chart follows.
    > library(gcc)
    > pareto.chart(Ycount)
"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(81,81), col=col.line)
text(0,49, label=t1, adj=0)

help.more("barplot", 19)
}


else if (topic == "plot") {
t0 <- "Plot"

f1 <- bquote(paste(bold("plot"), "  Plot values of one or two variables."))
f2 <- bquote(paste(bold("color.plot"), "  Enhances some of the capabilities of the plot function."))
f3 <- bquote(paste(bold("color.density"), "  Plot normal and general densities over the histogram."))

t1 <-
"The function, plot, can produce a wide range of plots. The function, color.plot, 
provides easier access to color enhancement. Either function plots run charts, 
scatter plots and the values of a function. The function, color.density, estimates the 
smooth normal curve or general density function from the data, and then displays 
over the histogram.

This example is the default scatterplot, in color, for variables named X and Y.
    > color.plot(X,Y)

Here a run chart is generated, in color, for a variable named Y. If the data do not 
have a pronounced trend, an added centerline is automatically provided.
    > color.plot(Y)

These graphic functions can access a wide range of graphics parameters, such 
as the size of the margins, the annotations, the line width, etc. These additional 
options are explained in the help files for functions par, title, points and lines. 

This scatter plot has dark red points and the #19 point character, pch, which 
is a filled circle.
    > color.plot(X, Y, col.point=\"darkred\", pch=19)
The help for the function, points, shows the different options for pch."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,88), c(81,81), col=col.line)
text(0,45, label=t1, adj=0)

help.more("color.plot", 9)
}


else if (topic == "stats") {
t0 <- "Descriptive Statistics"

f1 <- bquote(paste(bold("length"), "  sample size, i.e., count"))
f2 <- bquote(paste(bold("mean"), "  mean, including trimmed mean with trim option"))
f3 <- bquote(paste(bold("sd"), "  standard deviation"))
f4 <- bquote(paste(bold("median"), "  median"))
f5 <- bquote(paste(bold("min"), "  minimum"))
f6 <- bquote(paste(bold("max"), "  maximum"))
f7 <- bquote(paste(bold("range"), "  range"))
f8 <- bquote(paste(bold("quantile"), "  min, 1st quartile, median, 3rd quartile, max"))
f9 <- bquote(paste(bold("scale"), "  standardize"))

t1 <-
"Each of these functions applies to the analysis of single variable. 

For example, calculate the mean of variable called Y.
    > mean(Y)

R provides many summary statistics. Enter the following to see the entire list, 
    > library(help=\"stats\")."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
text(0,78, label=f5, adj=0)
text(0,74, label=f6, adj=0)
text(0,70, label=f7, adj=0)
text(0,66, label=f8, adj=0)
text(0,62, label=f9, adj=0)
lines(c(5,90), c(58,58), col=col.line)
text(0,44, label=t1, adj=0)

help.more("mean", 30)
}


else if (topic == "one.sample") {
t0 <- "Inference for a Single Variable"

f1 <- bquote(paste(bold("t.test"), "  Inference for a mean."))
f2 <- bquote(paste(bold("binom.test"), "  Inference for a proportion from exact binomial probability."))
f3 <- bquote(paste(bold("prop.test"), "  Inference for a proportion from approximate normal probability."))

t1 <-
"These inference tests analyze the mean of a numeric variable or the proportion 
of a value of a categorical variable. These tests provide a hypothesis test 
and a confidence interval.

This example is for a variable named Y and a null hypothesis of mu=100.
    > t.test(Y, mu=100)
    
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

help.more("t.test", 33)
}


else if (topic == "two.samples") {
t0 <- "Compare Two Group Means"

f1 <- bquote(paste(bold("t.test"), "  The standard R function to compare two group means with a t-test."))
f2 <- bquote(paste(bold("smd.t.test"), "  An enhanced version of t.test to compare two group means."))

t1 <-
"When responses to a variable are organized into two or more groups, compare
the group means with a t-test.  For example, suppose the response variable is 
Salary and the grouping variable is Gender, with two values, M and F.

Here the numerical response variable is named Y and the grouping variable, 
also called a factor, is named X, which must have exactly two values.
    >  smd.t.test(Y ~ X)
When the tilde, ~, expresses the relationship between two or more variables, 
R refers to this expression as a formula, read as: Y is described by X.

To do a separate analysis of Y for each group, use the [...] notation to define a 
vector that contains just the Y responses for one group.  In this example, one 
of the two values of X is Group1.
    > Y1 <- Y[X==\"Group1\"]
    > hist(Y1)
Create a new variable for each group.

Sometimes the data for a t-test are arranged so that the responses, Y, for 
each group already are in separate columns called vectors. Here calculate 
the t-test directly from two vectors called Y1 and Y2.
    > smd.t.test(Y1, Y2)"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(86,86), col=col.line)
text(0,50, label=t1, adj=0)

help.more("smd.t.test", 13)
}


else if (topic == "many.samples") {
t0 <- "Compare Means of Two or More Groups"

f1 <- bquote(paste(bold("aov"), "  Analysis of variance to compare two or more group means."))

f2 <- bquote(paste(bold("TukeyHSD"), "  Tukey Honest Significant Differences post-hoc comparison of means."))

t1 <-
"When responses to a variable are organized into exactly two groups, either the 
t-test or analysis of variance, ANOVA, can compare the group means. With more 
than two groups, ANOVA is required. The function aov works only in formula mode.
Here the numerical response variable is named Y and the grouping variable, or 
factor, is X, which may have more than two discrete values.
    > aov(Y ~ X)
This is called one-way ANOVA because there is only a single factor, X.

If the ANOVA with more than two levels is significant, then a post-hoc examination 
of the mean differences with a controlled error rate will help uncover where the 
differences occurred. The function used here is based on Tukey's HSD procedure.  
Both tabular and plotted output are obtained.
    > a <- aov(Y ~ X)
    > aTukey <- TukeyHSD(a, \"X\")
    > aTukey
    > plot(aTukey)
    > abline(v=0, lty=\"dotted\")"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,55, label=t1, adj=0)

help.more("aov", 25)
}


else if (topic == "props") {
t0 <- "Analysis of Cross-Tabulation or Pivot Tables"

f1 <- bquote(paste(bold("table"), "  Construct the cross-tabulation table from joint frequencies."))
f2 <- bquote(paste(bold("addmargins"), "  Add row and column margins to the cross-tabulation table."))
f3 <- bquote(paste(bold("chisq.test"),  "  ", chi^2, " (chi-square) test from the null hypothesis of no relation."))

t1 <-
"Calculate the cross-tabulation table from the categorical variables, or 
factors, with the table function, applied here to factors X and Y.
    > table(X,Y)
    
To store the counts for later analysis, assign the output of the table 
function to an object, here called mytable. Use the addmargins function 
to display the frequencies, as well as the marginal frequencies for the 
rows and columns, of the cross-tabulation table.
    > mytable <- table(X,Y)
    > addmargins(mytable)

Obtain the chi-square statistic and associated p-value calculated with the 
assumption of no relation between the variables, that is, equal proportions.
    > chisq.test(mytable)
"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
lines(c(5,90), c(81,81), col=col.line)
text(0,55, label=t1, adj=0)

help.more("chisq.test", 31)
}


else if (topic == "power") {
t0 <- "Power"

f1 <- bquote(paste(bold("power.t.test"), "  The standard R function for the power analysis of the t-test."))
f2 <- bquote(paste(bold("smd.t.test"), "  Enhanced power function, also provides power curve."))

t1 <-
"The function, powercurve.t.test, uses the standard R function, power.t.test, to 
calculate a range of power values and automatically provide a power curve. 

To accomplish this analysis otherwise requires setting up the range of alternative 
mean or mean difference values, usually by trial and error, invoking power.t.test, 
saving the results, and then invoking the plot function, including the labeling 
of each axis. Then to analyze related results such as power at a different
sample size, the power.t.test function must be run several more times. 

The enhanced function, powercurve.t.test, does all of this automatically for one 
or two sample t-tests, and also plots the power curve in color. This example is 
for the default power curve plotted in color for a sample size of 20 in each group 
and a within-group or pooled standard deviation of 5.
    > powercurve.t.test(n=20, s=5)
Related analysis is also provided."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
lines(c(5,90), c(85,85), col=col.line)
text(0,58, label=t1, adj=0)

help.more("powercurve.t.test", 30)
}


else if (topic == "cor") {
t0 <- "Correlation and Related Graphics"

f1 <- bquote(paste(bold("cor"), "  Correlation coefficient(s) between two or more variables."))
f2 <- bquote(paste(bold("cor.test"), "  Correlation coefficient with statistical inference."))
f3 <- bquote(paste(bold("plot"), "  Graphics, generate a scatterplot for two variables."))
f4 <- bquote(paste(bold("color.plot"), "  Graphics, enhances some of the capabilities of the plot function."))
f5 <- bquote(paste(bold("pairs"), "  Generate a matrix of all possible scatter plots of many variables."))

t1 <-
"The function, cor, can compute correlations for a single pair of variables, such as 
cor(X,Y), or for all numeric variables in the data frame, such as cor(mydata), 
for a data frame named mydata. The cor.test function applies only to a single pair 
of variables, and provides output similar to the t.test and related functions.

The graphic functions, color.plot and plot, display a scatterplot for two variables. 
The graphic function, pairs, generates a scatterplot matrix for all numeric 
variables in an entire data frame, or a subset of variables from the data frame.

This example is for the correlation coefficient, inference and scatterplot for two 
numerical variables, X and Y.
    > cor.test(X,Y)
    > color.plot(X,Y)

This example of functions cor and pairs applies to Variables Y, X1, X2 and X3 in 
the data frame called mydata.
    > cor(subset(mydata, select=c(Y,X1:X3)))
    > pairs(subset(mydata, select=c(Y,X1:X3)))"

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
text(0,90, label=f2, adj=0)
text(0,86, label=f3, adj=0)
text(0,82, label=f4, adj=0)
text(0,78, label=f5, adj=0)
lines(c(5,90), c(73,73), col=col.line)
text(0,41, label=t1, adj=0)

help.more("color.plot", 9)
}


else if (topic == "reg") {
t0 <- "Linear Models and Regression"

f1 <- bquote(paste(bold("reg"), "  Regression analysis."))

t1 <-
"The function reg preforms a regression analysis and stores the results in an R 
object called model, which is available for further analysis. This example is for 
a multiple regression with a response variable named Y and predictor variables X1 
and X2.
    > reg(Y ~ X1 + X2)
The function uses the standard R specification for the model's defining formula, 
of which the details are accessed by entering:  ?formula

The function reg consolidates the following three standard R function calls into 
a single statement, as applied here to the previous example.
    > model <- lm(Y ~ X1 + X2)
    > summary(model)
    > confint(model)
    > anova(model)

The output of reg also includes output for the following functions: resid, fitted, 
rstudent, cooks.distance and predict. If there is only one predictor variable, a 
scatterplot of the data with included regression line and prediction and confidence 
intervals is also provided by default."

set.up.plot()
text(50,100, label=t0, font=4)
text(0,94, label=f1, adj=0)
lines(c(5,90), c(89,89), col=col.line)
text(0,57, label=t1, adj=0)

help.more("reg", 24)
}


else if (topic == "help.to.pdf") {
pdf("R_help.pdf")

t1 <- "Contents of the Help Files for R Function help.me()"
t2 <- "from the R Contributed Package:"
t3 <- "lessR"
t4 <- "David W. Gerbing"
t5 <- "School of Business Administration"
t6 <- "Portland State University"
t7 <- "Version 1.4"
set.up.plot()
text(50,84, label=t1)
text(50,80, label=t2)
text(50,76, label=t3)
text(50,58, label=t4)
text(50,54, label=t5)
text(50,50, label=t6)
text(50,24, label=t7)

help.me()
help.me("data")
help.me("read")
help.me("write")
help.me("library")
help.me("prob")
help.me("random")
help.me("sample")
help.me("histogram")
help.me("bar.chart")
help.me("plot")
help.me("stats")
help.me("one.sample")
help.me("two.samples")
help.me("many.samples")
help.me("props")
help.me("power")
help.me("cor")
help.me("reg")
dev.off()

if (getwd() =="/")
  workdir <- "top level of your file system"
else
  workdir <- getwd()
cat("PDF file of help.me contents located at current working directory.\n")
cat("   R_help.pdf at: ", workdir, "\n")

}


else {
cat("
Value ", topic," for help.me not recognized.\n
Complete list of help.me topics, enter:  help.me()\n
PDF file of all help.me topics, enter:  help.me(\"help.to.pdf\")
\n")

}

}

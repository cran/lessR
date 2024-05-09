## ----include=FALSE------------------------------------------------------------
suppressPackageStartupMessages(library("lessR"))

## ----read---------------------------------------------------------------------
d <- Read("Employee")

## ----labels-------------------------------------------------------------------
l <- rd("Employee_lbl")
l

## ----bc1, dataTable, echo=FALSE, out.width='28%', fig.asp=.7, fig.align='center', out.extra='style="border-style: none"'----
knitr::include_graphics(system.file("img", "bcExplain.png", package="lessR"))

## ----bcEx, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Bar chart of tablulated counts of employees in each department."----
BarChart(Dept)

## ---------------------------------------------------------------------------------------------------------------------
style(quiet=TRUE)

## ----fig.width=4, fig.height=3.75, fig.align='center'-----------------------------------------------------------------
BarChart(Dept, fill="darkred", color="black", transparency=.8,
         labels_color="black")

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, theme="gray", labels="off", horiz=TRUE)

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, fill="reds")

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, fill="viridis")

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, fill="GrandBudapest1")

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, rotate_x=45, offset=1, sort="-")

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, fill=(count))

## ----a----------------------------------------------------------------------------------------------------------------
a <- pivot(d, mean, Salary, Dept)
a

## ----bcXY, dataTable, echo=FALSE, out.width='35%', fig.asp=.7, fig.align='center', out.extra='style="border-style: none"'----
knitr::include_graphics(system.file("img", "bcXYExplain.png", package="lessR"))

## ----xy, fig.width=4, fig.height=3.5, fig.align='center'--------------------------------------------------------------
BarChart(Dept, Salary_mean, data=a)

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
BarChart(Dept, Salary, stat="dev", sort="+", fill_split=0)

## ----fig.width=4, fig.height=3.5, fig.align='center'------------------------------------------------------------------
style(add_fill="aliceblue")
BarChart(Dept, add=c("rect", "Employees by\nDepartment"),
                     x1=c(1.75,3), y1=c(11, 10), x2=4.25, y2=9)

## ----echo=FALSE, include=FALSE----------------------------------------------------------------------------------------
d <- Read("Employee")

## ----pc1, fig.align='center', fig.width=3.5, fig.height=3.5-----------------------------------------------------------
PieChart(Dept)

## ----hole0, fig.width=3.5, fig.height=3.5, fig.align='center', fig.cap="Standard pie chart of variable Dept in the _d_ data frame."----
PieChart(Dept, hole=0, quiet=TRUE)

## ----bc2var, dataTable, echo=FALSE, out.width='34%', fig.asp=.7, fig.align='center', out.extra='style="border-style: none"'----
knitr::include_graphics(system.file("img", "bc2Explain.png", package="lessR"))

## ----fig.width=4, fig.align='center'----------------------------------------------------------------------------------
BarChart(Dept, by=Gender)

## ----fig.width=4, fig.align='center'----------------------------------------------------------------------------------
BarChart(Dept, by=Gender, fill=c("deepskyblue", "black"))

## ----fig.width=5, fig.align='center'----------------------------------------------------------------------------------
BarChart(Dept, by=Gender, beside=TRUE, labels_position="out")

## ----fig.width=6------------------------------------------------------------------------------------------------------
BarChart(Gender, by=Dept, horiz=TRUE)

## ----fig.width=4, fig.align='center'----------------------------------------------------------------------------------
BarChart(Dept, by1=Gender)

## ----fig.align='center'-----------------------------------------------------------------------------------------------
BarChart(Dept, by1=Gender, n_col=1)

## ----fig.width=4, fig.align='center'----------------------------------------------------------------------------------
BarChart(Dept, by=Gender, stack100=TRUE)

## ---------------------------------------------------------------------------------------------------------------------
d <- rd("Mach4", quiet=TRUE)

## ---------------------------------------------------------------------------------------------------------------------
l <- rd("Mach4_lbl", quiet=TRUE)

## ---------------------------------------------------------------------------------------------------------------------
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree")
d <- factors(c(m06,m07,m09,m10), levels=0:5, labels=LikertCats, ordered=TRUE)

## ----fig.width=6, fig.height=4.5, fig.align='center'------------------------------------------------------------------
BarChart(m06, by=m07, quiet=FALSE)

## ----mult, fig.width=6, fig.height=5----------------------------------------------------------------------------------
d <- rd("Mach4", quiet=TRUE)
BarChart(m01:m20)


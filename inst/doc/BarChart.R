## -----------------------------------------------------------------------------
library("lessR")

## ----read---------------------------------------------------------------------
d <- Read("Employee")

## ----bcEx, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Bar chart of tablulated counts of employees in each department."----
BarChart(Dept)

## -----------------------------------------------------------------------------
getColors("hues")

## -----------------------------------------------------------------------------
BarChart(Dept, fill="darkred", horiz=TRUE, values="off", quiet=TRUE)

## -----------------------------------------------------------------------------
BarChart(Dept, theme="darkred")

## -----------------------------------------------------------------------------
BarChart(Dept, fill="reds")

## -----------------------------------------------------------------------------
BarChart(Dept, rotate_x=45, offset=1, sort="-")

## -----------------------------------------------------------------------------
BarChart(Dept, fill=(count))

## -----------------------------------------------------------------------------
d <- rd("Mach4", quiet=TRUE)
l <- rd("dataMach4_lbl", format="lessR", quiet=TRUE)
LikertCats <- c("Strongly Disagree", "Disagree", "Slightly Disagree",
                     "Slightly Agree", "Agree", "Strongly Agree")
d <- factors(c(m06,m07,m09,m10), levels=0:5, labels=LikertCats, ordered=TRUE, new=TRUE)

## -----------------------------------------------------------------------------
d <- Read("Employee", quiet=TRUE)
BarChart(Dept, Salary, stat="dev", sort="+", fill_split=0)

## ---------------------------------------------------------------------------------------------------------------------
style(add_fill="aliceblue")
BarChart(Dept, add=c("rect", "Employees by\nDepartment"),
                     x1=c(1.75,3), y1=c(11, 10), x2=4.25, y2=9)

## ---- echo=FALSE, include=FALSE---------------------------------------------------------------------------------------
d <- Read("Employee")

## ----pc1--------------------------------------------------------------------------------------------------------------
PieChart(Dept)

## ----hole0, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Standard pie chart of variable _Dept_ in the _d_ data frame."----
PieChart(Dept, hole=0, quiet=TRUE)

## ---- fig.width=4-----------------------------------------------------------------------------------------------------
BarChart(Dept, by=Gender)

## ---- fig.width=5-----------------------------------------------------------------------------------------------------
BarChart(Dept, by=Gender, beside=TRUE)

## ---- fig.width=4-----------------------------------------------------------------------------------------------------
BarChart(Dept, by1=Gender)

## ---------------------------------------------------------------------------------------------------------------------
BarChart(Dept, by1=Gender, n_col=1, quiet=TRUE)

## ---- fig.width=4-----------------------------------------------------------------------------------------------------
BarChart(Dept, by=Gender, stack100=TRUE)


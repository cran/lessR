## -----------------------------------------------------------------------------
library("lessR")

## ----read---------------------------------------------------------------------
d <- Read("Employee")

## ----bcEx, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Bar chart of tablulated counts of employees in each department."----
BarChart(Dept)

## ---- echo=FALSE, include=FALSE-----------------------------------------------
d <- Read("Employee")

## ----pc1----------------------------------------------------------------------
PieChart(Dept)

## ----hole0, fig.width=4, fig.height=3.5, fig.align='center', fig.cap="Standard pie chart of variable _Dept_ in the _d_ data frame."----
PieChart(Dept, hole=0, quiet=TRUE)


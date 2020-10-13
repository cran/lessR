## ---------------------------------------------------------------------------------------------------------------------
library("lessR")

## ----dataTable, echo=FALSE, out.width='50%', fig.asp=.7, fig.align='center', fig.cap="Structure of a data table."-----
knitr::include_graphics(system.file("img", "DataTable.png", package="lessR"))

## ----rline, fig.align='center', fig.cap="Output of Read()."-----------------------------------------------------------
d <- Read("Employee")

## ---------------------------------------------------------------------------------------------------------------------
l <- Read("Employee_lbl", var_labels=TRUE)


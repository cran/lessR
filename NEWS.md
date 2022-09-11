# lessR version 4.2.3, Sept 11, 2022

## Primary Update

* `interact()`: lessR data analysis is now interactive, with each interactive analysis now displayed for analyzing the user's own data, which also includes summary statistics as output with the option to save the plot to a pdf file plus the corresponding R code


## Updates

* `BarChart()`: Parameter `do_plot` added for the option of computing summary statistics but not plotting

* `Histogram()`: Underlying algorithm more efficient

* `Histogram(..., density=TRUE)`: `fill_gen` and `color_gen` revised to `fill_general` and` color_general`, and `fill_nrm` and `color_nrm` to `fill_normal` and `color_normal` though the old names still accepted, as with `bandwidth` in place of `bw` and `include_histogram` in place of `dn.hist`

* `Histogram()`: for density curve, provided values for `fill_general` and `fill_normal` that are color names now are converted to the same transparency level as their default colors

* `PieChart()`: value labels now separated from the category label so that size can be set independently 

* `pivot()`: Long labels now abbreviated to stay within column widths

* `Plot()`: For numbers < 10000, more decimal digits for stat outcomes of `fit` parameter, such as MSE

* `Plot()`: With a by variable a named, sequential color range such as "reds" can now be specified in addition to the qualitative range of "hues"

* `tt.brief()` removed, long since replaced by `tt_brief()`


## Bug Fixes

* `loading lessR`: Now lessR loads properly if another previously loaded package had loaded shiny

* `Histogram()`: Returns in list structure, such as `h`, saved output from `h <- Histogram(...)`

* `Histogram()`: When `bin_start` was < 0 the number of displayed digits for the mid point of each bin could be excessive, now fixed 

* `Histogram()`: spelling error of out_suggewt to `out_suggest` fixed for output names

* `pivot()`: When constructing a table (instead of a dataframe), `NA`'s now properly displayed

* `Plot()`: Restored title that indicates scaling of bubble plot

* `Plot()`: Transparency on bubble plots now works

* `Read2()`: Now works properly



# lessR version 4.2.2, July 13, 2022

## Updates

* `BoxPlot()`, `Histogram()`: Recent message from outlier check from package `robustbase` removed

* `Logit()`: Confusion matrix now displayed even if all predictions are for a single outcome category

* `pivot()`: If not specified, output defaults to 3 decimal digits, otherwise displays the needed number of decimal digits to avoid rounding to 0.000  

* `pivot()`: For consistency, parameter `n_show_group` changed to `n_group_show`  

* `Plot()`: New parameter `n_bins` > 1 indicates to bin the numeric x-variable and plot the mean or median of the numeric y-variable for each bin, with each point size dependent on the corresponding bin sample size

* `Plot()`: More stats displayed regarding each panel when doing a fit line

* `Read()`: Browsing for data file in RStudio, message about hidden window removed



## Bug Fixes

* `pivot()`: If `na_remove` is `FALSE`, the missing data value for the aggregated statistic was reported as 0 instead of `NA`

* `Plot()`: y-axis label now evaluated on all axis values instead of only the maximum value, which, if 1.0, rounds to 1, only a single digit

* `Plot()`: `segments=TRUE` now applies to all scatter plots, not just with `by=`



# lessR version 4.2.0, June 3, 2022
## Updates

* `interact()`: Each interactive display now shows the underlying function call 

* `Plot()`: If `x` is equally spaced, a line chart not default if a `fit` line

* `Plot()`: Parameter `sqrt`  deprecated for  `quat`

* `Plot()`: Parameter `root`  deprecated for  `power`

* `Plot()`: `fit_color` parameter added to set directly, not just with `style()` function

* `Plot()`: When curve fitting with `fit` parameter, `b0` and `b1` given for the linearized line from nonlinear functions

* `Prop_test()`: If the hypothesis test of a proportion is one-sided, the `alternative` hypothesis is now provided


## Bug Fixes

*  `Histogram()`: `density=TRUE` properly generates console output

*  `Plot()`: For `fit` parameter, `"reciprocal"` option removed as better covered with `"exp"` which also does exponential decay in place of `"reciprocal"` if no `by` variable

*  `Plot()`: `fit_color` now works for a `fit` line

*  `Prop_test()`: parameters such as `alternative` properly passed to R functions `binom.test()`, `prop.test()`, and `chisq.test()`

*  `ttest()`: One-tailed alternatives `less` and `greater` options work



# lessR version 4.1.9, May 4, 2022

## Updates

* `ANOVA()`: One-way, scatterplot labels separated more from plot

* `Histogram()`: Summary stats headings adjusted to better align with numerical values

* `Prop_test()`: Parameter `p0` changed to `pi` for consistency with `ttest()`, `p0` still works

* `reshape_long()`: Default name of `Response` changed from `"Value"` to `"Response"`

* `style()`: Slightly lighten grayscale bars and points from `"gray35"` to `"gray42"` and point `fill` and `color` from `"gray30"` to `"gray42"`

* `train_test()`: New function to either create training and testing data from a data frame with `x` and `y` combined or separated


## Bug Fixes

  * `Plot()`: a `--run-donttest` test example was improperly configured, now works
  * `Plot()`: Plotting a vector for `x` or `y` with `fit` now works 


# lessR version 4.1.8, April 27, 2022

## Updates

* `ANOVA()`: Interaction plot for 2-way factorial reflects color `theme`

* `BarChart()`: Appropriate error message if `by` and `by1` parameters both specified as only one for one function call is allowed

* `BarChart()`: Bar chart from a pivot table for two vars now possible with `NA` column

* `data set`: `dataAnova_rbf` data set for randomized block factorial ANOVA added

* `data set`:  `dataAnova_sp` data set for split-plot design ANOVA added

* `interact()`: More colors added 

* `interact()`: Trellis plot has added `violin_fill` and `box_fill` parameters

* `pivot()`: `by` variables except for `Date` variables by default now converted to factors, though controlled with parameter: `factors`

* `reshape_long()`: If needed, an ID variable is now provided by default

* `reshape_long()`: Generated `ID` variable now has a prefix in front of the integer ID by default, can be changed with the parameter: `prefix`

* `reshape_long()`: `sep` parameter specifies a separator between the prefix and the ID integer output data frame has ID variable listed first

* `Transform()`: Previously deprecated, its abbreviation trans() now removed


## Bug Fixes

* `details()`: Now properly reports details of a `tibble` version of a data frame

* `interact()`: `Histogram()` `fill` now set properly

* `Plot()`: Panels in Trellis plots with no data now properly display as blank with grid lines instead of showing an error message

* `Plot()`: Interaction plot with `segments=TRUE` of a pivot table now has all segments

* `Plot()`: Can now plot one continuous variable with violin/box/scatter plots with `by`, `by1`, and `by2` parameters

* `Plot()`: Bubble plot transparency option now works with parameter `trans`

* `Plot()`: Bubble plot `fill` and `color` now properly specified 


# lessR version 4.1.7, March 30, 2022

## Updates

* `ANOVA()`: ANOVA 2-way factorial analysis provides Type II sum of squares for unbalanced designs

* `ANOVA()`: R-squared and related indices now reported to three decimal digits

* `ANOVA()`: Plotted point in interaction plot a little smaller

* `ANOVA()`: Scatterplot title removed for more room for the plot, labels adjusted

* `ANOVA()`: Scatterplot moved to the first plot displayed

* `BarChart()`: Right-margin legend for two variable chart no longer defaults to abbreviations for title and values, use `legend_abbrev` to specify

* `Histogram()`: Bars slightly more gray and slightly less blue

* `Plot()`: For `by` and `fit` parameters together, equation, MSE, and R2 given for each fit line

* `Regression()`: Analysis of covariance with one categorical variable and one continuous variable displays the ...\cr
    1. regression line and equation for each level of the categorical variable superimposed over the scatterplot\cr
    2. Type II SS for the ANOVA table\cr
    3. test of the interaction of the covariate with the grouping variable

* `Regression()`: `n_cat` parameter added to allow integer variables to be treated as categorical without declaring as a formal R `factor`

* `Regression()`: `kfold` parameter now applies to models with categorical variables with dummy variables created automatically

* `Regression()`: Extra line of spacing between major sections of output

* `Regression()`: Grid lines added to scatterplot

* `Regression()`: title removed from scatterplot, remains in text console output

* `Regression()`: Parameter `best_sub` provides the value `"Cp"` for Mallow's `"Cp"` statistic in addition to the default value `"adjr2"` for adjusted R-squared

* `rename()`: New utility function for renaming a variable in the specified data frame


## Bug Fixes

* `ANOVA()`: Interaction plot for 2-way factorial generalizes better beyond two levels brief output works more reliably for 2-way factorial

* `Regression()`: `scale_response` parameter now works, applicable when `new_scale` is implemented, `FALSE` by default but if `TRUE` then the response variable `y` is also rescaled


# lessR version 4.1.6, February 18, 2022

## Updates

* `ANOVA()`: Better calculation of size of left-margin for pairwise mean differences plot

* `Logit()`: For a single predictor, for a given probability classification
threshold, the `x`-cutoff value now provided

* `Logit()`: For a single predictor, annotations added to the sigmoid curve

* `Logit()`: `ref` parameter for designating the value of the response variable that is the reference group instead of relying upon defaults

* `Logit()`: Rows in the classification table reversed to correspond to sigmoid curve plot

* `pivot()`: For consistency with other `na` parameters, `show_group_na` parameter changed to `na_show_group`

* `Plot()`: Boxplot median line width increased to 2 for better visibility in the presence of an embedded scatterplot


## Bug Fixes

* `Logit()`: Bug fixed that occurred for 50\% of the analyses when the response variable has integer input values of 0,1 instead of a `factor` such as Man/Woman

* `Logit()`: Fitted values now properly displayed when `X1_new`, etc. specified

* `Logit()`: Edge condition that crashed with very little variance among fitted probabilities now fixed

* `Plot()`: `by1` parameter now works when `x` is categorical

* `Regression()`: Table of estimates now properly displayed for indicator variables with large number of characters for the variable names


# lessR version 4.1.5, January 29, 2022

## Updates

* `BarChart()`: `digits_d` parameter added for text output to control number of decimal digits for two categorical variable output

* `BarChart()`: Default gray fill color for the bars lightened slightly

* `data()`:\cr
  `dataStockPrice` updated to Jan 1, 2022\cr
  `dataEmployee_lbl` updated\cr
  `dataWeightLoss` data file added\cr
  `dataAnova_1way` data file added\cr
  `dataAnova_rb` data file added\cr
  `dataAnova_2way` data file added\cr

* `factors()`: Now processes tidyverse `tibble` version of a data frame

* `Help()`: Deprecated, now removed, replaced by vignettes

* `interact()`: When prompting for valid names by passing no arguments, no longer displays an error message

* `interact()`: More colors added to choice of fill colors for interactive displays

* `Logit()`: For a single predictor variable, if the response `y` is non-numeric, then the value set at 1 has the highest mean, for a positive difference

* `Logit()`: For a single predictor variable, logistic curve better labeled 

* `Nest()`: If response variable is a character variable, automatically converted to a `factor` to allow to run without an error

* `Plot()`: For Trellis plots, default changed to a single column (`n_col=1`)

* `Read()`: If specified a `lessR` data file improperly, the list of valid data files is displayed

* `Read()`: Read data files in the Open Document Spreadsheet format, `.ods`

* `Read()`: New parameter row_names for consistent interface across the various file formats, for `cs` files R `row.names` still works

* `Read()`: Report of variables with all unique values moved to brief report

* `Regression()`: If `x` is a factor with two levels, the regression line is plotted in the scatterplot

* `Regression()`: Scatterplot can now have customized axis labels size, set by `style()`

* `reshape_long()`: New function that is a simple wrapper for Base R `reshape()` with sensible parameter names and sensible defaults, and allow for variable ranges of columns to transfer from wide to long

* `reshape_wide()`: New function that is a simple wrapper for Base R `reshape()` with sensible parameter names to convert a long-form data frame to a wide-form

* `Write()`: Parameters -- `data` and `to` -- switched to a more natural order with `data` now the first parameter: `Write(data=, to=, ...)`

* `Write()`: Write data frames in the Open Document Spreadsheet format, `.ods`, with `format="ODS"`

* `Write()`: Write data frames in the SPSS format, `.sav`, with `format="SPSS"`


## Bug Fixes

* `ANOVA()`: Family-wise confidence level now reported in output

* `BarChart()`: Formatting so that columns do not run together for some output with integers represented as integers instead of double

* `BarChart()`: Specifying `one_plot=TRUE` and multiple variables now works

* `BarChart()`: Custom `ylab` now works when the stat parameter is activated

* `Histogram()`: For `density=TRUE`, `quiet=TRUE` now works

* `Logit()`: If many variables, such as from created dummy variables, columns of estimated coefficients now align correctly

* `PieChart()`: Text now responds to corresponding style parameters: `lab_cex`, `main_cex=1.5`, and `values_size`

* `Plot()`: If a `by` or `by1` variable, misleading warning message sort order deleted



# lessR version 4.1.4, December 14, 2021

## Updates

* `ANOVA()`, `BarChart()`, `Histogram()`, `pivot()`, `Plot()`, `Regression()`, `ttest()`: Informative error message added if a subset with the rows parameter returns no rows to subset

* `getColors()`: Little more differentiation between default sequential colors by lightening a bit more the lightest color

* `Histogram()`: Output objects `n.bins` and `n.miss` updated to n_bins and n_miss

* `Histogram()`: `density=TRUE` output provides more summary statistics and suggestions

* `Plot()`: When specifying a fit line with a by categorical variable the sum of squared errors for each category are better labeled

* `Plot()`: For a `by` categorical variable, no longer is the correlational analysis done for all the data, but the SSE is reported for each group

* `Plot()`: Pairwise correlation output takes up fewer lines

* `Plot()`: Variable labels for `y`-axis made a little longer and still fit without breaking into an additional line

* `Plot()`: Some suggestions now change depending on random "coin flip"

* `Plot()`: Trellis plot default changed from `n_col=1` to `n_row=1`

* `Regression()`: Standard deviation of response variable added to fit information, other stats removed

* `Regression()`: Parameter `fit_line` width set at 1.5 if plotting w/o outlier line also and no standard error region for original line

* `to()`: Parameter `same.size` updated to `same_size`, old value still works



## Bug Fixes


* `BarChart()`: For theme other than `"hues"`, `fill` colors the same by default unless an ordered `factor`, then a sequential palette

* `Correlation()`: Pairwise correlation analysis properly returns lower and upper bounds of confidence interval about the sample correlation coefficient

* `Correlation()`: Pairwise correlation when called from `Correlation()` function works

* `PieChart()`: Default colors with default color theme, `"colors"`, now has qualitative color palette `"hues"`, the same as bar chart, for a non-ordinal categorical variable

* `PieChart()`: Inner circle that creates the hole now with higher resolution so as to make a smoother circle

* `PieChart()`: Better placement of the plot title if there is one



# lessR version 4.0.8, November 18, 2021

## Updates

* `ANOVA()`: Randomized blocks fitted means visualizations enhanced

* `ANOVA()`: `rb_points` parameter removed

* `Histogram()`: For grayscale with a black background, histogram bars have no transparency and are lighter

* `interact()`: `"PieChart"` option added for interactive, Shiny display

* `Logit()`: Grid lines added to logit plot for a single predictor transparency of 0.7 added to the points in the logit plot

* `pivot()`: If `n=0` for an output row or column, then converting to proportions returns 0's instead of `NaN`'s

* `pivot()`: New parameter `show_group_na` with default of `TRUE` that will list `NA` for missing data of a grouping variable as a level on the output

* `PieChart()`: Labels proper size outside of RStudio

* `Plot()`: `plot_errors` now plots the connecting segments from each point to the fitted line with gray if `style("gray")`

* `Plot()`: Parameter `size` for size of points now applies to a scatterplot matrix

* `Plot()`: `fill` color for points and bubbles lightened slightly for gray scale

* `Plot()`: Trellis line plots increase slightly the width of the lines

* `recode()`: Transformed variables converted to be consistent with their type ex: convert a `character` variable to `integer`, but before was still `character`

* `Regression()`: For parameter `new_scale`, new option is `"centered"`

* `Regression()`: For all rescaling, now default is only predictor numeric variables and only for numerical variables with more than two values

* `Regression()`: If rescaling, set new parameter `rescale_response` to `TRUE` to also rescale the response variable

* `Regression()`: For model fit, standard deviation and min and max of the target variable displayed along with the 95% range of variation of the residuals, with values displayed with commas for large numbers

* `Regression()`: Fitted vs residuals plot more efficiently sized and largest Cooks Distance value displayed as a sub-title

* `Regression()`: Summary stats of response variable added to Basic Analysis

* `Sort()`: Feedback includes names of sorted variables in place of their index

* `ttest()`: For one group t-test, density curve now has dark gray border and value of smd added to the top text display over the visualization


## Bug Fixes

* `ANOVA()`: Randomized blocks ANOVA displays correct visualizations

* `BarChart()`: Ordinal factors properly display as color gradients, e.g., "blues"

* `PieChart()`: `theme` other than default now implemented

* `Plot()`: For plotting a Trellis plot with a `by` variable, all levels correctly plotted and legend correct

* `Plot()`: If `x` or `y` is categorical and the other continuous, if a `by` variable then `fill` and `color` colors properly displayed at end of display

* `Sort()`: Internal coding simplified for improved functionality and generality

* `ttest()`: `pdf_file` now works for one group t-test of the mean


# lessR version 4.0.6, October 24, 2021

## Updates

* `interact()`: New function for running one of the following shiny apps for interactive visualizations: "BarChart1", "BarChart2", "Histogram", "ScatterPlot", "Trellis", e.g., interact("BarChart1")

* `pivot()`: Read a tidyverse `tibble` in addition to standard R data frames

* `pivot()`: When naming variables, for variable `y`, now `y_mean` instead of `y_mn`

* `Plot()`: For `x=categorical` and `y=continuous`, or vice versa, no jitter added if there are no duplications of `y` at each level of `x`, plus more jitter added for larger sample size if jitter is needed


## Bug Fixes

* `BarChart()`: Sort now works properly with a horizontal plot

* `pivot()`: Now can have one variable to aggregate and one aggregation variable named `x`

* `Regression()`: Debugging code removed from the k-fold cross-validation analysis


# lessR version 4.0.5, October 4, 2021

## Updates

* `ANOVA()`: Scatterplot for 1-way ANOVA now has grid lines and jitter

* `ANOVA()`: Scatterplot for 1-way ANOVA now has `jitter_x` parameter added to customize the jitter level

* `ANOVA()`: Scatterplot for 1-way ANOVA now has means plotted in a dark red instead of a dark gray

* `BarChart()`: Tilde removed from each label if for a `by` variable in the legend where it is nonfunctional
 
* `pivot()`: User defined functions accounted for and named in the output
 
* `pivot()`: When analysis of all data values, dropped the `Grand_Stat` label

* `Plot()`: Point color and fit line now a little darker and complements of each other, `plot_errors` segments a little darker as well

* `Plot()`: For `fit="reciprocal"`, fit line present for all values of `x`, even if plot window does not accommodate the full range, can adjust range with parameter `pad_y`

* `Plot()`: Parameter `root` added for general root transformation beyond the value of 0.5 for the square root

* `Plot()`: Parameter `fit_power` added to raise `y` to the designated power for the `exp`, `root`, and `reciprocal` transformations

* `Plot()`: Correlational analysis no longer displayed for a non-linear fit line

* `Plot()`: SSE displayed for corresponding `fit` line (curve)


## Bug Fixes

* `BarChart()`: Two variable chart with parameters `x` and `by` specified now correctly displays a color range when the style is not the default theme
 
* `BarChart()`: Base R `text()` function misleadingly displays text at size 1 if a value of 0 is entered, lessR functions that rely on the R text function now fixed by changing an input value of 0 to 0.01

* `pivot()`: Output variables now properly named

* `Plot()`: Trellis plots with `by1` and `by2` parameters work in Shiny environment


# lessR version 4.0.3, September 9, 2021

## Updates

* `BarChart()`: If parameter `sta` not specified, there is now no default if the parameter `y` variable is specified without an indication of what statistic to compute

* `BarChart()`: Gentle termination with a message if `stats` specifies a transform that results in missing data in the computed summary table

* `BarChart()`: For analysis of computing a summary table with the parameter `stat` parameter, no longer display the summary stats for the parameter `x` variable when a by variable is present

* `getColors()`: Default qualitative color palette softened a bit by increasing luminance from 55 to 60

* `getColors()`: By default, now does not generate output when called from visualization parameters parameter `fill` and parameter `color` for parameter `BarChart()`, etc.

* `getColors()`: Output parameter values changed to parameter `logical`, parameter `TRUE` and parameter `FALSE`

* `pivot()`: New parameter parameter `table_long` allows the table computation for a cross-tabs table be output in long form

* `pivot()`: Tabulate computation continues to work, though no longer documented as it is supersede


## Bug Fixes

* `BarChart()`: Further adjustment to accommodate very large legend labels for two variable plots

* `BarChart()`: If the y values are input, the display of the values will be properly turned off if the parameter `style` option for values is set to parameter `"off"`

* `Plot()`: Default jitter for 1-D scatterplot works again in VBS plots

* `Plot()`: Parameter `by` works again for VBS plots, multiple plots on same panel

* `Plot()`: Parameter `color` now works in conjunction with the `by` parameter 

* `Plot()`: Applied to a scatterplot of two continuous variables:\cr
  if `color` is set to `"off"` (or `"transparent"`), a requested `fit line` still displays\cr
  means are now properly plotted for a scatterplot of a continuous and
     categorical variable when jitter is applied to the plotted points\cr
  default outlier shape in gray scale changes to diamond as documented


# lessR version 4.0.2, August 5, 2021 

## Updates

* `BarChart()`: For a two categorical variable bar chart, the `legend` labels are by default no longer abbreviated, instead sufficient room is generated by the plot
 
* `BarChart()`: New parameter `legend_abbrev` allows for the specification of the maximum number of characters to display for the `legend` labels
 
* `BarChart()`: For a two categorical variable bar chart, better default placement of the `legend` new parameter `legend_adj` allows for horizontal adjustment of the `legend`


## Bug Fixes

* `BarChart()`: First variable `x` is a vector works again

* `Histogram()`: Parameter `pdf_file` works

* `Plot()`: Parameter `enhance=TRUE` correctly prints least-squares line when fill changed



# lessR version 4.0.1, June 6, 2021 

## Updates

* `BarChart()`: When fill or color set with `getColors()`, the value of n, the number of colors, is provided according to the number of bars

* `BarChart()`: When horiz is `TRUE`, order of sort changed so that `"-"` starts at top

* `BarChart()`: Space to separate the `x`-axis label when several lines revised parameter `pdf` to `pdf_file`

* `corProp()`: Alias changed from `prop` to `cp`, so `prop` for new function `Proportion()`

* `Density()`: Parameter `pdf` to `pdf_file`

* `Histogram()`: Parameter `pdf` to `pdf_file`

* `kurtosis()`: New function for computing kurtosis, available for the `pivot()` function

* `pivot()`: Choose any two of the following three parameters: multiple compute functions, multiple values over which to compute, and multiple grouping (`by`) variables

* `pivot()`:  No `by` variables imply to do grand total(s) without an aggregation

* `pivot()`: Choose any two of the following three parameters: multiple compute functions, multiple variables, and multiple `by`-variables.

* `pivot()`: Sort parameter applies for a single aggregated numerical variable and specifies the variable or column number to sort

* `pivot()`: For each non-decimal digit variable with unique data values, display

* `pivot()`: The variable name and the unique values to better label the output when drilling down into levels of one or more categorical variables

* `pivot()`: Compute `quantiles()` function provides a standard data frame

* `pivot()`: Compute `table()` function provides a cross-classification table for a discrete aggregated variable

* `pivot()`: Table frequencies can be converted to proportions, and row and column proportions

* `pivot()`: Parameter `out_names` parameter to custom-name aggregated variables

* `pivot()`: Parameter `na_by` renamed to `na_by_show` to better communicate its meaning skew and kurtosis added to the list of available descriptive statistics

* `pivot()`: Parameter `q_num` specifies number of intervals for quantiles 

* `pivot()`: Parameter `rows` subsets rows of the data frame for analysis

* `pivot()`: Parameter `valu`e changed to parameter variable

* `pivot()`: Parameters `rows_by` and `cols_by` changed to `by` and `by_cols`

* `Plot()`: Scatterplot matrix does a loess fit if `fit=TRUE`

* `Plot()`: If multiple `x` or `y` variables, text correlation analysis displayed for each combination

* `Plot()`: If a `by` variable, then text correlation for each level not yet available, so not displayed 

* `Plot()`: Default for plotting a fit line with by groups is to not plot the standard errors because the plot becomes overcrowded

* `Plot()`: Warning displayed if the date field or a by/by1 field is not ordered

* `Plot()`: Stacked time series chart of different shades of same hue, such as `"blues"` now have each sub-plot with a black border

* `Plot()`: Trellis plot distinguishes `area_fill` under a line from `fill` for a point

* `Plot()`: `radius` value for bubble plot frequency matrix dependent on size of

* `Plot()`: Largest frequency `radius` value displayed for bubble plot frequency matrix

* `prob_norm()`: Probability is returned as the value of the function, not just printed

* `prob_tcut()`: Cutoff is returned as the value of the function, not just printed

* `Prop_test()`: New function for the analysis of proportions: test of homogeneity for one or more samples based on the proportion of successes for a specific value of a variable, and for a single sample, goodness-of-fit for a single categorical variable and test of independence for two categorical variables 

* `Prop_test()`: `prop` is alias
  
* `recode()`: Changed to lower case function name 

* `rescale()`: Changed to lower case function name

* `Regression()`: Bubble plot form of a scatterplot for `integer` variables with less than 10 unique values and a single predictor variable

* `skew()`: New function for computing skew, available for the `pivot()` function

* `style()`: `slatered` theme added

* `style()`: `ellipse_fill` for default `"colors"` theme now a pale brown instead of blue


## Bug Fixes

* `BarChart()`: With more than 30 categories, a message to indicate how to retrieve now properly displays instead of crashing

* `BarChart()`: `fill=(count)` would fail for a large number of levels

* `BarChart()`: Stacked bars from multiple items now works with variable labels

* `BarChart()`: Stacked bars from multiple items now works for binary responses

* `Extract()`: Now works if name of data frame is a function in another context

* `Extract()`: Now works even if one variable name is embedded in another name

* `Plot()`: Forcing an evaluation of a scalar logical on a vector fixed

* `Plot()`: `theme` parameter works for most common parameters

* `Plot()`: Bubble plot frequency matrix text output works with labels

* `Plot()`: Bubble plot frequency matrix plot `x`-axis works with binary response

* `Plot()`: `area_fill` works correctly
 
* `ttest()`: For paired analysis, `x`-axis properly labeled Difference

`
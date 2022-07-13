\name{NEWS}
\title{News for Package \pkg{lessR}}

\section{in version 4.2.2}{

\subsection{Updates}{
\itemize{
\item \code{BoxPlot()}, \code{Histogram()}: Recent message from outlier check from package \code{robustbase} removed

\item \code{Logit()}: Confusion matrix now displayed even if all predictions are for a single outcome category

\item \code{pivot()}: If not specified, output defaults to 3 decimal digits, otherwise displays the needed number of decimal digits to avoid rounding to 0.000  

\item \code{pivot()}: For consistency, parameter \code{n_show_group} changed to \code{n_group_show}  

\item \code{Plot()}: New parameter \code{n_bins} > 1 indicates to bin the numeric x-variable and plot the mean or median of the numeric y-variable for each bin, with each point size dependent on the corresponding bin sample size

\item \code{Plot()}: More stats displayed regarding each panel when doing a fit line

\item \code{Read()}: Browsing for data file in RStudio, message about hidden window removed

}}

\subsection{Bug Fixes}{
\itemize{
\item \code{pivot()}: If \code{na_remove} is \code{FALSE}, the missing data value for the aggregated statistic was reported as 0 instead of \code{NA}

\item \code{Plot()}: y-axis label now evaluated on all axis values instead of only the maximum value, which, if 1.0, rounds to 1, only a single digit

\item \code{Plot()}: \code{segments=TRUE} now applies to all scatter plots, not just with \code{by=}
}}
}


\section{in version 4.2.0}{
\subsection{Updates}{
\itemize{
\item \code{interact()}: Each interactive display now shows the underlying function call 

\item \code{Plot()}: If \code{x} is equally spaced, a line chart not default if a \code{fit} line

\item \code{Plot()}: Parameter \code{sqrt}  deprecated for  \code{quat}

\item \code{Plot()}: Parameter \code{root}  deprecated for  \code{power}

\item \code{Plot()}: \code{fit_color} parameter added to set directly, not just with \code{style()} function

\item \code{Plot()}: When curve fitting with \code{fit} parameter, \code{b0} and \code{b1} given for the linearized line from nonlinear functions

\item \code{Prop_test()}: If the hypothesis test of a proportion is one-sided, the \code{alternative} hypothesis is now provided
}}

\subsection{Bug Fixes}{
\itemize{
\item  \code{Histogram()}: \code{density=TRUE} properly generates console output

\item  \code{Plot()}: For \code{fit} parameter, \code{"reciprocal"} option removed as better covered with \code{"exp"} which also does exponential decay in place of \code{"reciprocal"} if no \code{by} variable

\item  \code{Plot()}: \code{fit_color} now works for a \code{fit} line

\item  \code{Prop_test()}: parameters such as \code{alternative} properly passed to R functions \code{binom.test()}, \code{prop.test()}, and \code{chisq.test()}

\item  \code{ttest()}: One-tailed alternatives \code{less} and \code{greater} options work
}}
}


\section{in version 4.1.9}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}: One-way, scatterplot labels separated more from plot

\item \code{Histogram()}: Summary stats headings adjusted to better align with numerical values

\item \code{Prop_test()}: Parameter \code{p0} changed to \code{pi} for consistency with \code{ttest()}, \code{p0} still works

\item \code{reshape_long()}: Default name of \code{Response} changed from \code{"Value"} to \code{"Response"}

\item \code{style()}: Slightly lighten grayscale bars and points from \code{"gray35"} to \code{"gray42"} and point \code{fill} and \code{color} from \code{"gray30"} to \code{"gray42"}

\item \code{train_test()}: New function to either create training and testing data from a data frame with \code{x} and \code{y} combined or separated
}}

\subsection{Bug Fixes}{
\itemize{
  \item \code{Plot()}: a \code{--run-donttest} test example was improperly configured, now works
  \item \code{Plot()}: Plotting a vector for \code{x} or \code{y} with \code{fit} now works 
}}
}


\section{in version 4.1.8}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}: Interaction plot for 2-way factorial reflects color \code{theme}

\item \code{BarChart()}: Appropriate error message if \code{by} and \code{by1} parameters both specified as only one for one function call is allowed

\item \code{BarChart()}: Bar chart from a pivot table for two vars now possible with \code{NA} column

\item \code{data set}: \code{dataAnova_rbf} data set for randomized block factorial ANOVA added

\item \code{data set}:  \code{dataAnova_sp} data set for split-plot design ANOVA added

\item \code{interact()}: More colors added 

\item \code{interact()}: Trellis plot has added \code{violin_fill} and \code{box_fill} parameters

\item \code{pivot()}: \code{by} variables except for \code{Date} variables by default now converted to factors, though controlled with parameter: \code{factors}

\item \code{reshape_long()}: If needed, an ID variable is now provided by default

\item \code{reshape_long()}: Generated \code{ID} variable now has a prefix in front of the integer ID by default, can be changed with the parameter: \code{prefix}

\item \code{reshape_long()}: \code{sep} parameter specifies a separator between the prefix and the ID integer output data frame has ID variable listed first

\item \code{Transform()}: Previously deprecated, its abbreviation trans() now removed
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{details()}: Now properly reports details of a \code{tibble} version of a data frame

\item \code{interact()}: \code{Histogram()} \code{fill} now set properly

\item \code{Plot()}: Panels in Trellis plots with no data now properly display as blank with grid lines instead of showing an error message

\item \code{Plot()}: Interaction plot with \code{segments=TRUE} of a pivot table now has all segments

\item \code{Plot()}: Can now plot one continuous variable with violin/box/scatter plots with \code{by}, \code{by1}, and \code{by2} parameters

\item \code{Plot()}: Bubble plot transparency option now works with parameter \code{trans}

\item \code{Plot()}: Bubble plot \code{fill} and \code{color} now properly specified 
}}
}


\section{in version 4.1.7}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}: ANOVA 2-way factorial analysis provides Type II sum of squares for unbalanced designs

\item \code{ANOVA()}: R-squared and related indices now reported to three decimal digits

\item \code{ANOVA()}: Plotted point in interaction plot a little smaller

\item \code{ANOVA()}: Scatterplot title removed for more room for the plot, labels adjusted

\item \code{ANOVA()}: Scatterplot moved to the first plot displayed

\item \code{BarChart()}: Right-margin legend for two variable chart no longer defaults to abbreviations for title and values, use \code{legend_abbrev} to specify

\item \code{Histogram()}: Bars slightly more gray and slightly less blue

\item \code{Plot()}: For \code{by} and \code{fit} parameters together, equation, MSE, and R2 given for each fit line

\item \code{Regression()}: Analysis of covariance with one categorical variable and one continuous variable displays the ...\cr
    1. regression line and equation for each level of the categorical variable superimposed over the scatterplot\cr
    2. Type II SS for the ANOVA table\cr
    3. test of the interaction of the covariate with the grouping variable

\item \code{Regression()}: \code{n_cat} parameter added to allow integer variables to be treated as categorical without declaring as a formal R \code{factor}

\item \code{Regression()}: \code{kfold} parameter now applies to models with categorical variables with dummy variables created automatically

\item \code{Regression()}: Extra line of spacing between major sections of output

\item \code{Regression()}: Grid lines added to scatterplot

\item \code{Regression()}: title removed from scatterplot, remains in text console output

\item \code{Regression()}: Parameter \code{best_sub} provides the value \code{"Cp"} for Mallow's \code{"Cp"} statistic in addition to the default value \code{"adjr2"} for adjusted R-squared

\item \code{rename()}: New utility function for renaming a variable in the specified data frame
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{ANOVA()}: Interaction plot for 2-way factorial generalizes better beyond two levels brief output works more reliably for 2-way factorial

\item \code{Regression()}: \code{scale_response} parameter now works, applicable when \code{new_scale} is implemented, \code{FALSE} by default but if \code{TRUE} then the response variable \code{y} is also rescaled
}}
}


\section{in version 4.1.6}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}: Better calculation of size of left-margin for pairwise mean differences plot

\item \code{Logit()}: For a single predictor, for a given probability classification
threshold, the \code{x}-cutoff value now provided

\item \code{Logit()}: For a single predictor, annotations added to the sigmoid curve

\item \code{Logit()}: \code{ref} parameter for designating the value of the response variable that is the reference group instead of relying upon defaults

\item \code{Logit()}: Rows in the classification table reversed to correspond to sigmoid curve plot

\item \code{pivot()}: For consistency with other \code{na} parameters, \code{show_group_na} parameter changed to \code{na_show_group}

\item \code{Plot()}: Boxplot median line width increased to 2 for better visibility in the presence of an embedded scatterplot
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{Logit()}: Bug fixed that occurred for 50\% of the analyses when the response variable has integer input values of 0,1 instead of a \code{factor} such as Man/Woman

\item \code{Logit()}: Fitted values now properly displayed when \code{X1_new}, etc. specified

\item \code{Logit()}: Edge condition that crashed with very little variance among fitted probabilities now fixed

\item \code{Plot()}: \code{by1} parameter now works when \code{x} is categorical

\item \code{Regression()}: Table of estimates now properly displayed for indicator variables with large number of characters for the variable names
}}
}


\section{in version 4.1.5}{

\subsection{Updates}{
\itemize{
\item \code{BarChart()}: \code{digits_d} parameter added for text output to control number of decimal digits for two categorical variable output

\item \code{BarChart()}: Default gray fill color for the bars lightened slightly

\item \code{data()}:\cr
  \code{dataStockPrice} updated to Jan 1, 2022\cr
  \code{dataEmployee_lbl} updated\cr
  \code{dataWeightLoss} data file added\cr
  \code{dataAnova_1way} data file added\cr
  \code{dataAnova_rb} data file added\cr
  \code{dataAnova_2way} data file added\cr

\item \code{factors()}: Now processes tidyverse \code{tibble} version of a data frame

\item \code{Help()}: Deprecated, now removed, replaced by vignettes

\item \code{interact()}: When prompting for valid names by passing no arguments, no longer displays an error message

\item \code{interact()}: More colors added to choice of fill colors for interactive displays

\item \code{Logit()}: For a single predictor variable, if the response \code{y} is non-numeric, then the value set at 1 has the highest mean, for a positive difference

\item \code{Logit()}: For a single predictor variable, logistic curve better labeled 

\item \code{Nest()}: If response variable is a character variable, automatically converted to a \code{factor} to allow to run without an error

\item \code{Plot()}: For Trellis plots, default changed to a single column (\code{n_col=1})

\item \code{Read()}: If specified a \code{lessR} data file improperly, the list of valid data files is displayed

\item \code{Read()}: Read data files in the Open Document Spreadsheet format, \code{.ods}

\item \code{Read()}: New parameter row_names for consistent interface across the various file formats, for \code{cs} files R \code{row.names} still works

\item \code{Read()}: Report of variables with all unique values moved to brief report

\item \code{Regression()}: If \code{x} is a factor with two levels, the regression line is plotted in the scatterplot

\item \code{Regression()}: Scatterplot can now have customized axis labels size, set by \code{style()}

\item \code{reshape_long()}: New function that is a simple wrapper for Base R \code{reshape()} with sensible parameter names and sensible defaults, and allow for variable ranges of columns to transfer from wide to long

\item \code{reshape_wide()}: New function that is a simple wrapper for Base R \code{reshape()} with sensible parameter names to convert a long-form data frame to a wide-form

\item \code{Write()}: Parameters -- \code{data} and \code{to} -- switched to a more natural order with \code{data} now the first parameter: \code{Write(data=, to=, ...)}

\item \code{Write()}: Write data frames in the Open Document Spreadsheet format, \code{.ods}, with \code{format="ODS"}

\item \code{Write()}: Write data frames in the SPSS format, \code{.sav}, with \code{format="SPSS"}
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{ANOVA()}: Family-wise confidence level now reported in output

\item \code{BarChart()}: Formatting so that columns do not run together for some output with integers represented as integers instead of double

\item \code{BarChart()}: Specifying \code{one_plot=TRUE} and multiple variables now works

\item \code{BarChart()}: Custom \code{ylab} now works when the stat parameter is activated

\item \code{Histogram()}: For \code{density=TRUE}, \code{quiet=TRUE} now works

\item \code{Logit()}: If many variables, such as from created dummy variables, columns of estimated coefficients now align correctly

\item \code{PieChart()}: Text now responds to corresponding style parameters: \code{lab_cex}, \code{main_cex=1.5}, and \code{values_size}

\item \code{Plot()}: If a \code{by} or \code{by1} variable, misleading warning message sort order deleted
}}
}


\section{in version 4.1.4}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}, \code{BarChart()}, \code{Histogram()}, \code{pivot()}, \code{Plot()}, \code{Regression()}, \code{ttest()}: Informative error message added if a subset with the rows parameter returns no rows to subset

\item \code{getColors()}: Little more differentiation between default sequential colors by lightening a bit more the lightest color

\item \code{Histogram()}: Output objects \code{n.bins} and \code{n.miss} updated to n_bins and n_miss

\item \code{Histogram()}: \code{density=TRUE} output provides more summary statistics and suggestions

\item \code{Plot()}: When specifying a fit line with a by categorical variable the sum of squared errors for each category are better labeled

\item \code{Plot()}: For a \code{by} categorical variable, no longer is the correlational analysis done for all the data, but the SSE is reported for each group

\item \code{Plot()}: Pairwise correlation output takes up fewer lines

\item \code{Plot()}: Variable labels for \code{y}-axis made a little longer and still fit without breaking into an additional line

\item \code{Plot()}: Some suggestions now change depending on random "coin flip"

\item \code{Plot()}: Trellis plot default changed from \code{n_col=1} to \code{n_row=1}

\item \code{Regression()}: Standard deviation of response variable added to fit information, other stats removed

\item \code{Regression()}: Parameter \code{fit_line} width set at 1.5 if plotting w/o outlier line also and no standard error region for original line

\item \code{to()}: Parameter \code{same.size} updated to \code{same_size}, old value still works

}}

\subsection{Bug Fixes}{
\itemize{

\item \code{BarChart()}: For theme other than \code{"hues"}, \code{fill} colors the same by default unless an ordered \code{factor}, then a sequential palette

\item \code{Correlation()}: Pairwise correlation analysis properly returns lower and upper bounds of confidence interval about the sample correlation coefficient

\item \code{Correlation()}: Pairwise correlation when called from \code{Correlation()} function works

\item \code{PieChart()}: Default colors with default color theme, \code{"colors"}, now has qualitative color palette \code{"hues"}, the same as bar chart, for a non-ordinal categorical variable

\item \code{PieChart()}: Inner circle that creates the hole now with higher resolution so as to make a smoother circle

\item \code{PieChart()}: Better placement of the plot title if there is one

}}
}


\section{in version 4.0.8}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}: Randomized blocks fitted means visualizations enhanced

\item \code{ANOVA()}: \code{rb_points} parameter removed

\item \code{Histogram()}: For grayscale with a black background, histogram bars have no transparency and are lighter

\item \code{interact()}: \code{"PieChart"} option added for interactive, Shiny display

\item \code{Logit()}: Grid lines added to logit plot for a single predictor transparency of 0.7 added to the points in the logit plot

\item \code{pivot()}: If \code{n=0} for an output row or column, then converting to proportions returns 0's instead of \code{NaN}'s

\item \code{pivot()}: New parameter \code{show_group_na} with default of \code{TRUE} that will list \code{NA} for missing data of a grouping variable as a level on the output

\item \code{PieChart()}: Labels proper size outside of RStudio

\item \code{Plot()}: \code{plot_errors} now plots the connecting segments from each point to the fitted line with gray if \code{style("gray")}

\item \code{Plot()}: Parameter \code{size} for size of points now applies to a scatterplot matrix

\item \code{Plot()}: \code{fill} color for points and bubbles lightened slightly for gray scale

\item \code{Plot()}: Trellis line plots increase slightly the width of the lines

\item \code{recode()}: Transformed variables converted to be consistent with their type ex: convert a \code{character} variable to \code{integer}, but before was still \code{character}

\item \code{Regression()}: For parameter \code{new_scale}, new option is \code{"centered"}

\item \code{Regression()}: For all rescaling, now default is only predictor numeric variables and only for numerical variables with more than two values

\item \code{Regression()}: If rescaling, set new parameter \code{rescale_response} to \code{TRUE} to also rescale the response variable

\item \code{Regression()}: For model fit, standard deviation and min and max of the target variable displayed along with the 95% range of variation of the residuals, with values displayed with commas for large numbers

\item \code{Regression()}: Fitted vs residuals plot more efficiently sized and largest Cooks Distance value displayed as a sub-title

\item \code{Regression()}: Summary stats of response variable added to Basic Analysis

\item \code{Sort()}: Feedback includes names of sorted variables in place of their index

\item \code{ttest()}: For one group t-test, density curve now has dark gray border and value of smd added to the top text display over the visualization
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{ANOVA()}: Randomized blocks ANOVA displays correct visualizations

\item \code{BarChart()}: Ordinal factors properly display as color gradients, e.g., "blues"

\item \code{PieChart()}: \code{theme} other than default now implemented

\item \code{Plot()}: For plotting a Trellis plot with a \code{by} variable, all levels correctly plotted and legend correct

\item \code{Plot()}: If \code{x} or \code{y} is categorical and the other continuous, if a \code{by} variable then \code{fill} and \code{color} colors properly displayed at end of display

\item \code{Sort()}: Internal coding simplified for improved functionality and generality

\item \code{ttest()}: \code{pdf_file} now works for one group t-test of the mean
}}
}


\section{in version 4.0.6}{

\subsection{Updates}{
\itemize{
\item \code{interact()}: New function for running one of the following shiny apps for interactive visualizations: "BarChart1", "BarChart2", "Histogram", "ScatterPlot", "Trellis", e.g., interact("BarChart1")

\item \code{pivot()}: Read a tidyverse \code{tibble} in addition to standard R data frames

\item \code{pivot()}: When naming variables, for variable \code{y}, now \code{y_mean} instead of \code{y_mn}

\item \code{Plot()}: For \code{x=categorical} and \code{y=continuous}, or vice versa, no jitter added if there are no duplications of \code{y} at each level of \code{x}, plus more jitter added for larger sample size if jitter is needed
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{BarChart()}: Sort now works properly with a horizontal plot

\item \code{pivot()}: Now can have one variable to aggregate and one aggregation variable named \code{x}

\item \code{Regression()}: Debugging code removed from the k-fold cross-validation analysis
}}
}


\section{in version 4.0.5}{

\subsection{Updates}{
\itemize{
\item \code{ANOVA()}: Scatterplot for 1-way ANOVA now has grid lines and jitter

\item \code{ANOVA()}: Scatterplot for 1-way ANOVA now has \code{jitter_x} parameter added to customize the jitter level

\item \code{ANOVA()}: Scatterplot for 1-way ANOVA now has means plotted in a dark red instead of a dark gray

\item \code{BarChart()}: Tilde removed from each label if for a \code{by} variable in the legend where it is nonfunctional
 
\item \code{pivot()}: User defined functions accounted for and named in the output
 
\item \code{pivot()}: When analysis of all data values, dropped the \code{Grand_Stat} label

\item \code{Plot()}: Point color and fit line now a little darker and complements of each other, \code{plot_errors} segments a little darker as well

\item \code{Plot()}: For \code{fit="reciprocal"}, fit line present for all values of \code{x}, even if plot window does not accommodate the full range, can adjust range with parameter \code{pad_y}

\item \code{Plot()}: Parameter \code{root} added for general root transformation beyond the value of 0.5 for the square root

\item \code{Plot()}: Parameter \code{fit_power} added to raise \code{y} to the designated power for the \code{exp}, \code{root}, and \code{reciprocal} transformations

\item \code{Plot()}: Correlational analysis no longer displayed for a non-linear fit line

\item \code{Plot()}: SSE displayed for corresponding \code{fit} line (curve)
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{BarChart()}: Two variable chart with parameters \code{x} and \code{by} specified now correctly displays a color range when the style is not the default theme
 
\item \code{BarChart()}: Base R \code{text()} function misleadingly displays text at size 1 if a value of 0 is entered, lessR functions that rely on the R text function now fixed by changing an input value of 0 to 0.01

\item \code{pivot()}: Output variables now properly named

\item \code{Plot()}: Trellis plots with \code{by1} and \code{by2} parameters work in Shiny environment
}}
}


\section{in version 4.0.3}{

\subsection{Updates}{
\itemize{
\item \code{BarChart()}: If parameter \code{sta} not specified, there is now no default if the parameter \code{y} variable is specified without an indication of what statistic to compute

\item \code{BarChart()}: Gentle termination with a message if \code{stats} specifies a transform that results in missing data in the computed summary table

\item \code{BarChart()}: For analysis of computing a summary table with the parameter \code{stat} parameter, no longer display the summary stats for the parameter \code{x} variable when a by variable is present

\item \code{getColors()}: Default qualitative color palette softened a bit by increasing luminance from 55 to 60

\item \code{getColors()}: By default, now does not generate output when called from visualization parameters parameter \code{fill} and parameter \code{color} for parameter \code{BarChart()}, etc.

\item \code{getColors()}: Output parameter values changed to parameter \code{logical}, parameter \code{TRUE} and parameter \code{FALSE}

\item \code{pivot()}: New parameter parameter \code{table_long} allows the table computation for a cross-tabs table be output in long form

\item \code{pivot()}: Tabulate computation continues to work, though no longer documented as it is supersede
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{BarChart()}: Further adjustment to accommodate very large legend labels for two variable plots

\item \code{BarChart()}: If the y values are input, the display of the values will be properly turned off if the parameter \code{style} option for values is set to parameter \code{"off"}

\item \code{Plot()}: Default jitter for 1-D scatterplot works again in VBS plots

\item \code{Plot()}: Parameter \code{by} works again for VBS plots, multiple plots on same panel

\item \code{Plot()}: Parameter \code{color} now works in conjunction with the \code{by} parameter 

\item \code{Plot()}: Applied to a scatterplot of two continuous variables:\cr
  if \code{color} is set to \code{"off"} (or \code{"transparent"}), a requested \code{fit line} still displays\cr
  means are now properly plotted for a scatterplot of a continuous and
     categorical variable when jitter is applied to the plotted points\cr
  default outlier shape in gray scale changes to diamond as documented
}}
}


\section{in version 4.0.2}{

\subsection{Updates}{
\itemize{
\item \code{BarChart()}: For a two categorical variable bar chart, the \code{legend} labels are by default no longer abbreviated, instead sufficient room is generated by the plot
 
\item \code{BarChart()}: New parameter \code{legend_abbrev} allows for the specification of the maximum number of characters to display for the \code{legend} labels
 
\item \code{BarChart()}: For a two categorical variable bar chart, better default placement of the \code{legend} new parameter \code{legend_adj} allows for horizontal adjustment of the \code{legend}
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{BarChart()}: First variable \code{x} is a vector works again

\item \code{Histogram()}: Parameter \code{pdf_file} works

\item \code{Plot()}: Parameter \code{enhance=TRUE} correctly prints least-squares line when fill changed
}}
}


\section{in version 4.0.0}{

\subsection{Updates}{
\itemize{
\item \code{BarChart()}: When fill or color set with \code{getColors()}, the value of n, the number of colors, is provided according to the number of bars

\item \code{BarChart()}: When horiz is \code{TRUE}, order of sort changed so that \code{"-"} starts at top

\item \code{BarChart()}: Space to separate the \code{x}-axis label when several lines revised parameter \code{pdf} to \code{pdf_file}

\item \code{corProp()}: Alias changed from \code{prop} to \code{cp}, so \code{prop} for new function \code{Proportion()}

\item \code{Density()}: Parameter \code{pdf} to \code{pdf_file}

\item \code{Histogram()}: Parameter \code{pdf} to \code{pdf_file}

\item \code{kurtosis()}: New function for computing kurtosis, available for the \code{pivot()} function

\item \code{pivot()}: Choose any two of the following three parameters: multiple compute functions, multiple values over which to compute, and multiple grouping (\code{by}) variables

\item \code{pivot()}:  No \code{by} variables imply to do grand total(s) without an aggregation

\item \code{pivot()}: Choose any two of the following three parameters: multiple compute functions, multiple variables, and multiple \code{by}-variables.

\item \code{pivot()}: Sort parameter applies for a single aggregated numerical variable and specifies the variable or column number to sort

\item \code{pivot()}: For each non-decimal digit variable with unique data values, display

\item \code{pivot()}: The variable name and the unique values to better label the output when drilling down into levels of one or more categorical variables

\item \code{pivot()}: Compute \code{quantiles()} function provides a standard data frame

\item \code{pivot()}: Compute \code{table()} function provides a cross-classification table for a discrete aggregated variable

\item \code{pivot()}: Table frequencies can be converted to proportions, and row and column proportions

\item \code{pivot()}: Parameter \code{out_names} parameter to custom-name aggregated variables

\item \code{pivot()}: Parameter \code{na_by} renamed to \code{na_by_show} to better communicate its meaning skew and kurtosis added to the list of available descriptive statistics

\item \code{pivot()}: Parameter \code{q_num} specifies number of intervals for quantiles 

\item \code{pivot()}: Parameter \code{rows} subsets rows of the data frame for analysis

\item \code{pivot()}: Parameter \code{valu}e changed to parameter variable

\item \code{pivot()}: Parameters \code{rows_by} and \code{cols_by} changed to \code{by} and \code{by_cols}

\item \code{Plot()}: Scatterplot matrix does a loess fit if \code{fit=TRUE}

\item \code{Plot()}: If multiple \code{x} or \code{y} variables, text correlation analysis displayed for each combination

\item \code{Plot()}: If a \code{by} variable, then text correlation for each level not yet available, so not displayed 

\item \code{Plot()}: Default for plotting a fit line with by groups is to not plot the standard errors because the plot becomes overcrowded

\item \code{Plot()}: Warning displayed if the date field or a by/by1 field is not ordered

\item \code{Plot()}: Stacked time series chart of different shades of same hue, such as \code{"blues"} now have each sub-plot with a black border

\item \code{Plot()}: Trellis plot distinguishes \code{area_fill} under a line from \code{fill} for a point

\item \code{Plot()}: \code{radius} value for bubble plot frequency matrix dependent on size of

\item \code{Plot()}: Largest frequency \code{radius} value displayed for bubble plot frequency matrix

\item \code{prob_norm()}: Probability is returned as the value of the function, not just printed

\item \code{prob_tcut()}: Cutoff is returned as the value of the function, not just printed

\item \code{Prop_test()}: New function for the analysis of proportions: test of homogeneity for one or more samples based on the proportion of successes for a specific value of a variable, and for a single sample, goodness-of-fit for a single categorical variable and test of independence for two categorical variables 

\item \code{Prop_test()}: \code{prop} is alias
  
\item \code{recode()}: Changed to lower case function name 

\item \code{rescale()}: Changed to lower case function name

\item \code{Regression()}: Bubble plot form of a scatterplot for \code{integer} variables with less than 10 unique values and a single predictor variable

\item \code{skew()}: New function for computing skew, available for the \code{pivot()} function

\item \code{style()}: \code{slatered} theme added

\item \code{style()}: \code{ellipse_fill} for default \code{"colors"} theme now a pale brown instead of blue
}}

\subsection{Bug Fixes}{
\itemize{
\item \code{BarChart()}: With more than 30 categories, a message to indicate how to retrieve now properly displays instead of crashing

\item \code{BarChart()}: \code{fill=(count)} would fail for a large number of levels

\item \code{BarChart()}: Stacked bars from multiple items now works with variable labels

\item \code{BarChart()}: Stacked bars from multiple items now works for binary responses

\item \code{Extract()}: Now works if name of data frame is a function in another context

\item \code{Extract()}: Now works even if one variable name is embedded in another name

\item \code{Plot()}: Forcing an evaluation of a scalar logical on a vector fixed

\item \code{Plot()}: \code{theme} parameter works for most common parameters

\item \code{Plot()}: Bubble plot frequency matrix text output works with labels

\item \code{Plot()}: Bubble plot frequency matrix plot \code{x}-axis works with binary response

\item \code{Plot()}: \code{area_fill} works correctly
 
\item \code{ttest()}: For paired analysis, \code{x}-axis properly labeled Difference
}}
}

# SQUEAK - Statistical Quantification & Understanding of Experimental Animal Kinetics

SQUEAK is a package to perform regression modelling and subsequent visualisation of mouse tumour growth data. It is designed to provide accessible access to high quality statistical analysis to support comparisons between groups, while accounting for the inherent variability between individual animals.

This package stands on the shoulders of giants, and wraps a number of excellent other tools that should be given due credit, principally: **nlme, ggplot2 and sjPlot.**

### Installation

Currently, SQUEAK is available through github and requires the `devtools` library.

``` r
# install.packages("devtools")
devtools::install_github("seedgeorge/SQUEAK")
```

### Minimal Usage Example

The following code uses a dataset included in the package, performs a simple data check, and then makes a simple plot.

``` r
data("long_mice")
ready_data = check_long(long_mice,
                        timecol = "Days",
                        IDcol = 'Number',
                        groupcol = 'Group',
                        measurementcol = 'Value')
plot_raw_lines(ready_data = ready_data )
mixed_effect_model(ready_data = ready_data)
```

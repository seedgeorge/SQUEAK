# SQUEAK - Statistical Quantification & Understanding of Experimental Animal Kinetics

SQUEAK is a package to perform regression modelling and subsequent visualisation of mouse tumour growth data. It is designed to provide high quality statistical analysis to support comparisons between groups, while accounting for the inherent variability between individual animals.

### Installation

Currently, SQUEAK is available through github and requires the `devtools` library.

``` R
# install.packages("devtools")
devtools::install_github("seedgeorge/SQUEAK")
```

### Minimal Usage Example

The following code uses an dataset included in the package, performs a simple data check, and then makes a simple plot.

``` R
data("long_mice")
ready_data = check_long(long_mice,
                        timecol = "Days",
                        IDcol = 'Number',
                        groupcol = 'Group',
                        measurementcol = 'Value')
plot_raw_lines(ready_data = ready_data )
```

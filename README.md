
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplots

<!-- badges: start -->
<!-- badges: end -->

The goal of `tidyplots` is make to it ridiculously simple to create
scientific plots by gradually adding, changing and adjusting plot
components. It follows a similar logic as `ggplot2`, but uses the pipe
`%>%` instead of `+`, which allows for more flexible and powerful
workflows. Moreover, `tidyplots` is making the most common scientific
plotting routines more accessible, by proving a clean and minimalist
interface. Finally, you can always `add()` ggplot code, to handle those
cases where you need full control.

## Installation

You can install the development version of `tidyplots` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jbengler/tidyplots")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyplots)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplots

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

tidyplots is an opinionated plotting package that makes to it
ridiculously simple to create ready-to-use plots for scientific papers.
It works by gradually adding and adjusting plot components and provides
full control over composition, style, and absolute sizes. tidyplots is
powered by the pipe `%>%` and provides a clean and minimalist interface
for commonly used scientific plotting routines.

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

df_demo %>% 
  tidyplot(x = category, y = value, color = category) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_error() %>% 
  add_jitter()
```

<img src="man/figures/README-example-1.png" style="display: block; margin: auto;" />

## Learn more

<https://jbengler.github.io/tidyplots/>

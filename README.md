
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplots <a href="https://jbengler.github.io/tidyplots/"><img src="man/figures/logo.svg" align="right" height="139" alt="tidyplots website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbengler/tidyplots/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbengler/tidyplots/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `tidyplots` is to streamline the creation of
publication-ready plots for scientific papers. It allows to gradually
add and refine plot elements, and provides precise control over
composition, style, and absolute sizes. By consistent use of the pipe
`%>%` it simplifies the construction of advanced plotting pipelines.

## Disclaimer

This package is still in early development. Expect user-facing and
breaking changes. I would not recommend to use this package in
production yet.

Feel free to report bugs and suggest features
[here](https://github.com/jbengler/tidyplots/issues).

## Installation

``` r
# install.packages("devtools")
devtools::install_github("jbengler/tidyplots")
```

## Usage

Here are some examples. The full documentation can be found
[here](https://jbengler.github.io/tidyplots/).

``` r
library(tidyplots)

study %>% 
  tidyplot(x = treatment, y = score, color = treatment) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_error_bar() %>% 
  add_data_points_beeswarm()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
energy %>% 
  tidyplot(year, power, color = energy_source) %>% 
  add_barstack_absolute()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
energy %>% 
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) %>% 
  tidyplot(y = power, color = energy_source) %>% 
  add_donut() %>% 
  split_plot(by = year)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
energy_week %>% 
  tidyplot(date, power, color = energy_source) %>% 
  add_areastack_absolute()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
energy_week %>% 
  tidyplot(date, power, color = energy_source) %>% 
  add_areastack_relative()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
study %>% 
  tidyplot(x = group, y = score, color = dose) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_mean_dash() %>% 
  add_mean_value()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
time_course %>%
  tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
  add_mean_line() %>%
  add_mean_dot() %>%
  add_error_ribbon()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
study %>% 
  tidyplot(x = treatment, y = score, color = treatment) %>% 
  add_boxplot() %>% 
  add_stats_pvalue(ref.group = 1)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r
gene_expression %>% 
  dplyr::filter(external_gene_name %in% c("Apol6", "Col5a3", "Vgf", "Bsn")) %>% 
  tidyplot(x = condition, y = expression, color = sample_type) %>% 
  add_mean_dash() %>% 
  add_error_bar() %>% 
  add_data_points_beeswarm() %>% 
  add_stats_asterisks(include_info = FALSE) %>% 
  remove_x_axis_title() %>% 
  split_plot(by = external_gene_name)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
study %>% 
  tidyplot(x = treatment, y = score, color = treatment) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_error_bar() %>% 
  add_data_points_beeswarm() %>% 
  view_plot(title = "Default color scheme: metro") %>% 
  adjust_colors(colors_discrete_candy) %>% 
  view_plot(title = "Alternative color scheme: candy") %>% 
  adjust_colors(colors_discrete_seaside) %>% 
  view_plot(title = "Alternative color scheme: seaside")
```

<img src="man/figures/README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-11-2.png" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-11-3.png" style="display: block; margin: auto;" />

## Acknowledgements

tidyplots relies on a number of fantastic packages that do all the heavy
lifting behind the scenes. These include cli, dplyr, forcats, ggplot2,
ggpubr, ggrastr, ggrepel, glue, Hmisc, patchwork, purrr, ragg, rlang,
scales, stringr, svglite and tidyr.

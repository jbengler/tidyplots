
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplots <a href="https://jbengler.github.io/tidyplots/"><img src="man/figures/logo.svg" align="right" height="139" alt="tidyplots website" /></a>

<!-- badges: start -->
<!-- badges: end -->

tidyplots streamlines the creation of publication-ready plots for
scientific papers, making it incredibly easy to incorporate and refine
plot elements. It allows precise control over composition, style, and
absolute sizes, while its utilization of the pipe `%>%` simplifies the
construction of powerful plotting pipelines.

## Installation

``` r
# install.packages("pak")
pak::pak("jbengler/tidyplots")
```

## Usage

Here are some examples of what you can do with just a few key strokes.
The full documentation can be found
[here](https://jbengler.github.io/tidyplots/).

``` r
library(tidyplots)

study %>% 
  tidyplot(x = treatment, y = score, color = treatment) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_error() %>% 
  add_jitter()
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
  add_box() %>% 
  add_stats_pvalue(ref.group = 1)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r
gene_expression %>% 
  dplyr::filter(external_gene_name %in% c("Apol6", "Col5a3", "Vgf", "Bsn")) %>% 
  tidyplot(x = condition, y = expression, color = sample_type) %>% 
  add_mean_dash() %>% 
  add_error() %>% 
  add_jitter() %>% 
  add_stats_pvalue(include_info = FALSE) %>% 
  adjust_x_axis(title = "") %>% 
  split_plot(by = external_gene_name)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

## Acknowledgements

tidyplots relies on a number of fantastic packages that do all the heavy
lifting behind the scenes. These include cli, dplyr, forcats, ggplot2,
ggpubr, ggrastr, ggrepel, glue, Hmisc, patchwork, purrr, ragg, rlang,
scales, stringr, svglite and tidyr.

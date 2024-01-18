
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

This is a basic example which shows you how to create a simple plot.

``` r
library(tidyplots)

study %>% 
  tidyplot(x = treatment, y = score, color = treatment) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_error() %>% 
  add_jitter() %>% 
  save_plot("my_plot.pdf")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

## Documentation

<https://jbengler.github.io/tidyplots/>

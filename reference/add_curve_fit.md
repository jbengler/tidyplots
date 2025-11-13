# Add curve fit

Add curve fit

## Usage

``` r
add_curve_fit(
  plot,
  dodge_width = NULL,
  method = "loess",
  linewidth = 0.25,
  alpha = 0.4,
  preserve = "total",
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- dodge_width:

  For adjusting the distance between grouped objects. Defaults to `0.8`
  for plots with at least one discrete axis and `0` for plots with two
  continuous axes.

- method:

  Smoothing method (function) to use, accepts either `NULL` or a
  character vector, e.g. `"lm"`, `"glm"`, `"gam"`, `"loess"` or a
  function, e.g. [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html) or
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`stats::lm`](https://rdrr.io/r/stats/lm.html), or
  [`stats::loess`](https://rdrr.io/r/stats/loess.html). `"auto"` is also
  accepted for backwards compatibility. It is equivalent to `NULL`.

  For `method = NULL` the smoothing method is chosen based on the size
  of the largest group (across all panels).
  [`stats::loess()`](https://rdrr.io/r/stats/loess.html) is used for
  less than 1,000 observations; otherwise
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) is used with
  `formula = y ~ s(x, bs = "cs")` with `method = "REML"`. Somewhat
  anecdotally, `loess` gives a better appearance, but is \\O(N^{2})\\ in
  memory, so does not work for larger datasets.

  If you have fewer than 1,000 observations but want to use the same
  `gam()` model that `method = NULL` would use, then set
  `method = "gam", formula = y ~ s(x, bs = "cs")`.

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- alpha:

  A `number` between `0` and `1` for the opacity of an object. A value
  of `0` is completely transparent, `1` is completely opaque.

- preserve:

  Should dodging preserve the `"total"` width of all elements at a
  position, or the width of a `"single"` element?

- ...:

  Arguments passed on to
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html).

## Value

A `tidyplot` object.

## Examples

``` r
time_course |>
  tidyplot(x = day, y = score, color = treatment) |>
  add_curve_fit()
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Removed 170 rows containing non-finite outside the scale range
#> (`stat_smooth()`).


# Changing arguments
time_course |>
  tidyplot(x = day, y = score, color = treatment) |>
  add_curve_fit(linewidth = 1)
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Removed 170 rows containing non-finite outside the scale range
#> (`stat_smooth()`).


time_course |>
  tidyplot(x = day, y = score, color = treatment) |>
  add_curve_fit(alpha = 0.8)
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Removed 170 rows containing non-finite outside the scale range
#> (`stat_smooth()`).


# Remove confidence interval
time_course |>
  tidyplot(x = day, y = score, color = treatment) |>
  add_curve_fit(se = FALSE)
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Removed 170 rows containing non-finite outside the scale range
#> (`stat_smooth()`).

```

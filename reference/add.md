# Add ggplot2 code to a tidyplot

Add ggplot2 code to a tidyplot

## Usage

``` r
add()
```

## Value

A `tidyplot` object.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add(ggplot2::geom_point())

```

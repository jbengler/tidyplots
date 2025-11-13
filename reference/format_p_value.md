# Format p values

Format p values

## Usage

``` r
format_p_value(x, accuracy = 1e-04)
```

## Arguments

- x:

  A `number` to format.

- accuracy:

  A number to round to. For example, use `0.01` to show 2 decimal places
  of precision. Defaults to `0.0001`, corresponding to 4 decimal places
  of precision.

## Value

Formatted number as `character` string.

## Examples

``` r
format_p_value(0.03445553)
#> [1] "0.0345"
format_p_value(0.0003445553)
#> [1] "0.0003"
format_p_value(0.00003445553)
#> [1] "< 0.0001"
```

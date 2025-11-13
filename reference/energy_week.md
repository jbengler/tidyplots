# Energy week data

Energy week data

## Usage

``` r
energy_week
```

## Format

A data frame.

## Source

[Energy-Charts](https://www.energy-charts.info), Energy production data,
Germany

## Examples

``` r
dplyr::glimpse(energy_week)
#> Rows: 10,080
#> Columns: 5
#> $ date          <dttm> 2023-09-03 22:00:00, 2023-09-03 22:00:00, 2023-09-03 22…
#> $ energy_source <fct> Nuclear, Hydro Run-of-River, Biomass, Fossil brown coal …
#> $ energy_type   <fct> Nuclear, Renewable, Renewable, Fossil, Fossil, Fossil, F…
#> $ power         <dbl> 0.0, 2634.3, 4710.9, 8399.1, 1725.6, 400.7, 4900.3, 17.9…
#> $ power_unit    <chr> "MW", "MW", "MW", "MW", "MW", "MW", "MW", "MW", "MW", "M…
```

# Energy data

Energy data

## Usage

``` r
energy
```

## Format

A data frame.

## Source

[Energy-Charts](https://www.energy-charts.info), Energy production data,
Germany

## Examples

``` r
dplyr::glimpse(energy)
#> Rows: 344
#> Columns: 5
#> $ year          <dbl> 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 20…
#> $ energy_source <fct> Biomass, Fossil brown coal / lignite, Fossil gas, Fossil…
#> $ energy_type   <fct> Renewable, Fossil, Fossil, Fossil, Fossil, Renewable, Re…
#> $ energy        <dbl> 3.723, 140.544, 39.983, 111.427, 1.755, 0.000, 23.377, 1…
#> $ energy_unit   <chr> "TWh", "TWh", "TWh", "TWh", "TWh", "TWh", "TWh", "TWh", …
```

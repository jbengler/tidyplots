# Principle component analysis data

Principle component analysis data

## Usage

``` r
pca
```

## Format

A data frame.

## Source

[Bassoon proteinopathy drives neurodegeneration in multiple
sclerosis](https://www.nature.com/articles/s41593-019-0385-4), Nature
Neuroscience 2019

## Examples

``` r
dplyr::glimpse(pca)
#> Rows: 20
#> Columns: 4
#> $ pc1    <dbl> -10.8006941, -11.4517496, -10.2164780, -8.1750841, -10.6788845,…
#> $ pc2    <dbl> -9.1734842, -7.8359546, -9.1090846, -7.2980632, -8.1548689, 3.7…
#> $ sample <chr> "Hin_1", "Hin_2", "Hin_3", "Hin_4", "Hin_5", "Ein_1", "Ein_2", …
#> $ group  <chr> "Hin", "Hin", "Hin", "Hin", "Hin", "Ein", "Ein", "Ein", "Ein", …
```

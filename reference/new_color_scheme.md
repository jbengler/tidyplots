# New color scheme

For more information about the use of color schemes in tidyplots, check
out this article: [Color
schemes](https://jbengler.github.io/tidyplots/articles/Color-schemes.html)

## Usage

``` r
new_color_scheme(x, name = "Untitled color scheme", reverse = FALSE)
```

## Arguments

- x:

  Character vector of hex colors. For example
  `x = c("#FF00FF", "#00FFFF")`.

- name:

  Name of the custom color scheme.

- reverse:

  Whether the order should be reversed or not. Defaults to `FALSE`,
  meaning not reversed.

## Value

A `tidyplot` object.

## Examples

``` r
new_color_scheme(c("#ECA669","#E06681","#8087E2","#E2D269"))
#> [1] "#ECA669" "#E06681" "#8087E2" "#E2D269"

new_color_scheme(c("#ECA669","#E06681","#8087E2","#E2D269"),
  name = "my_custom_color_scheme")
#> [1] "#ECA669" "#E06681" "#8087E2" "#E2D269"
```

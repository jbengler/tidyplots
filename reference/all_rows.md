# Subset data rows

Subset data rows

## Usage

``` r
all_rows()

filter_rows(..., .by = NULL)

max_rows(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE)

min_rows(order_by, n, by = NULL, with_ties = TRUE, na_rm = FALSE)

first_rows(n, by = NULL)

last_rows(n, by = NULL)

sample_rows(n, by = NULL)
```

## Arguments

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical value, and are defined in terms of
  the variables in `.data`. If multiple expressions are included, they
  are combined with the `&` operator. Only rows for which all conditions
  evaluate to `TRUE` are kept.

- .by, by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- order_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variable or function of variables to order by. To order by multiple
  variables, wrap them in a data frame or tibble.

- n:

  The number of rows to select. If not are supplied, `n = 1` will be
  used. If `n` is greater than the number of rows in the group, the
  result will be silently truncated to the group size.

  A negative value of `n` will be subtracted from the group size. For
  example, `n = -2` with a group of 5 rows will select 5 - 2 = 3 rows.

- with_ties:

  Should ties be kept together? The default, `TRUE`, may return more
  rows than you request. Use `FALSE` to ignore ties, and return the
  first `n` rows.

- na_rm:

  Should missing values in `order_by` be removed from the result? If
  `FALSE`, `NA` values are sorted to the end (like in
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)),
  so they will only be included if there are insufficient non-missing
  values to reach `n`.

## Value

A `function` to achieve the desired data subsetting.

## Examples

``` r
# Highlight all animals
animals |>
 tidyplot(x = weight, y = size) |>
 add_data_points() |>
 add_data_points(data = all_rows(),
  color = "red", shape = 1, size = 3)


# Highlight 3 animals with the highest weight
animals |>
 tidyplot(x = weight, y = size) |>
 add_data_points() |>
 add_data_points(data = max_rows(weight, n = 3),
  color = "red", shape = 1, size = 3)


# Highlight 3 animals with the lowest weight
animals |>
 tidyplot(x = weight, y = size) |>
 add_data_points() |>
 add_data_points(data = min_rows(weight, n = 3),
  color = "red", shape = 1, size = 3)


# Highlight the first 3 animals in the dataset
animals |>
 tidyplot(x = weight, y = size) |>
 add_data_points() |>
 add_data_points(data = first_rows(n = 3),
  color = "red", shape = 1, size = 3)


# Highlight the last 3 animals in the dataset
animals |>
 tidyplot(x = weight, y = size) |>
 add_data_points() |>
 add_data_points(data = last_rows(n = 3),
  color = "red", shape = 1, size = 3)


# Highlight 3 random animals
animals |>
 tidyplot(x = weight, y = size) |>
 add_data_points() |>
 add_data_points(data = sample_rows(n = 3),
  color = "red", shape = 1, size = 3)

```

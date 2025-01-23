test_that("absolute barstacks work", {
  animals |>
    tidyplot(color = family) |>
    add_barstack_absolute() |>
    vdiffr::expect_doppelganger("absolute barstack no x", fig = _)

  animals |>
    tidyplot(x = diet, color = family) |>
    add_barstack_absolute() |>
    vdiffr::expect_doppelganger("absolute barstack x", fig = _)

  animals |>
    tidyplot(y = diet, color = family) |>
    add_barstack_absolute() |>
    vdiffr::expect_doppelganger("absolute barstack y", fig = _)

  animals |>
    tidyplot(x = diet, y = speed, color = family) |>
    add_barstack_absolute() |>
    vdiffr::expect_doppelganger("absolute barstack xy", fig = _)

  animals |>
    tidyplot(y = diet, x = speed, color = family) |>
    add_barstack_absolute() |>
    vdiffr::expect_doppelganger("absolute barstack yx", fig = _)
})

test_that("relative barstacks work", {
  animals |>
    tidyplot(color = family) |>
    add_barstack_relative() |>
    vdiffr::expect_doppelganger("relative barstack no x", fig = _)

  animals |>
    tidyplot(x = diet, color = family) |>
    add_barstack_relative() |>
    vdiffr::expect_doppelganger("relative barstack x", fig = _)

  animals |>
    tidyplot(y = diet, color = family) |>
    add_barstack_relative() |>
    vdiffr::expect_doppelganger("relative barstack y", fig = _)

  animals |>
    tidyplot(x = diet, y = speed, color = family) |>
    add_barstack_relative() |>
    vdiffr::expect_doppelganger("relative barstack xy", fig = _)

  animals |>
    tidyplot(y = diet, x = speed, color = family) |>
    add_barstack_relative() |>
    vdiffr::expect_doppelganger("relative barstack yx", fig = _)
})

test_that("absolute areastacks work", {
  animals |>
    tidyplot(x = diet, color = family) |>
    add_areastack_absolute() |>
    vdiffr::expect_doppelganger("absolute areastack x", fig = _)

  animals |>
    tidyplot(y = diet, color = family) |>
    add_areastack_absolute() |>
    vdiffr::expect_doppelganger("absolute areastack y", fig = _)

  animals |>
    tidyplot(x = diet, y = speed, color = family) |>
    add_areastack_absolute() |>
    vdiffr::expect_doppelganger("absolute areastack xy", fig = _)

  animals |>
    tidyplot(y = diet, x = speed, color = family) |>
    add_areastack_absolute() |>
    vdiffr::expect_doppelganger("absolute areastack yx", fig = _)
})

test_that("relative areastacks work", {
  animals |>
    tidyplot(x = diet, color = family) |>
    add_areastack_relative() |>
    vdiffr::expect_doppelganger("relative areastack x", fig = _)

  animals |>
    tidyplot(y = diet, color = family) |>
    add_areastack_relative() |>
    vdiffr::expect_doppelganger("relative areastack y", fig = _)

  animals |>
    tidyplot(x = diet, y = speed, color = family) |>
    add_areastack_relative() |>
    vdiffr::expect_doppelganger("relative areastack xy", fig = _)

  animals |>
    tidyplot(y = diet, x = speed, color = family) |>
    add_areastack_relative() |>
    vdiffr::expect_doppelganger("relative areastack yx", fig = _)
})

test_that("barstacks with date work", {
spendings |>
  tidyplot(y = date, color = category) |>
  add_barstack_absolute() |>
    vdiffr::expect_doppelganger("barstack with date y", fig = _)

spendings |>
  tidyplot(x = date, color = category) |>
  add_barstack_absolute() |>
  vdiffr::expect_doppelganger("barstack with date x", fig = _)

spendings |>
  tidyplot(x = date, y = amount, color = category) |>
  add_barstack_absolute() |>
  vdiffr::expect_doppelganger("barstack with date xy", fig = _)

spendings |>
  tidyplot(x = amount, y = date, color = category) |>
  add_barstack_absolute() |>
    vdiffr::expect_doppelganger("barstack with date yx", fig = _)
})

test_that("absolute areastacks with replace_na work", {
  animals |>
    tidyplot(x = diet, color = family) |>
    add_areastack_absolute(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("absolute areastack replace_na x", fig = _)

  animals |>
    tidyplot(y = diet, color = family) |>
    add_areastack_absolute(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("absolute areastack replace_na y", fig = _)

  animals |>
    tidyplot(x = diet, y = speed, color = family) |>
    add_areastack_absolute(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("absolute areastack replace_na xy", fig = _)

  animals |>
    tidyplot(y = diet, x = speed, color = family) |>
    add_areastack_absolute(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("absolute areastack replace_na yx", fig = _)
})

test_that("relative areastacks with replace_na work", {
  animals |>
    tidyplot(x = diet, color = family) |>
    add_areastack_relative(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("relative areastack replace_na x", fig = _)

  animals |>
    tidyplot(y = diet, color = family) |>
    add_areastack_relative(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("relative areastack replace_na y", fig = _)

  animals |>
    tidyplot(x = diet, y = speed, color = family) |>
    add_areastack_relative(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("relative areastack replace_na xy", fig = _)

  animals |>
    tidyplot(y = diet, x = speed, color = family) |>
    add_areastack_relative(replace_na = TRUE) |>
    vdiffr::expect_doppelganger("relative areastack replace_na yx", fig = _)
})

test_that("pie plots work", {
  animals |>
    tidyplot(color = diet) |>
    add_pie() |>
    vdiffr::expect_doppelganger("pie count 1", fig = _)

  animals |>
    tidyplot(color = activity) |>
    add_pie() |>
    vdiffr::expect_doppelganger("pie count 2", fig = _)

  animals |>
    tidyplot(color = number_of_legs) |>
    add_pie() |>
    vdiffr::expect_doppelganger("pie count 3", fig = _)

  spendings |>
    tidyplot(color = category) |>
    add_pie() |>
    vdiffr::expect_doppelganger("pie count 4", fig = _)

  spendings |>
    tidyplot(y = amount, color = category) |>
    add_pie() |>
    vdiffr::expect_doppelganger("pie sum", fig = _)
})

test_that("donut plots work", {
  animals |>
    tidyplot(color = diet) |>
    add_donut() |>
    vdiffr::expect_doppelganger("donut count 1", fig = _)

  animals |>
    tidyplot(color = activity) |>
    add_donut() |>
    vdiffr::expect_doppelganger("donut count 2", fig = _)

  animals |>
    tidyplot(color = number_of_legs) |>
    add_donut() |>
    vdiffr::expect_doppelganger("donut count 3", fig = _)

  spendings |>
    tidyplot(color = category) |>
    add_donut() |>
    vdiffr::expect_doppelganger("donut count 4", fig = _)

  spendings |>
    tidyplot(y = amount, color = category) |>
    add_donut() |>
    vdiffr::expect_doppelganger("donut sum", fig = _)
})

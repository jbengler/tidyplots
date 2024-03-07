test_that("is_hex_vector works", {
  expect_equal(is_hex_vector(c(2,3,3)), FALSE)
  expect_equal(is_hex_vector(c("jdj","koi", "klk")), FALSE)
  expect_equal(is_hex_vector(c("#jdj","#koi", "klk")), FALSE)
  expect_equal(is_hex_vector(c("abc","094", "aff")), FALSE)
  expect_equal(is_hex_vector(c("#jdj","#koi", "#klk")), FALSE)
  expect_equal(is_hex_vector(c("#889098","#89f987", "0099ff")), FALSE)
  expect_equal(is_hex_vector(c("#889098","#89f987", "#ff99ff")), TRUE)
  expect_equal(is_hex_vector(c("#889098","#89f987", "#ff99ffff")), TRUE)
  expect_equal(is_hex_vector(c("#889098","#89f987", "#ff99ffff0")), FALSE)
  expect_equal(is_hex_vector(c("#889098","#89f987", "#fff")), TRUE)
})

test_that("check_input works", {
  p1 <-
    study %>%
    ggplot2::ggplot(ggplot2::aes(treatment, score, color = treatment)) +
    ggplot2::geom_point()
  tp <-
    study %>%
    tidyplot(treatment, score, color = treatment) %>%
    add_data_points_beeswarm()
  p2 <- p1
  pw <- patchwork::wrap_plots(p1, p2)
  gg_list <- list(p1, p2)
  pw_list <- list(pw, pw)
  tp_list <- list(tp, tp)
  expect_equal(check_input(p1), "gg")
  expect_equal(check_input(pw), "pw")
  expect_equal(check_input(tp), "tp")
  expect_equal(check_input(gg_list), "gg_list")
  expect_equal(check_input(pw_list), "pw_list")
  expect_equal(check_input(tp_list), "tp_list")
  expect_equal(check_input(c("hello")), "none")
})


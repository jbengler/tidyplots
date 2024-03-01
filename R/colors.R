#' @export
print.tidycolor <- function(x, return_html = FALSE, ...) print_tidycolor(x, return_html = return_html)
#' @export
c.tidycolor <- function(...) new_color_scheme(NextMethod())
#' @export
`[.tidycolor` <- function(x, i) new_color_scheme(NextMethod())
#' @export
`[[.tidycolor` <- function(x, i) new_color_scheme(NextMethod())

#' Adjust colors
#' @param gg bla
#' @param new_colors bla
#' @param saturation bla
#' @param labels bla
#' @param ... bla
#' @export
adjust_colors <- function(gg, new_colors = NULL,
                          saturation = 1,
                          labels = tidyplot_parse_labels(), ...) {
  out <- gg
  as_palette <- FALSE

  if (is_discrete(gg, "colour")) {

    # Default colors
    if (is.null(new_colors)) {
      new_colors <- colors_discrete_metro
      as_palette <- TRUE
    }

    # Are enough new_colors provided?
    named_vector <- FALSE
    n_ratio <- 0
    if (!is.null(names(new_colors))) {
      named_vector <- TRUE
      n_ratio <- 1
    } else {
      n_provided <- length(new_colors)
      n_requested <-
        dplyr::pull(gg$data, get_variable(gg, "colour")) %>%
        unique() %>%
        length
      n_ratio <- n_provided / n_requested
    }

    if (named_vector || !as_palette && n_ratio >= 1) {

      # Too many colors
      if (n_ratio > 1 && !as_palette) {
        cli::cli_alert_danger("adjust_colors: Too many colors. {n_provided} colors provided, but only {n_requested} needed.")
        new_colors <- downsample_vector(new_colors, n_requested)
      }

      suppressMessages(out <- out + ggplot2::scale_color_manual(values = new_colors, drop = FALSE, labels = labels, ...))
      suppressMessages(out <- out + ggplot2::scale_fill_manual(values = apply_saturation(new_colors, saturation = saturation), drop = FALSE, labels = labels, ...))
      cli::cli_alert_success("adjust_colors: applied discrete {.pkg color values}")

    } else {
      suppressMessages(out <- out + scale_color_d(palette = new_colors, drop = FALSE, labels = labels, ...))
      suppressMessages(out <- out + scale_fill_d(palette = new_colors, saturation = saturation, drop = FALSE, labels = labels, ...))
      cli::cli_alert_success("adjust_colors: applied discrete {.pkg color palette}")

      # Too few colors
      if (n_ratio < 1 && !as_palette) cli::cli_alert_danger("adjust_colors: Too few colors. {n_provided} colors provided, but {n_requested} expected.")
    }
  }

  if (is_continuous(gg, "colour")) {
    # Default colors
    if (is.null(new_colors)) new_colors <- colors_continuous_blue_pink_yellow

    suppressMessages(out <- out + scale_color_c(palette = new_colors, labels = labels, ...))
    suppressMessages(out <- out + scale_fill_c(palette = new_colors, saturation = saturation, labels = labels, ...))
    cli::cli_alert_success("adjust_colors: applied continous {.pkg color palette}")
  }
  out
}

#' New color scheme
#'
#' @param x bla
#' @param name bla
#' @export
new_color_scheme <- function(x, name = "Untitled color scheme") {
  x <- as.character(x)
  structure(x, class = c('tidycolor', 'character'), tidycolor.name = name)
}


#' Discrete color schemes
#'
#' @details
#' Previews of build-in discrete color schemes
#'
#' ```{r results="asis", echo=FALSE, message=FALSE}
#' print(colors_discrete_metro, return_html = TRUE)
#' print(colors_discrete_circle, return_html = TRUE)
#' print(colors_discrete_candy, return_html = TRUE)
#' print(colors_discrete_pastel, return_html = TRUE)
#' print(colors_discrete_seaside, return_html = TRUE)
#' ```
#' @md
#'
#' @export
colors_discrete_metro <- new_color_scheme(color_palettes$metro, "colors_discrete_metro")
#' @rdname colors_discrete_metro
#' @export
colors_discrete_circle <- new_color_scheme(color_palettes$color_circle_vivid, "colors_discrete_circle")
#' @rdname colors_discrete_metro
#' @export
colors_discrete_candy <- new_color_scheme(c("#9b5de5", "#f15bb5", "#fee440", "#00bbf9", "#00f5d4"), "colors_discrete_candy")
#' @rdname colors_discrete_metro
#' @export
colors_discrete_pastel <- new_color_scheme(c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff"), "colors_discrete_pastel")
#' @rdname colors_discrete_metro
#' @export
colors_discrete_seaside <- new_color_scheme(c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500"), "colors_discrete_seaside")


#' Continuous color schemes
#'
#' @details
#' Previews of build-in continuous color schemes
#'
#' ```{r results="asis", echo=FALSE, message=FALSE}
#' print(colors_continuous_viridis, return_html = TRUE)
#' print(colors_continuous_magma, return_html = TRUE)
#' print(colors_continuous_inferno, return_html = TRUE)
#' print(colors_continuous_plasma, return_html = TRUE)
#' print(colors_continuous_cividis, return_html = TRUE)
#' print(colors_continuous_rocket, return_html = TRUE)
#' print(colors_continuous_mako, return_html = TRUE)
#' print(colors_continuous_turbo, return_html = TRUE)
#' print(colors_continuous_blue_pink_yellow, return_html = TRUE)
#' ```
#' @md
#'
#' @export
colors_continuous_viridis <- new_color_scheme(viridisLite::viridis(265), name = "colors_continuous_viridis")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_magma <- new_color_scheme(viridisLite::magma(265), "colors_continuous_magma")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_inferno <- new_color_scheme(viridisLite::inferno(265),"colors_continuous_inferno")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_plasma <- new_color_scheme(viridisLite::plasma(265), "colors_continuous_plasma")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_cividis <- new_color_scheme(viridisLite::cividis(265), "colors_continuous_cividis")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_rocket <- new_color_scheme(viridisLite::rocket(265), "colors_continuous_rocket")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_mako <- new_color_scheme(viridisLite::mako(265), "colors_continuous_mako")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_turbo <- new_color_scheme(viridisLite::turbo(265), "colors_continuous_turbo")
#' @rdname colors_continuous_viridis
#' @export
colors_continuous_blue_pink_yellow <- new_color_scheme(color_palettes$blue_pink_yellow, "colors_continuous_blue_pink_yellow")


#' Diverging color schemes
#'
#' @details
#' Previews of build-in diverging color schemes
#'
#' ```{r results="asis", echo=FALSE, message=FALSE}
#' print(colors_diverging_blue2brown, return_html = TRUE)
#' print(colors_diverging_blue2red, return_html = TRUE)
#' print(colors_diverging_RdBu, return_html = TRUE)
#' print(colors_diverging_RdYlBu, return_html = TRUE)
#' ```
#' @md
#'
#' @export
colors_diverging_blue2brown <- new_color_scheme(color_palettes$blue2brown, "colors_diverging_blue2brown")
#' @rdname colors_diverging_blue2brown
#' @export
colors_diverging_blue2red <- new_color_scheme(c("#0000FF","#FFFFFF","#FF0000"), "colors_diverging_blue2red")
#' @rdname colors_diverging_blue2brown
#' @export
colors_diverging_RdBu <- new_color_scheme(RColorBrewer::brewer.pal("RdBu", n = 11), "colors_diverging_RdBu")
#' @rdname colors_diverging_blue2brown
#' @export
colors_diverging_RdYlBu <- new_color_scheme(RColorBrewer::brewer.pal("RdYlBu", n = 9), "colors_diverging_RdYlBu")


# not exported
downsample_vector <- function(x, n) {
  if (length(x) <= n) return(x)
  by <- (length(x) / (n-1)) - (1 / (n-1))
  i <- round(cumsum(c(1, rep(by, n-1))))
  x[i]
}

apply_saturation <- function(colors, saturation, background_color = "#FFFFFF") {
  purrr::map_chr(colors, function(color) {
    color <- col2rgb(color)
    background_color <- col2rgb(background_color)
    new_color <- (1-saturation) * background_color + saturation * color
    rgb(t(new_color), maxColorValue = 255)
  })
}

generate_html <- function(x, max_colors = 64) {
  name <- attr(x, "tidycolor.name")
  size <- length(x)
  downsampled <- FALSE
  downsample_text <- ""

  if (length(x) > max_colors) {
    downsampled <- TRUE
    downsample_text <- paste0(", downsampled to ",max_colors," colors")
    x <- downsample_vector(x, max_colors)
  }

  color_bar <- color_vector <- paste0("<span style='background-color:",x,"; display: inline-block; width:30px; height:20px;'></span>", collapse = "")
  color_vector <- paste0("<span style='background-color:",x,"; display: inline-block; padding: 2px 10px 2px 10px;'>\"",x,"\"</span>", collapse = ",")

  paste0("
  <div style='margin-bottom: 80px;'>
  <h4 style=\"font-family: 'Courier New', monospace;\">", name, "</h4>
  <small style=\"font-family: 'Courier New', monospace;\">A tidyplots color scheme with ",size," colors",downsample_text,".</small>
  <p>
  ",color_bar,"
  <p><p style=\"font-family: 'Courier New', monospace;\">
  c(<br>", color_vector ,")
  </div>")
}

print_tidycolor <- function(x, return_html = FALSE, max_colors = 64) {
  cli::cli_alert_info(paste0("A tidyplots color scheme with ",length(x) ," colors."))

  viewer <- getOption("viewer")
  if (!is.null(viewer) || return_html) {

    inner_html <- generate_html(x = x, max_colors = max_colors)
    if (return_html)
      return(htmltools::HTML(inner_html))

    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")
    html <- paste0("
    <html><body>", inner_html, "</body></html>")

    writeLines(html, htmlFile)
    viewer(htmlFile)
    cli::cli_alert_success("A preview was send to the RStudio viewer pane.")
  } else {
    print(as.character(x))
  }
}

# palette and scale functions
make_palette <- function(palette, reverse = FALSE, saturation = 1, ...) {
  pal <- palette
  if (reverse) pal <- rev(pal)
  pal <- apply_saturation(pal, saturation = saturation)
  grDevices::colorRampPalette(pal, ...)
}

scale_color_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
}

scale_fill_d <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
}

scale_color_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

scale_fill_c <- function(palette = NULL, saturation = 1, reverse = FALSE, ...) {
  pal <- make_palette(palette = palette, saturation = saturation, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

#' @export
print.tidycolor <- function(x, max_colors = 42, return_html = FALSE, ...) print_tidycolor(x, max_colors = max_colors, return_html = return_html)
#' @export
c.tidycolor <- function(...) new_color_scheme(NextMethod())
#' @export
`[.tidycolor` <- function(x, i) new_color_scheme(NextMethod())
#' @export
`[[.tidycolor` <- function(x, i) new_color_scheme(NextMethod())

#' New color scheme
#'
#' @param x bla
#' @param name bla
#' @param reverse bla
#' @export
new_color_scheme <- function(x, name = "Untitled color scheme", reverse = FALSE) {
  if (!is_hex_vector(x)) cli::cli_abort("{.arg x} must be a vector of hex colors.")
  if (reverse) x <- rev(x)
  structure(x, class = c('tidycolor', 'character'), tidycolor.name = name)
}


#' Discrete color schemes
#'
#' @details
#' Color schemes can be conveniently previewed by using the print method of the
#' `tidycolor` class. This will send a html preview to the RStudio Viewer pane
#' that looks like this:
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
#' Color schemes can be conveniently previewed by using the print method of the
#' `tidycolor` class. This will send a html preview to the RStudio Viewer pane
#' that looks like this:
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
#' print(colors_continuous_bluepinkyellow, return_html = TRUE)
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
colors_continuous_bluepinkyellow <- new_color_scheme(color_palettes$blue_pink_yellow, "colors_continuous_bluepinkyellow")


#' Diverging color schemes
#'
#' @details
#' Color schemes can be conveniently previewed by using the print method of the
#' `tidycolor` class. This will send a html preview to the RStudio Viewer pane
#' that looks like this:
#'
#' ```{r results="asis", echo=FALSE, message=FALSE}
#' print(colors_diverging_blue2brown, return_html = TRUE)
#' print(colors_diverging_blue2red, return_html = TRUE)
#' print(colors_diverging_BuRd, return_html = TRUE)
#' print(colors_diverging_BuYlRd, return_html = TRUE)
#' ```
#' @md
#'
#' @export
colors_diverging_blue2brown <- new_color_scheme(color_palettes$blue2brown, "colors_diverging_blue2brown")
#' @rdname colors_diverging_blue2brown
#' @export
colors_diverging_blue2red <- new_color_scheme(grDevices::colorRampPalette(c("#0000FF","#FFFFFF","#FF0000"))(9), "colors_diverging_blue2red")
#' @rdname colors_diverging_blue2brown
#' @export
colors_diverging_BuRd <- new_color_scheme(RColorBrewer::brewer.pal("RdBu", n = 9), "colors_diverging_BuRd", reverse = TRUE)
#' @rdname colors_diverging_blue2brown
#' @export
colors_diverging_BuYlRd <- new_color_scheme(RColorBrewer::brewer.pal("RdYlBu", n = 9), "colors_diverging_BuYlRd", reverse = TRUE)


# not exported
downsample_vector <- function(x, n) {
  if (length(x) <= n) return(x)
  by <- (length(x) / (n-1)) - (1 / (n-1))
  i <- floor(cumsum(c(1, rep(by, n-1))))
  x[i]
}

generate_html <- function(x, max_colors) {
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

print_tidycolor <- function(x, max_colors, return_html = FALSE) {
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

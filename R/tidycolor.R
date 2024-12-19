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
#' For more information about the use of color schemes in tidyplots, check out this article:
#' [Color schemes](https://jbengler.github.io/tidyplots/articles/Color-schemes.html)
#'
#' @param x Character vector of hex colors. For example `x = c("#FF00FF", "#00FFFF")`.
#' @param name Name of the custom color scheme.
#' @inherit common_arguments
#'
#' @examples
#' new_color_scheme(c("#ECA669","#E06681","#8087E2","#E2D269"))
#'
#' new_color_scheme(c("#ECA669","#E06681","#8087E2","#E2D269"),
#'   name = "my_custom_color_scheme")
#'
#' @export
new_color_scheme <- function(x, name = "Untitled color scheme", reverse = FALSE) {
  if (!is_hex_vector(x)) cli::cli_abort("{.arg x} must be a vector of hex colors.")
  if (reverse) x <- rev(x)
  structure(x, class = c('tidycolor', 'character'), tidycolor.name = name)
}


#' Discrete color schemes
#'
#' For more information about the use of color schemes in tidyplots, check out this article:
#' [Color schemes](https://jbengler.github.io/tidyplots/articles/Color-schemes.html)
#'
#' The signature theme of tidyplots `colors_discrete_friendly` was adapted from
#' the [Okabe & Ito](https://jfly.uni-koeln.de/color/) color palette that was designed
#' to work well for people with color vision deficiency.
#'
#' @details
#' Color schemes can be conveniently previewed by using the print method of the
#' `tidycolor` class. This will send a html preview to the RStudio Viewer pane.
#'
#' ```{r eval=FALSE}
#' colors_discrete_friendly
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_friendly, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_seaside
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_seaside, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_apple
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_apple, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_friendly_long
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_friendly_long, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_okabeito
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_okabeito, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_ibm
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_ibm, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_metro
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_metro, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_candy
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_candy, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_alger
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_alger, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_discrete_rainbow
#' ```
#' ```{r echo=FALSE}
#' print(colors_discrete_rainbow, return_html = TRUE)
#' ```
#'
#' @md
#' @export
colors_discrete_friendly <- new_color_scheme(c("#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00"), "colors_discrete_friendly")
# Bright yellow "#F0E442" replaced with darker yellow "#F5C710"
# https://easystats.github.io/see/reference/scale_color_okabeito.html
# downsample_vector(colors_discrete_friendly, 5)
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_seaside <- new_color_scheme(c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500"), "colors_discrete_seaside")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_apple <- new_color_scheme(c("#ff3b30", "#ff9500", "#ffcc00", "#4cd964", "#5ac8fa", "#007aff", "#5856d6"), "colors_discrete_apple")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_friendly_long <- new_color_scheme(c("#CC79A7", "#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00"), "colors_discrete_friendly_long")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_okabeito <- new_color_scheme(unname(grDevices::palette.colors()[c(-1, -9)]), "colors_discrete_okabeito")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_ibm <- new_color_scheme(c("#5B8DFE", "#725DEE", "#DD227D", "#FE5F00", "#FFB109"), "colors_discrete_ibm")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_metro <- new_color_scheme(c("#4DACD6","#4FAE62","#F6C54D","#E37D46","#C02D45"), "colors_discrete_metro")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_candy <- new_color_scheme(c("#9b5de5", "#f15bb5", "#fee440", "#00bbf9", "#00f5d4"), "colors_discrete_candy")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_alger <- new_color_scheme(c("#000000", "#1A5B5B", "#ACC8BE", "#F4AB5C", "#D1422F"), "colors_discrete_alger")
#' @rdname colors_discrete_friendly
#' @export
colors_discrete_rainbow <- new_color_scheme(c("#FF7777", "#FFAB74", "#FFE577", "#DBF47B", "#91E480", "#7CC9E5", "#7DA8E6", "#887DE6", "#BC7BE4"), "colors_discrete_rainbow")



#' Continuous color schemes
#'
#' For more information about the use of color schemes in tidyplots, check out this article:
#' [Color schemes](https://jbengler.github.io/tidyplots/articles/Color-schemes.html)
#'
#' @details
#' Color schemes can be conveniently previewed by using the print method of the
#' `tidycolor` class. This will send a html preview to the RStudio Viewer pane.
#'
#' ```{r eval=FALSE}
#' colors_continuous_viridis
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_viridis, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_magma
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_magma, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_inferno
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_inferno, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_plasma
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_plasma, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_cividis
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_cividis, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_rocket
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_rocket, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_mako
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_mako, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_turbo
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_turbo, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_continuous_bluepinkyellow
#' ```
#' ```{r echo=FALSE}
#' print(colors_continuous_bluepinkyellow, return_html = TRUE)
#' ```
#'
#' @md
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
colors_continuous_bluepinkyellow <- new_color_scheme(c(
  "#00034D","#000F9F","#001CEF","#241EF5","#5823F6","#A033E0","#E85AB1",
  "#F1907C","#F4AF63","#FCE552","#FFFB6D"), "colors_continuous_bluepinkyellow")


#' Diverging color schemes
#'
#' For more information about the use of color schemes in tidyplots, check out this article:
#' [Color schemes](https://jbengler.github.io/tidyplots/articles/Color-schemes.html)
#'
#' @details
#' Color schemes can be conveniently previewed by using the print method of the
#' `tidycolor` class. This will send a html preview to the RStudio Viewer pane.
#'
#' ```{r eval=FALSE}
#' colors_diverging_blue2red
#' ```
#' ```{r echo=FALSE}
#' print(colors_diverging_blue2red, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_diverging_blue2brown
#' ```
#' ```{r echo=FALSE}
#' print(colors_diverging_blue2brown, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_diverging_BuRd
#' ```
#' ```{r echo=FALSE}
#' print(colors_diverging_BuRd, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_diverging_BuYlRd
#' ```
#' ```{r echo=FALSE}
#' print(colors_diverging_BuYlRd, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_diverging_spectral
#' ```
#' ```{r echo=FALSE}
#' print(colors_diverging_spectral, return_html = TRUE)
#' ```
#' ```{r eval=FALSE}
#' colors_diverging_icefire
#' ```
#' ```{r echo=FALSE}
#' print(colors_diverging_icefire, return_html = TRUE)
#' ```
#'
#' @md
#' @export
colors_diverging_blue2red <- new_color_scheme(grDevices::colorRampPalette(c("#0000FF","#FFFFFF","#FF0000"))(17), "colors_diverging_blue2red")
#' @rdname colors_diverging_blue2red
#' @export
colors_diverging_blue2brown <- new_color_scheme(c(
  "#1961A5","#2671B5","#2D80BF","#268CC9","#119DD8","#00B2EB","#66C5EF",
  "#C4E5F8","#FEFCF6","#FDEEB8","#FCDD67","#F6C445","#E78B43","#DD5642",
  "#DB3E34","#CA3632","#B3322E"), "colors_diverging_blue2brown")
#' @rdname colors_diverging_blue2red
#' @export
colors_diverging_BuRd <- new_color_scheme(RColorBrewer::brewer.pal("RdBu", n = 11), "colors_diverging_BuRd", reverse = TRUE)
#' @rdname colors_diverging_blue2red
#' @export
colors_diverging_BuYlRd <- new_color_scheme(RColorBrewer::brewer.pal("RdYlBu", n = 11), "colors_diverging_BuYlRd", reverse = TRUE)
#' @rdname colors_diverging_blue2red
#' @export
colors_diverging_spectral <- new_color_scheme(
  c('#a20643', '#a90d45', '#ad1246', '#b41947', '#ba2049', '#be254a', '#c52c4b',
    '#cb334d', '#d0384e', '#d63f4f', '#d9444d', '#dc484c', '#df4e4b', '#e2514a',
    '#e55749', '#e95c47', '#eb6046', '#ef6645', '#f36b43', '#f47044', '#f57748',
    '#f67f4b', '#f7844e', '#f88c51', '#f99153', '#fa9857', '#fba05b', '#fca55d',
    '#fdad60', '#fdb365', '#fdb768', '#fdbd6d', '#fdc372', '#fdc776', '#fecc7b',
    '#fed27f', '#fed683', '#fedc88', '#fee08b', '#fee491', '#fee797', '#feea9b',
    '#feeda1', '#fff1a8', '#fff3ac', '#fff7b2', '#fffbb8', '#fffdbc', '#fefebd',
    '#fcfeba', '#f9fcb5', '#f6fbb0', '#f4faad', '#f1f9a9', '#eef8a4', '#ecf7a1',
    '#e9f69d', '#e6f598', '#e1f399', '#daf09a', '#d6ee9b', '#cfec9d', '#c8e99e',
    '#c3e79f', '#bce4a0', '#b5e1a2', '#b1dfa3', '#aadca4', '#a2d9a4', '#9cd7a4',
    '#94d4a4', '#8cd1a4', '#86cfa5', '#7ecca5', '#79c9a5', '#71c6a5', '#69c3a5',
    '#64c0a6', '#5eb9a9', '#58b2ac', '#54aead', '#4ea7b0', '#47a0b3', '#439bb5',
    '#3d95b8', '#3990ba', '#3389bd', '#3682ba', '#3a7eb8', '#3f77b5', '#4471b2',
    '#486cb0', '#4d65ad', '#525fa9', '#555aa7', '#5b53a4'),
  "colors_diverging_spectral", reverse = TRUE)
#' @rdname colors_diverging_blue2red
#' @export
colors_diverging_icefire <- new_color_scheme(
  c('#b7e3d9', '#afddd7', '#a9d9d6', '#a1d3d4', '#98cdd2', '#93cad1', '#8ac4d0',
    '#81bfcf', '#7bbbce', '#72b6ce', '#68b0cd', '#63adcd', '#5aa7cd', '#55a3cd',
    '#4e9ecd', '#4798ce', '#4394ce', '#3e8ecf', '#3987cf', '#3783d0', '#377cd0',
    '#3975cf', '#3b71cd', '#3f69c9', '#4265c5', '#465ebe', '#4858b6', '#4954b0',
    '#4a4fa5', '#494a9a', '#474792', '#454386', '#42407b', '#3f3d74', '#3c3a69',
    '#38365f', '#363459', '#323050', '#302e4a', '#2c2b42', '#29283b', '#272636',
    '#242430', '#22222b', '#212028', '#201f24', '#1f1e21', '#1f1e1f', '#201e1e',
    '#221e1e', '#261e1f', '#2a1e20', '#2d1f21', '#332023', '#392126', '#3d2228',
    '#43232a', '#4a252e', '#4f2630', '#572833', '#5c2935', '#642a38', '#6d2b3b',
    '#722c3d', '#7b2d40', '#842d42', '#8a2e43', '#932e44', '#9c2f45', '#a22f44',
    '#ab3043', '#b33341', '#b93540', '#c0393d', '#c53c3c', '#cc4139', '#d24737',
    '#d54b35', '#da5334', '#df5a33', '#e25f33', '#e66734', '#e96f36', '#eb753a',
    '#ed7e40', '#ef8445', '#f18c4e', '#f29558', '#f39a5f', '#f5a36a', '#f7ab75',
    '#f8b07c', '#fab887', '#fbc192', '#fcc69a', '#fecea5'),
  "colors_diverging_icefire")



# not exported
downsample_vector <- function(x, n, downsample = c("evenly", "first", "last", "middle")) {
  if (length(x) <= n) return(x)
  downsample <- match.arg(downsample)
  if (downsample == "evenly") {
    by <- (length(x) / (n-1)) - (1 / (n-1))
    i <- floor(cumsum(c(1, rep(by, n-1))))
    x[i]
  } else if (downsample == "first") {
    x[1:n]
  } else if (downsample == "last") {
    x[(length(x) - n + 1):length(x)]
  } else {
    start_index <- ceiling((length(x) - n) / 2) + 1
    end_index <- start_index + n - 1
    x[start_index:end_index]
  }
}

# downsample_vector(1:11, 6, downsample = "evenly")
# downsample_vector(1:11, 6, downsample = "first")
# downsample_vector(1:11, 6, downsample = "last")
# downsample_vector(1:11, 6, downsample = "middle")
# downsample_vector(1:5, 4, downsample = "evenly")
# downsample_vector(1:5, 4, downsample = "first")
# downsample_vector(1:5, 4, downsample = "last")
# downsample_vector(1:5, 4, downsample = "middle")


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

  paste0("<div style=\"margin-top: 60px; margin-bottom: 60px;\"><h4 style=\"font-family: 'Courier New', monospace;\">", name, "</h4><small style=\"font-family: 'Courier New', monospace;\">A tidyplots color scheme with ",size," colors",downsample_text,".</small><br>",color_bar,"<br><span style=\"font-family: 'Courier New', monospace;\">c(<br>", color_vector ,")</span></div>")
}

print_tidycolor <- function(x, max_colors, return_html = FALSE) {
  # cli::cli_alert_info(paste0("A tidyplots color scheme with ",length(x) ," colors."))

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
    # cli::cli_alert_success("A preview was send to the RStudio viewer pane.")
  } else {
    print(as.character(x))
  }
}

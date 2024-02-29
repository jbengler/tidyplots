

print_color <- function(hex, show_hex_code = TRUE) {
  bg_style <- cli::make_ansi_style(hex, bg = TRUE)
  text_color <- dplyr::if_else(as(colorspace::hex2RGB(hex), "HLS")@coords[,2] > 0.6, "#000000", "#FFFFFF")
  text_style <- cli::make_ansi_style(text_color)
  if (show_hex_code)
    cat(bg_style(text_style(paste0('"',hex,'"'))))
  else
    cat(bg_style(text_style(paste0('   '))))
}

preview_colors_in_console <- function(x, max_colors = 64) {
  if (length(x) > max_colors) {
    cat(paste0(length(x), " colors downsampled to ",max_colors," colors:\n"))
    x <- downsample_vector(x, max_colors)
  }

  n_per_line <- floor(cli::console_width() / 3)
  for (i in 1:length(x)) {
    print_color(x[i], show_hex_code = FALSE)
    if (i %% n_per_line == 0 && i != length(x)) cat("\n")
  }
  n_per_line <- floor(cli::console_width() / (nchar(x[[1]]) + 3))
  cat("\n\nc(")
  for (i in 1:length(x)) {
    print_color(x[i])
    if (i != length(x)) cat(",")
    if (i %% n_per_line == 0 && i != length(x)) cat("\n")
  }
  cat(")\n\nDisclaimer: Color preview in the console is not accurate.\nUse `view()` for accurate color preview.")
  cat("\n")
}

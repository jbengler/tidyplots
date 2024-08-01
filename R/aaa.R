#' @importFrom grDevices col2rgb rgb dev.off pdf
#' @importFrom stats sd setNames median
#' @importFrom utils tail
#' @importFrom rlang := .data
NULL

utils::globalVariables(c(".", ".single_color", "Lower", "Mean", "Upper", "aesthetic",
                         "col_zscore", "count", "row_zscore", "variable", "x", "y"))

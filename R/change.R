#' @importFrom ggplot2 %+%

#' @export
modify_order <- function(gg, var, levels, sort_by, reverse = FALSE) {
  if (!missing(levels) && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := factor({{var}}, levels = levels))
    cli::cli_alert_success("modify_order: reorderd by {.pkg levels}")
    return(gg %+% out)
  }
  if (!missing(sort_by) && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_reorder({{var}}, {{sort_by}}))
    cli::cli_alert_success("modify_order: reorderd by variable {.pkg sort_by}")
    return(gg %+% out)
  }
  if (reverse && !missing(var)) {
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_rev({{var}}))
    cli::cli_alert_success("modify_order: {.pkg reversed} order of labels")
    return(gg %+% out)
  }
  cli::cli_alert_warning("modify_order: order was {.pkg not changed}.")
  cli::cli_alert_warning("Please provide {.pkg var} together with {.pkg levels}, {.pkg sort_by} or {.pkg reverse = TRUE}.")
}

#' @export
modify_labels <- function(gg, var, labels) {
  if (!missing(labels) && !missing(var)) {
    labels <- setNames(names(labels), labels)
    out <-
      gg$data %>% dplyr::mutate({{var}} := forcats::fct_recode({{var}}, !!!labels))
    cli::cli_alert_success("modify_labels: applied custom {.pkg labels}")
    return(gg %+% out)
  }
  cli::cli_alert_warning("modify_labels: lables were {.pkg not changed}.")
  cli::cli_alert_warning("Please provide {.pkg var} together with {.pkg labels}")
}

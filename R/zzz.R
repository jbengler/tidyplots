.onAttach <- function(libname, pkgname) {
  if (!getOption("tidyplots.startup_message_shown", FALSE)) {
    doi <- "https://doi.org/10.1002/imt2.70018"

    packageStartupMessage(
      paste0(
        pkgname,
        " ",
        utils::packageVersion(pkgname),
        "\n",
        "In publications, please cite: ",
        cli::style_hyperlink(doi, doi)
      )
    )

    options(tidyplots.startup_message_shown = TRUE)
  }
}

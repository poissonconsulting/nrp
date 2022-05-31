.onLoad <- function(...) {
  suppressWarnings(units::install_unit("NTU"))
  suppressWarnings(units::install_unit("PSU"))
  suppressWarnings(units::install_unit("degC"))
}

#' @noRd
get_column <- function(dat, col_name) {
  if (col_name %in% names(dat)) {
    dat[[col_name]]
  } else {
    stop(col_name, " is not in selected data")
  }
}

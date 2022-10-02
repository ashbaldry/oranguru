#' @noRd
get_column <- function(dat, col_name) {
  if (col_name %in% names(dat)) {
    dat[[col_name]]
  } else {
    stop(col_name, " is not in selected data")
  }
}

#' @noRd
check_valid_integer <- function(x, min = -Inf, max = Inf, ref = "") {
  if (is_valid_integer(x, min, max)) {
    invisible(TRUE)
  } else {
    if (ref != "") {
      ref <- paste0("for ", ref, " ")
    }
    stop("Number ", ref, "must be a valid integer between ", min, " and ", max)
  }
}

#' @noRd
is_valid_integer <- function(x, min = -Inf, max = Inf) {
  is.numeric(x) && x == as.integer(x) && x >= min && x <= max
}

#' @seealso https://github.com/PokeAPI/pokeapi/blob/master/data/v2/csv/move_meta_ailments.csv
#' @noRd
cause_ailment_v1 <- function(move, attacker, defender) {
  ailment <- move$get_stat("meta_ailment_id")
  if (defender$apply_ailment(ailment)) {
    cat(defender$get_stat("name"), "has been", AILMENT_CHANGES[ailment], "\n")
  } else {
    cat(defender$get_stat("name"), "cannot be", AILMENT_CHANGES[ailment], "\n")
  }

  invisible(NULL)
}

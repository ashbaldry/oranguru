#' @seealso https://github.com/PokeAPI/pokeapi/blob/master/data/v2/csv/move_meta_ailments.csv
#' @noRd
cause_ailment_v1 <- function(move, attacker, defender) {
  ailment <- move$get_stat("meta_ailment_id")
  if (defender$apply_ailment(ailment)) {
    cat(defender$get_stat("name"), "has been", AILMENT_CHANGES[ailment], "\n")
  } else {
    current_ailment <- defender$get_stat("ailment")
    current_ailment <- current_ailment[current_ailment <= 5L]
    cat(
      defender$get_stat("name"), " is already ", AILMENT_CHANGES[current_ailment], ", ",
      "they cannot be ", AILMENT_CHANGES[ailment], "\n",
      sep = ""
    )
  }

  invisible(NULL)
}

#' @noRd
calculate_damage <- function(move, attacker, defender, ..., generation = 1L) {
  check_generation(generation)
  if (move %nin% attacker$get_moves()) {
    stop(
      attacker$get_stat("name"), " does not know ", move, ". ",
      "Please use one of ", toString(attacker$get_moves())
    )
  }

  if (generation == 1L) {
    calculate_damage_v1(move, attacker, defender)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }
}

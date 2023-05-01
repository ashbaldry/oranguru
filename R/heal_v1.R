#' @noRd
heal_pokemon_v1 <- function(move, attacker, defender) {
  effect_id <- move$get_stat("effect_id")
  if (effect_id == 33L) {
    attacker$heal(50)
  }
}

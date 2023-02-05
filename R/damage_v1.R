#' @details
#' Calculation:
#'
#' ((2 \* Level \* Critical / 5 + 2) \* Power \* Attack / Defense / 50 + 2) \*
#' STAB \* Type1 \* Type2 \* random
#'
#' @seealso https://bulbapedia.bulbagarden.net/wiki/Damage
#' @noRd
calculate_damage_v1 <- function(move, attacker, defender) {
  move_type <- subset(moves, identifier == move, select = "type_id", drop = TRUE)
  move_power <- subset(moves, identifier == move, select = "power", drop = TRUE)

  level <- attacker$get_stat("level")
  p1_types <- attacker$get_stat("type_id")
  p2_types <- defender$get_stat("type_id")

  if (TRUE) {
    p1_attack <- attacker$get_stat("attack")
    p2_defense <- defender$get_stat("defense")
  } else {
    p1_attack <- attacker$get_stat("sp_attack")
    p2_defense <- defender$get_stat("sp_defense")
  }

  crit <- include_crit_multipler(attacker$get_crit_chance(move), generation = 1L)

  damage <- ((2 * level * crit / 5 + 2) * move_power * p1_attack / p2_defense / 50 + 2) *
    include_stab_multiplier(move_type, p1_types) *
    include_type_multiplier(move_type, p2_types, generation = 1L)

  if (damage >= 2L) {
    damage <- damage * include_random_factor()
  }

  floor(damage)
}

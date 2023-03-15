#' @details
#' Calculation for Generation 1:
#'
#' ((2 \* Level \* Critical / 5 + 2) \* Power \* Attack / Defense / 50 + 2) \*
#' STAB \* Type1 \* Type2 \* random
#'
#' @seealso https://bulbapedia.bulbagarden.net/wiki/Damage
#' @noRd
calculate_damage_v1 <- function(move, attacker, defender, damage_range = FALSE, crit_range = FALSE) {
  move_type <- move$get_stat("type_id")
  move_power <- move$get_stat("power")

  level <- attacker$get_stat("level")
  p1_types <- attacker$get_stat("type_id")
  p2_types <- defender$get_stat("type_id")

  # 1: no damage, 2: attack/defense, 3: special attack/defense
  if (move$get_stat("damage_class_id") == 2) {
    p1_attack <- attacker$get_stat("attack")
    p2_defense <- defender$get_stat("defense")
  } else {
    p1_attack <- attacker$get_stat("sp_attack")
    p2_defense <- defender$get_stat("sp_defense")
  }

  if (damage_range) {
    crit <- include_crit_multipler(as.numeric(crit_range), generation = 1L)
  } else {
    crit <- include_crit_multipler(attacker$get_crit_chance(move), generation = 1L)
    if (crit > 1) {
      defender$crit_applied()
    }
  }

  damage <- ((2 * level * crit / 5 + 2) * move_power * p1_attack / p2_defense / 50 + 2) *
    include_stab_multiplier(move_type, p1_types) *
    include_type_multiplier(move_type, p2_types, generation = 1L)
  damage <- include_random_factor(damage, generation = 1L, damage_range = damage_range)

  if (is.finite(damage)) floor(damage) else 0L
}

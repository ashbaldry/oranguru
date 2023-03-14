#' @noRd
use_attack_v1 <- function(move, attacker, defender, battle) {
  if (move$get_stat("meta_category_id") == 0L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
  } else {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
  }

  cat(move$get_stat("name"), "did", damage_dealt, "damage\n")
  defender$take_damage(damage_dealt)
  invisible(NULL)
}

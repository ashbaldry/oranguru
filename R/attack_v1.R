#' @noRd
use_attack_v1 <- function(move, attacker, defender, battle) {
  move_category <- get_move_meta_info(move, "meta_category_id")

  # Damage dealt
  if (move_category == 0L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
  } else {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
  }


}

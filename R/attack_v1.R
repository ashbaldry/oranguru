#' @noRd
use_attack_v1 <- function(move, attacker, defender, battle) {
  accuracy <- move$get_stat("accuracy")
  if (isTRUE(accuracy < 100L)) {
    attack_miss <- sample(c(FALSE, TRUE), 1L, prob = c(accuracy, 100 - accuracy))
    if (attack_miss) {
      cat("The attack missed!\n")
      return(invisible(NULL))
    }
  }

  if (move$get_stat("meta_category_id") == 0L) {
    damage_dealt <- calculate_damage_v1(move, attacker, defender)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 1L) {
    cause_ailment_v1(move, attacker, defender)
  } else if (move$get_stat("meta_category_id") == 2L) {
    change_stat_v1(move, attacker, defender)
  } else if (move$get_stat("meta_category_id") == 3L) {
    heal_pokemon_v1(move, attacker, defender)
  } else if (move$get_stat("meta_category_id") == 4L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 6L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 8L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 9L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 10L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 11L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 12L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else if (move$get_stat("meta_category_id") == 13L) {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  } else {
    damage_dealt <- calculate_damage(move, attacker, defender, generation = 1L)
    defender$take_damage(damage_dealt)
  }

  invisible(NULL)
}

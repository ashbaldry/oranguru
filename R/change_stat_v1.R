#' @noRd
change_stat_v1 <- function(move, attacker, defender) {
  effect <- move$get_stat("effect_id")

  # Increase stat by 1
  if (effect %in% c(11L, 12L, 13L, 14L, 15L)) {
    attacker$change_stat(stat = PK_STATS[effect - 10L], change = 1L)
  }

  # Decrease stat by 1
  if (effect %in% c(19L, 20L, 21L, 22L, 23L)) {
    attacker$change_stat(stat = PK_STATS[effect - 18L], change = -1L)
  }

  # Increase stat by 2
  if (effect %in% c(51L, 52L, 53L, 54L, 55L)) {
    attacker$change_stat(stat = PK_STATS[effect - 50L], change = 2L)
  }

  # Decrease stat by 2
  if (effect %in% c(59L, 60L, 61L, 62L, 63L)) {
    attacker$change_stat(stat = PK_STATS[effect - 58L], change = -2L)
  }

  invisible(NULL)
}

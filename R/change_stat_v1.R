#' @noRd
change_stat_v1 <- function(move, attacker, defender) {
  effect <- move$get_stat("effect_id")

  # Increase stat by 1
  if (effect %in% c(51L, 52L, 53L, 54L, 55L)) {
    attacker$change_stat(stat = PK_STATS[effect - 50L], change = 1L)
  }

  invisible(NULL)
}

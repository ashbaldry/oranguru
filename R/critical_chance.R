#' Calculate Critical Hit Chance
#'
#' @seealso https://bulbapedia.bulbagarden.net/wiki/Critical_hit
#' @export
calculate_critical_chance <- function(move, ..., generation = 1L) {
  if (generation == 1L) {
    calculate_critical_chance_v1(move, ...)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }
}

calculate_critical_chance_v1 <- function(move, pokemon) {
  is_high_crit <- subset(moves_meta, move_id == get_move_info(move, "id"), select = "crit_rate", drop = TRUE)
  speed <- pokemon$get_stat("speed")
  pkmn_high_crit <- pokemon_1$get_stat("critical_hit_change") > 0L

  if (is_high_crit) {
    if (pkmn_high_crit) {
      crit <- 4L * floor(speed / 4L)
    } else {
      crit <- min(8L * floor(speed / 2L), 255L)
    }
  } else {
    if (pkmn_high_crit) {
      # Yes we are keeping the focus energy bug in
      crit <- floor(speed / 8L)
    } else {
      crit <- floor(speed / 2L)
    }
  }

  crit / 256
}

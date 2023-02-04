#' Calculate Damage for Generation 1
#'
#' @param move Name of the move
#' @param pokemon_1 The Pokémon that is performing the attack
#' @param pokemon_2 The Pokémon that is being attacked
#'
#' @seealso https://bulbapedia.bulbagarden.net/wiki/Damage
#'
#' @family damage_calculations
#' @encoding UTF-8
#' @noRd
calculate_damage_v1 <- function(move, pokemon_1, pokemon_2) {
  if (move %nin% pokemon_1$moveset()) {
    stop(move, " is not a valid move for ", pokemon_1$.__enclos_env__$private$name)
  }

  move_power <- get_move_info(move, "power")
}

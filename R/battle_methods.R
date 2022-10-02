#' Pokémon Battle Turn
#'
#' @rdname battle_methods
#' @encoding UTF-8
#' @export
# attack <- R7::new_generic("attack", "move")
#
# switch <- R7::new_generic("switch", "x", function(x, ...) R7::R7_dispatch())
# R7::method(switch, battle) <- function(x, new_active, player = 1) {
#   if (player == 1) {
#
#   }
# }
#
# R7::method(switch, team) <- function(x, new_active) {
#   check_valid_integer(new_active, 1L, 6L, "new active Pokémon")
#   if (new_active == x@active) {
#     stop("Pokémon cannot be switched with itself")
#   }
#
#   pk_health <- eval(parse(text = paste0("x@pokemon_", new_active, "@current_hp")))
#   if (pk_health == 0) {
#     stop("Unable to switch with a fainted Pokémon")
#   }
#
#   x@active <- new_active
#   invisible(x@active)
# }

#' Use Attack
#'
#' @description
#' Calculate the effect of the attack
#'
#' @param move Name of the move that the \code{atttacker} is using
#' @param attacker The attacking \code{\link{Pokemon}}
#' @param defender The defending \code{\link{Pokemon}}
#' @param battle The \code{\link{PokemonBattle}} in place. Used to know
#' @param generation The generation that the battle is happening in
#'
#' @return
#' Nothing returned, however the `attacker`, `defender` and `battle`
#' are all updated to the effects of the `move` used.
#'
#' @export
use_attack <- function(move, attacker, defender, battle, generation = 1L) {
  if (generation == 1L) {
    use_attack_v1(move, attacker, defender, battle)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }
}

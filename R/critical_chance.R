#' Calculate Critical Hit Chance
#'
#' @description
#' This calculates the probability that a critical hit will land.
#'
#' @details
#' Generation 1 does include the mechanics within the original games. For example, the
#' chance for a critical hit from standard moves **reduces** after a move like swords
#' dance has been used.
#'
#' @param move The name of the move that is being used
#' @param ... Parameters that will be sent to calculations dependent on the generation
#' @param generation The generation that the battle is based on
#'
#' @seealso https://bulbapedia.bulbagarden.net/wiki/Critical_hit
#'
#' @rdname calculate_critical_chance
#' @export
calculate_critical_chance <- function(move, ..., generation = 1L) {
  if (generation == 1L) {
    calculate_critical_chance_v1(move, ...)
  } else {
    calculate_critical_chance_v2(move)
  }
}

#' @param speed The base speed of the \code{\link{Pokemon}} that has used the attack
#' @param high_crit Logical: has the \code{\link{Pokemon}} used a move that has increased
#' its critical hit chance?
#'
#' @rdname calculate_critical_chance
calculate_critical_chance_v1 <- function(move, speed, high_crit = 0L) {
  is_high_crit <- subset(
    moves_meta,
    move_id == get_move_info(move, "id"),
    select = "crit_rate",
    drop = TRUE
  )

  if (is_high_crit) {
    if (high_crit > 0L) {
      crit <- 4L * floor(speed / 4L)
    } else {
      crit <- min(8L * floor(speed / 2L), 255L)
    }
  } else {
    if (high_crit > 0L) {
      # Yes we are keeping the focus energy bug in
      crit <- floor(speed / 8L)
    } else {
      crit <- floor(speed / 2L)
    }
  }

  crit / 256
}

#' @rdname calculate_critical_chance
calculate_critical_chance_v2 <- function(move) {
  stop("Damage has not been calculated yet for this generation")
}

#' Calculate Damage
#'
#' @description
#' A damage calculation
#'
#' @param move Name of the move that the \code{atttacker} is using
#' @param attacker The attacking \code{\link{Pokemon}}
#' @param defender The defending \code{\link{Pokemon}}
#' @param generation The generation that the battle is happening in
#'
#' @export
calculate_damage <- function(move, attacker, defender, generation = 1L) {
  check_damage_args(move, attacker, defender, generation)

  if (generation == 1L) {
    calculate_damage_v1(move, attacker, defender)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }
}

#' @description
#' \code{calculate_damage_range} will give the expected range that the damage to
#' the opposing Pokémon might be. There will be the assumption that no critical
#' hit has been landed
#'
#' @rdname calculate_damage
calculate_damage_range <- function(move, attacker, defender, generation = 1L) {
  check_damage_args(move, attacker, defender, generation)

  if (generation == 1L) {
    damage_dealt <- calculate_damage_v1(move, attacker, defender, damage_range = TRUE)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }

  damage_dealt_table <- table(damage_dealt)
  damage_dealt_table <- damage_dealt_table / length(damage_dealt)

  cat(
    "Move used: ", move, "\n",
    "Damage Range: ", min(damage_dealt), " - ", max(damage_dealt), "\n\n",
    "Probabilities:\n\n",
    sep = ""
  )
  print(
    data.frame(
      Damage = names(damage_dealt_table),
      Chance = paste0(round(damage_dealt_table * 100, 1), "%")
    )
  )

  invisible(damage_dealt_table)
}

check_damage_args <- function(move, attacker, defender, generation = 1L) {
  check_generation(generation)
  if (move %nin% attacker$get_moves()) {
    stop(
      attacker$get_stat("name"), " does not know ", move, ". ",
      "Please use one of ", toString(attacker$get_moves())
    )
  }
}

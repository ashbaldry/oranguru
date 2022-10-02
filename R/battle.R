#' Run Pokémon Battle
#'
#' @description
#' Create two teams of Pokémon and play out a match to see who is the world's number one Pokémon master!
#'
#' @param team_1 Player 1's Pokémon team
#' @param team_2 Player 2's Pokémon team
#' @param cpu Logical, is player 2 a CPU? Default set to `TRUE`
#' @param generation If teams are random, then the generation the Pokémon will be selected from.
#' Default is Gen 8.
#'
#' @details
#' For the sake of generation consistency, both teams must be pre-defined or both teams are random.
#' To create a random team use \code{\link{team}(generation = 1L)}.
#'
#' In the initial version, the CPU will use a random move. AI will improve in future releases.
#'
#' @include pokemon.R team.R
#'
#' @encoding UTF-8
#' @export
battle <- R7::new_class(
  name = "battle",
  package = "pokemon",

  properties = list(
    cpu = R7::class_logical,
    team_1 = team,
    team_2 = team
  ),

  constructor = function(team_1, team_2, cpu = TRUE, generation = 8L) {
    if (missing(team_1) && missing(team_2)) {
      team_1 <- team(generation = generation)
      team_2 <- team(generation = generation)
    } else if (missing(team_1) || missing(team_2)) {
      stop("Either both teams must be random or both teams are pre-defined")
    }

    R7::new_object(
      R7::R7_object(),
      team_1 = team_1,
      team_2 = team_2,
      cpu = TRUE
    )
  }
)

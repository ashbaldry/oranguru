#' Run Pokémon Battle
#'
#' @description
#' Create two teams of Pokémon and play out a match to see who is the world's number one Pokémon master!
#'
#' @details
#' For the sake of generation consistency, both teams must be pre-defined or both teams are random.
#' To create a random team use \code{\link{team}(generation = 1L)}.
#'
#' In the initial version, the CPU will use a random move. AI will improve in future releases.
#'
#' @encoding UTF-8
#' @export
PokemonBattle <- R6::R6Class(
  classname = "battle",

  public = list(
    #' @description
    #' Commence a Pokémon battle
    #'
    #' @param team_1 Player 1's Pokémon team
    #' @param team_2 Player 2's Pokémon team
    #' @param cpu Logical, is player 2 a CPU? Default set to `TRUE`
    #' @param generation If teams are random, then the generation the Pokémon will be selected from.
    #' Default is Gen 8.
    initialize = function(team_1, team_2, cpu = TRUE, generation = 8L) {
      if (missing(team_1) && missing(team_2)) {
        private$team_1 <- PokemonTeam$new(generation = generation)
        private$team_2 <- PokemonTeam$new(generation = generation)
      } else if (missing(team_1) || missing(team_2)) {
        stop("Either both teams must be random or both teams are pre-defined")
      } else {
        private$team_1 <- team_1
        private$team_2 <- team_2
      }
    },

    #' @description
    #' Show current status of Pokémon Battle
    status = function() {
      cat("Player 1\n")
      private$team_1$status()
      cat("\n")
      cat("Player 2", if (private$cpu) " (CPU)\n" else "\n")
      private$team_2$status()
    }
  ),

  private = list(
    cpu = TRUE,
    team_1 = NULL,
    team_2 = NULL,

    player_1_ready = FALSE,
    player_2_ready = FALSE
  )
)

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
    #' @param player_2_cpu Logical, is player 2 a CPU? Default set to `TRUE`
    #' @param generation If teams are random, then the generation the Pokémon will be selected from.
    #' Default is Gen 8.
    initialize = function(team_1, team_2, player_2_cpu = TRUE, generation = 1L) {
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
      cat("Player 2", if (private$player_2_cpu) " (CPU)\n" else "\n")
      private$team_2$status()
    },

    #' @description
    #' Select Pokémon attack
    attack = function() {
      private$player_1_ready <- TRUE
    },

    #' @description
    #' Select Pokémon attack (player 2)
    attack_p2 = function() {
      if (private$player_2_cpu) {
        message("Player 2 is a CPU. Don't try to cheat!")
      }
      private$player_2_ready <- TRUE
    },

    #' @description
    #' Select active Pokémon
    switch = function() {
      private$player_1_ready <- TRUE
    },

    #' @description
    #' Select active Pokémon (player 2)
    switch_p2 = function() {
      if (private$player_2_cpu) {
        message("Player 2 is a CPU. Don't try to cheat!")
      }
      private$player_2_ready <- TRUE
    }
  ),

  private = list(
    team_1 = NULL,
    team_2 = NULL,

    player_1_ready = FALSE,
    player_2_cpu = TRUE,
    player_2_ready = FALSE,

    resolve_turn = function() {
      if (isFALSE(private$player_1_ready) || isFALSE(private$player_2_ready || private$player_2_cpu)) {
        return()
      }
      if (private$player_2_cpu) {
        private$team_2$select_move()
      }



      private$player_1_ready <- FALSE
      private$player_2_ready <- FALSE
    }
  )
)

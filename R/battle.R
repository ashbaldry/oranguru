#' Run Pokémon Battle
#'
#' @description
#' Create two teams of Pokémon and play out a match to see who is the world's number one Pokémon master!
#'
#' @details
#' For the sake of generation consistency, both teams must be pre-defined or both teams are random.
#' To create a random team use \code{\link{PokemonTeam}$new(generation = 1L)}.
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
    #' @param level If random teams, the level the Pokémon should be
    #' @param generation If teams are random, then the generation the Pokémon will be selected from.
    #' Default is Gen 8.
    initialize = function(team_1, team_2, player_2_cpu = TRUE, level = 50L, generation = 1L) {
      if (missing(team_1) && missing(team_2)) {
        private$team_1 <- PokemonTeam$new(random = TRUE, level = level, generation = generation)
        private$team_2 <- PokemonTeam$new(random = TRUE, level = level, generation = generation)
      } else if (missing(team_2) && player_2_cpu) {
        private$team_1 <- team_1
        # TODO: get maximum level of player team
        cpu_level <- max(level, level)
        private$team_2 <- PokemonTeam$new(random = TRUE, level = cpu_level, generation = generation)
      } else if (missing(team_1) || missing(team_2)) {
        stop("Both player teams must either be random or pre-defined")
      } else {
        private$team_1 <- team_1
        private$team_2 <- team_2
      }
    },

    #' @description
    #' A helper to start the local console version of a battle
    start = function() {
      cat("BATTLE COMMENCE!\n\n")

      while (private$team_1$able_to_battle() && private$team_2$able_to_battle()) {
        private$match_status()

        private$player_choice(person = 1L)

        if (private$player_2_cpu) {

        } else {
          while (!private$player_2_ready) {
            p2_option <- menu(c("Attack", "Switch"), title = "(P2) What would you like to do?")

            if (p2_option == 1) {

            } else if (p2_option == 2) {
              private$team_2$status()
              cat("\n")
              available_pokemon <- private$team_2
              p2_swtich <- menu(available_pokemon, title = "(P2) Who would you like to switch with?")

            } else {
              next
            }
          }
        }

        private$resolve_turn()
      }

      winner <- 2 - as.numeric(private$team_1$able_to_battle())
      loser <- 3 - winner
      cat("Player", loser, "has no Pokémon available to battle, Player", winner, "wins!")
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
    #'
    #' @param new_active Team position of the Pokémon to switch in
    #' @param curr_active Team position of the Pokémon to switch out. Defaults to current active
    #'
    #' @encoding UTF-8
    switch = function(new_active, curr_active = private$active_1) {
      private$active_1 <- new_active
      private$player_1_ready <- TRUE
    },

    #' @description
    #' Select active Pokémon (player 2)
    #'
    #' @param new_active Team position of the Pokémon to switch in
    #' @param curr_active Team position of the Pokémon to switch out. Defaults to current active
    #'
    #' @encoding UTF-8
    switch_p2 = function(new_active, curr_active = private$active_2) {
      if (private$player_2_cpu) {
        message("Player 2 is a CPU. Don't try to cheat!")
      }

      private$active_2 <- new_active
      private$player_2_ready <- TRUE
    }
  ),

  private = list(
    team_1 = NULL,
    team_2 = NULL,

    player_1_ready = FALSE,
    player_2_cpu = TRUE,
    player_2_ready = FALSE,

    active_1 = 1L,
    active_2 = 1L,

    action_1 = NULL,
    action_2 = NULL,

    move_1 = NULL,
    move_2 = NULL,

    new_active_1 = NULL,
    new_active_2 = NULL,

    match_status = function() {

    },

    #' @description
    #' Determine player choice
    player_choice = function(person = 1L) {
      while (!private$player_1_ready) {
        p1_option <- menu(c("Attack", "Switch"), title = "What would you like to do?")

        if (p1_option == 1) {
          private$p1_attack()
        } else if (p1_option == 2) {
          private$p1_switch()
        }
      }
    },

    p1_attack = function() {
      active_pokemon <- private$team_1$get_pokemon(private$active_1)
      available_moves <- active_pokemon$get_moves()

      selected_move <- menu(available_moves, title = "Which move would you like to use?")
      if (selected_move > 0L) {
        private$move_1 <- available_moves[selected_move]
      }
    },

    p1_switch = function() {
      private$team_1$status()
      cat("\n")

      available_pokemon <- private$team_1$healthy_pokemon()
      available_pokemon <- available_pokemon[-match(private$active_1, available_pokemon)]

      if (length(available_pokemon) > 0L) {
        new_active <- menu(names(available_pokemon), title = "Who would you like to switch with?")
        if (new_active > 0L) {
          private$new_active_1 <- unname(available_pokemon)[new_active]
        }
      } else {
        cat("No available Pokémon to switch with. Returning to home options")
      }
    },

    resolve_turn = function() {
      if (isFALSE(private$player_1_ready) || isFALSE(private$player_2_ready || private$player_2_cpu)) {
        return()
      }
      if (private$player_2_cpu) {
        private$team_2$select_move()
      }

      private$action_1 <- NULL
      private$action_2 <- NULL

      private$player_1_ready <- FALSE
      private$player_2_ready <- FALSE
    },

    status_check = function() {

    }
  )
)

#' Run Pokémon Battle
#'
#' @description
#' Create two teams of Pokémon and play out a match to see who is the world's number one Pokémon master!
#'
#' To create a random team use \code{\link{PokemonTeam}$new(generation = 1L)}.
#'
#' In the initial version, the CPU will use a random move. AI will improve in future releases.
#'
#' @encoding UTF-8
#' @export
PokemonBattle <- R6::R6Class(
  classname = "pokemon_battle",

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
    #'
    #' @return
    #' A state where a Pokemon battle can commence
    #'
    #' @encoding UTF-8
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

      private$player_2_cpu <- player_2_cpu
    },

    #' @description
    #' A helper to start the local console version of a battle
    start = function() {
      if (private$started) {
        cat("BATTLE COMMENCE!\n\n")
        private$started <- FALSE
      } else {
        cat("BATTLE RESUMED\n\n")
      }

      while (private$team_1$able_to_battle() && private$team_2$able_to_battle()) {
        private$match_status()

        p1_decision_made <- private$player_choice(person = 1L)
        if (isFALSE(p1_decision_made)) {
          cat("Pausing match\n")
          return(invisible(NULL))
        }

        if (!private$player_2_cpu) {
          p2_decision_made <- private$player_choice(person = 2L)
          if (isFALSE(p2_decision_made)) {
            cat("Pausing match\n")
            return(invisible(NULL))
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
      private$player_ready_2 <- TRUE
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
      private$player_ready_1 <- TRUE
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

    started = TRUE,
    player_ready_1 = FALSE,
    player_ready_2 = FALSE,
    player_2_cpu = TRUE,

    active_1 = 1L,
    active_2 = 1L,

    action_1 = NULL,
    action_2 = NULL,

    move_1 = NULL,
    move_2 = NULL,

    new_active_1 = NULL,
    new_active_2 = NULL,

    match_status = function() {
      cat(
        "P1: ", private$team_1$get_pokemon(private$active_1)$status(simple = TRUE, console = FALSE),
        "P2: ", private$team_2$get_pokemon(private$active_2)$status(simple = TRUE, console = FALSE),
        "\n",
        sep = ""
      )
    },

    player_choice = function(person = 1L) {
      if (person == 2 && private$player_2_cpu) {
        message("Player 2 is a CPU. Don't try to cheat!")
      }

      option_message <- paste0(
        if (private$player_2_cpu) NULL else paste0("(P", person, ") "),
        "What would you like to do?"
      )

      while (!private[[paste0("player_ready_", person)]]) {
        selected_option <- menu(c("Attack", "Switch", "Check Stats"), title = option_message)

        if (selected_option == 1) {
          private$select_attack(person = person)
        } else if (selected_option == 2) {
          private$select_switch(person = person)
        } else if (selected_option == 3) {
          private[[paste0("team_", person)]]$get_pokemon(private[[paste0("active_", person)]])$status()
          cat("\n")
        } else if (selected_option == 0) {
          return(FALSE)
        }
      }
    },

    select_attack = function(person = 1L) {
      active_position <- private[[paste0("active_", person)]]
      active_pokemon <- private[[paste0("team_", person)]]$get_pokemon(active_position)
      available_moves <- active_pokemon$get_moves()

      selected_move <- menu(available_moves, title = "Which move would you like to use?")
      if (selected_move > 0L) {
        private[[paste0("action_", person)]] <- "attack"
        private[[paste0("move_", person)]] <- active_pokemon$get_stat(paste0("move_", selected_move))
        private[[paste0("player_ready_", person)]] <- TRUE
      }
    },

    select_cpu_attack = function() {
      active_pokemon <- private$team_2$get_pokemon(private$active_2)
      available_moves <- length(active_pokemon$get_moves())

      private$action_2 <- "attack"
      private$move_2 <- active_pokemon$get_stat(paste0("move_", sample(available_moves, 1L)))
    },

    select_switch = function(person = 1L) {
      team_id <- paste0("team_", person)
      active_position <- private[[paste0("active_", person)]]

      private[[team_id]]$status()
      cat("\n")

      available_pokemon <- private[[team_id]]$healthy_pokemon()
      available_pokemon <- available_pokemon[-match(active_position, available_pokemon)]

      if (length(available_pokemon) > 0L) {
        new_active <- menu(names(available_pokemon), title = "Who would you like to switch with?")
        if (new_active > 0L) {
          private[[paste0("action_", person)]] <- "switch"
          private[[paste0("new_active_", person)]] <- unname(available_pokemon)[new_active]
          private[[paste0("player_ready_", person)]] <- TRUE
        }
      } else {
        cat("No available Pokémon to switch with. Returning to home options")
      }
    },

    resolve_turn = function() {
      if (isFALSE(private$player_ready_1) || isFALSE(private$player_ready_2 || private$player_2_cpu)) {
        return(NULL)
      }
      if (private$player_2_cpu) {
        private$select_cpu_attack()
      }

      if (private$action_1 == "switch") private$switch_pokemon(1L)
      if (private$action_2 == "switch") private$switch_pokemon(2L)

      attack_order <- private$determine_attack_order()
      for (attacker in attack_order) {
        team_id <- paste0("team_", attacker)
        active_id <- paste0("active_", attacker)

        if (private[[team_id]]$get_pokemon(private[[active_id]])$get_stat("current_hp") > 0) {
          private$attack_pokemon(attacker)
        }
      }

      private$action_1 <- NULL
      private$action_2 <- NULL

      private$player_ready_1 <- FALSE
      private$player_ready_2 <- FALSE

      cat("\n")
    },

    switch_pokemon = function(person = 1L) {
      team_id <- paste0("team_", person)
      active_id <- paste0("active_", person)
      new_active_id <- paste0("new_active_", person)

      cat(
        "P1 has switched",
        private[[team_id]]$get_pokemon(private[[active_id]])$get_stat("name"),
        "for",
        private[[team_id]]$get_pokemon(private[[new_active_id]])$get_stat("name"),
        "\n"
      )

      private[[active_id]] <- private[[new_active_id]]
    },

    determine_attack_order = function() {
      if (private$action_1 == "switch" && private$action_2 == "switch") {
        NULL
      } else if (private$action_2 == "switch") {
        1L
      } else if (private$action_1 == "switch") {
        2L
      } else {
        m1_priority <- private$move_1$get_stat("priority")
        m2_priority <- private$move_2$get_stat("priority")
        if (m1_priority == m2_priority) {
          p1_pokemon <- private$team_1$get_pokemon(private$active_1)
          p2_pokemon <- private$team_2$get_pokemon(private$active_2)

          if (p1_pokemon$get_stat("speed") == p2_pokemon$get_stat("speed")) {
            first <- round(runif(1L) + 1L)
          } else {
            first <- which.max(c(p1_pokemon$get_stat("speed"), p2_pokemon$get_stat("speed")))
          }
        } else {
          first <- which.max(c(m1_priority, m2_priority))
        }

        c(first, 3L - first)
      }
    },

    attack_pokemon = function(person = 1L) {
      team_id <- paste0("team_", person)
      move_id <- paste0("move_", person)
      active_id <- private[[paste0("active_", person)]]
      pokemon <- private[[team_id]]$get_pokemon(active_id)

      cat(pokemon$get_stat("name"), "has used", private[[move_id]]$get_stat("name"), "\n")
    },

    status_check = function(pokemon) {
    }
  )
)

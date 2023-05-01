#' Create a New Pokémon
#'
#' @description
#' R6 object containing required information about a Pokémon.
#'
#' @encoding UTF-8
#'
#' @importFrom R6 R6Class
#' @export
Pokemon <- R6::R6Class(
  classname = "pokemon",
  public = list(
    #' @description
    #' Create a Pokémon
    #'
    #' @param pokemon Name of the Pokémon to create
    #' @param level The level the Pokémon should be
    #' @param generation The generation that the Pokémon comes from
    #' @param nature Either a specified nature, or `NULL` for a random nature.
    #' **NB** Nature is only required from generation 3 onwards
    #' @param api_data Either data pulled from `pokeapi::get_pokemon`, or
    #' `NULL` to select the data of a random Pokémon
    #' @param language ISO-2 character of the language to pull the name of
    #' the Pokémon
    #'
    #' @return
    #' A Pokémon
    #'
    #' @encoding UTF-8
    initialize = function(pokemon = NULL, level = 50L, generation = 1L,
                          nature = NULL, api_data = NULL, language = "en") {
      check_level(level)
      check_generation(generation)

      if (is.null(api_data)) {
        if (is.null(pokemon)) {
          pokemon <- get_random_pokemon_id(generation)
        }
        api_data <- pokeapi::get_pokemon(pokemon)
      }

      id <- api_data$id

      name <- get_pokemon_name(id, language = language)
      types <- vapply(api_data$types, \(x) x$type$name, character(1))
      type_ids <- vapply(api_data$types, \(x) as.integer(basename(x$type$url)), integer(1))

      private$name <- name
      private$type <- types
      private$type_id <- type_ids
      private$level <- as.integer(level)
      private$generation <- generation

      base_stats <- setNames(
        vapply(api_data$stats, \(x) x$base_stat, integer(1L)),
        vapply(api_data$stats, \(x) x$stat$name, character(1L))
      )

      hp <- calculate_hp(base_stats[["hp"]], level = level, generation = generation)
      private$hp <- hp
      private$current_hp <- hp

      private$base_attack <- base_stats[["attack"]]
      private$base_defense <- base_stats[["defense"]]
      private$base_sp_attack <- base_stats[["special-attack"]]
      private$base_sp_defense <- base_stats[["special-defense"]]
      private$base_speed <- base_stats[["speed"]]

      other_stats <- vapply(
        setNames(nm = names(base_stats)[-1L]),
        \(x) calculate_stat(base_stats[[x]], x, nature = nature, generation = generation),
        integer(1L)
      )

      private$attack <- other_stats[["attack"]]
      private$defense <- other_stats[["defense"]]
      private$sp_attack <- other_stats[["special-attack"]]
      private$sp_defense <- other_stats[["special-defense"]]
      private$speed <- other_stats[["speed"]]

      moves <- learn_moves(api_data$moves, level = level, generation = generation)
      private$move_1 <- Move$new(moves[1L])
      if (length(moves) >= 2L) private$move_2 <- Move$new(moves[2L])
      if (length(moves) >= 3L) private$move_3 <- Move$new(moves[3L])
      if (length(moves) >= 4L) private$move_4 <- Move$new(moves[4L])
      private$all_moves <- find_valid_moves(api_data$moves, level = level, generation = generation)

      if (generation > 2L) {
        nature <- find_nature(nature)
      } else {
        nature <- character(0L)
      }
      private$nature <- nature

      private$sprite_front_url <- api_data$sprites$front_default
      private$sprite_back_url <- api_data$sprites$back_default
    },

    #' @description
    #' Show current status of the Pokémon
    #'
    #' @param simple Logical, do you just want the simple status (name + HP) printed?
    #' @param console Logical, should the status be printed or returned as a string?
    #'
    #' @encoding UTF-8
    status = function(simple = FALSE, console = TRUE) {
      show_status(private, simple = simple, console = console)
    },

    #' @description
    #' Use an attack of the Pokémon
    #'
    #' @param move The name of the move that is being used
    #' @param def_pokemon The defending `Pokemon`
    #' @param battle A \code{\link{PokemonBattle}}
    #'
    #' @encoding UTF-8
    use_move = function(move, def_pokemon, battle) {
      if (isFALSE(move %in% self$get_moves())) {
        warning("Selected move (", move, ") is not available for ", private$name)
      }

      move_id <- match(move, self$get_moves())
      move_r6 <- private[[paste("move", move_id, sep = "_")]]

      if (move_r6$use_move()) {
        use_attack(move_r6, self, def_pokemon, battle, generation = private$generation)
      }

      invisible(NULL)
    },

    #' @description
    #' Get the stat of the Pokémon
    #'
    #' @param stat The private field of the Pokémon
    #'
    #' @encoding UTF-8
    get_stat = function(stat) {
      if (stat %nin% names(private)) {
        stop(stat, " not available for Pokémon")
      }
      private[[stat]]
    },

    #' @description
    #' Get the moveset of the Pokémon
    #'
    #' @encoding UTF-8
    get_moves = function() {
      c(
        private$move_1$get_stat("name"),
        if (inherits(private$move_2, "move")) private$move_2$get_stat("name"),
        if (inherits(private$move_3, "move")) private$move_3$get_stat("name"),
        if (inherits(private$move_4, "move")) private$move_4$get_stat("name")
      )
    },

    #' @description
    #' Get the detailed move information of the Pokémon
    #'
    #' @encoding UTF-8
    get_moves_pp = function() {
      c(
        private$move_1$get_pp_status(),
        if (inherits(private$move_2, "move")) private$move_2$get_pp_status(),
        if (inherits(private$move_3, "move")) private$move_3$get_pp_status(),
        if (inherits(private$move_4, "move")) private$move_4$get_pp_status()
      )
    },

    #' @description
    #' Change one of the moves of the Pokémon
    #'
    #' @param new_move Name of the new move
    #' @param replace_move Either the name of the move to replace, or the position of the move
    #'
    #' @encoding UTF-8
    change_move = function(new_move = NULL, replace_move = NULL) {
      if (is.null(new_move)) {
        available_moves <- setdiff(private$all_moves, self$get_moves())
        move_id <- utils::menu(title = "The following moves can be learnt:", available_moves)
        if (move_id == 0) {
          cat("No new move selected, returning\n")
          return(NULL)
        }
        new_move <- available_moves[move_id]
      } else if (!new_move %in% private$all_moves) {
        stop(new_move, " is not a valid move for ", private$name)
      }

      if (is.null(replace_move)) {
        replace_id <- utils::menu(title = "Which move would you like to replace?", self$get_moves())
      } else if (is.character(replace_move)) {
        if (replace_move %in% self$get_moves()) {
          replace_id <- match(replace_move, self$get_moves())
        } else {
          stop(replace_move, " is not a move known by ", private$name)
        }
      } else if (replace_move %in% 1:4) {
        replace_id <- replace_move
      } else {
        stop("Replacement move (", replace_move, ") for ", private$name, " cannot be found")
      }

      private[[paste0("move_", replace_id)]] <- Move$new(new_move)
      invisible(NULL)
    },

    #' @description
    #' Get the critical hit chance for a move used by the Pokémon
    #'
    #' @param move Name of the move used by the Pokémon
    #'
    #' @encoding UTF-8
    get_crit_chance = function(move) {
      move_name <- move$get_stat("name")
      if (move_name %nin% self$get_moves()) {
        stop(private$name, " does not know ", move_name, ". Please use one of ", toString(self$get_moves()))
      }

      calculate_critical_chance(
        move,
        speed = private$base_speed,
        high_crit = private$critical_hit_change,
        generation = private$generation
      )
    },

    #' @description
    #' Helper function to know that a critical hit has been applied to an attack.
    crit_applied = function() {
      private$crit_taken <- TRUE
    },

    #' @description
    #' Take damage from attack
    #'
    #' @param damage_dealt The amount of damage dealt by the opposing Pokémon's attack
    #'
    #' @encoding UTF-8
    take_damage = function(damage_dealt) {
      if (damage_dealt > 0L) {
        cat(private$name, "took", min(private$current_hp, damage_dealt), "damage\n")
        if (private$crit_taken) {
          cat("Critical Hit!\n")
        }
      }

      private$crit_taken <- FALSE
      private$current_hp <- max(private$current_hp - damage_dealt, 0L)
      invisible(NULL)
    },

    #' @description
    #' Apply an ailment to the Pokémon
    #'
    #' @param ailment Numeric ID of the ailment
    #'
    #' @return
    #' A logical value determining whether or not the ailment has been applied
    #'
    #' @encoding UTF-8
    apply_ailment = function(ailment) {
      ailment_allowed <- check_ailment(private$ailment, ailment)

      if (ailment_allowed) {
        cat(private$name, "has been", AILMENT_CHANGES[ailment], "\n")
        private$ailment <- c(private$ailment, ailment)
      } else {
        current_ailment <- private$ailment[private$ailment <= 5L]
        cat(
          private$name, " is already ", AILMENT_CHANGES[current_ailment], ", ",
          "they cannot be ", AILMENT_CHANGES[ailment], "\n",
          sep = ""
        )
      }

      invisible(ailment_allowed)
    },

    #' @description
    #' Change the stat e.g. attack of a Pokémon
    #'
    #' @details
    #' Stats cannot change by more than +/-6 points
    #'
    #' @param stat The name of the stat to change
    #' @param change Points of change of the stat
    #'
    #' @encoding UTF-8
    change_stat = function(stat = PK_STATS, change = 1L) {
      stat <- match.arg(stat)
      stat_name <- sub("sp_", "special ", stat)
      field <- paste0(stat, "_change")

      direction <- if (sign(change) == 1L) "increase" else "decrease"
      greatly <- if (abs(change) == 2L) "greatly " else ""

      if (abs(private[[field]]) >= 6L && sign(change) == sign(private[[field]])) {
        cat("Unable to", direction, stat_name, "any further\n")
        invisible(NULL)
      } else {
        private[[field]] <- max(-6L, min(6L, private[[field]] + change))
        cat(private$name, "'s ", stat_name, " has ", direction, "d ", greatly, "\n", sep = "")
      }
    },

    #' @description
    #' Heal the Pokémon
    #'
    #' @details
    #' Pokémon healing can never go above full health, and cannot recover from being fainted
    #'
    #' @param perc The percentage of the maximum HP to heal
    #' @param n The percentage points to heal
    #'
    #' @encoding UTF-8
    heal = function(perc, n) {
      if (private$current_hp == 0L) {
        cat(private$name, "is fainted, cannot recover health")
        return(invisible(FALSE))
      }

      if (missing(perc)) {
        health_recovered <- min(n, private$hp - private$current_hp)
      } else {
        health_recovered <- min(private$hp * perc / 100, private$hp - private$current_hp)
      }

      if (private$generation >= 5L) {
        health_recovered <- ceiling(health_recovered)
      } else {
        health_recovered <- floor(health_recovered)
      }

      if (health_recovered > 0L) {
        cat(private$name, "has recovered", health_recovered, "HP\n")
        private$current_hp <- private$current_hp + health_recovered
      } else {
        cat(private$name, "is currently at full health!\n")
      }
    }
  ),

  private = list(
    name = NULL,
    type = NULL,
    type_id = NULL,
    level = NULL,
    generation = NULL,

    hp = NULL,
    base_attack = NULL,
    base_defense = NULL,
    base_sp_attack = NULL,
    base_sp_defense = NULL,
    base_speed = NULL,

    attack = NULL,
    defense = NULL,
    sp_attack = NULL,
    sp_defense = NULL,
    speed = NULL,

    current_hp = NULL,
    ailment = numeric(0L),
    attack_change = 0L,
    defense_change = 0L,
    sp_attack_change = 0L,
    sp_defense_change = 0L,
    speed_change = 0L,
    accuracy_change = 0L,
    evasion_change = 0L,
    critical_hit_change = 0L,
    crit_taken = FALSE,

    move_1 = NULL,
    move_2 = NULL,
    move_3 = NULL,
    move_4 = NULL,
    all_moves = NULL,

    ability = NULL,
    nature = NULL,
    held_item = NULL,

    sprite_front_url = NULL,
    sprite_back_url = NULL
  )
)

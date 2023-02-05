#' Create a New Pokémon
#'
#' @description
#' R6 object containing required information about a Pokémon.
#'
#' @importFrom R6 R6Class
#' @encoding UTF-8
#'
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
      if (generation > 2) {
        nature <- find_nature(nature)
      } else {
        nature <- character(0)
      }
      type_ids <- vapply(api_data$types, \(x) as.integer(basename(x$type$url)), integer(1))

      base_stats <- setNames(
        vapply(api_data$stats, \(x) x$base_stat, integer(1)),
        vapply(api_data$stats, \(x) x$stat$name, character(1))
      )
      hp <- calculate_hp(base_stats[["hp"]], level = level, generation = generation)
      other_stats <- vapply(
        setNames(nm = names(base_stats)[-1]),
        \(x) calculate_stat(base_stats[[x]], x, nature = nature, generation = generation),
        integer(1)
      )

      moves <- learn_moves(api_data$moves, level = level, generation = generation)
      moves_pp <- vapply(moves, get_move_info, info = "pp", integer(1), USE.NAMES = FALSE)

      private$name <- name
      private$type <- types
      private$type_id <- type_ids
      private$level <- as.integer(level)
      private$generation <- generation

      private$base_attack <- base_stats[["attack"]]
      private$base_defense <- base_stats[["defense"]]
      private$base_sp_attack <- base_stats[["special-attack"]]
      private$base_sp_defense <- base_stats[["special-defense"]]
      private$base_speed <- base_stats[["speed"]]

      private$hp <- hp
      private$attack <- other_stats[["attack"]]
      private$defense <- other_stats[["defense"]]
      private$sp_attack <- other_stats[["special-attack"]]
      private$sp_defense <- other_stats[["special-defense"]]
      private$speed <- other_stats[["speed"]]

      private$current_hp <- hp
      private$move_1 <- moves[1]
      private$move_2 <- moves[2]
      private$move_3 <- moves[3]
      private$move_4 <- moves[4]
      private$all_moves <- find_valid_moves(api_data$moves, level = level, generation = generation)

      private$move_1_pp <- moves_pp[1]
      private$move_1_current_pp <- moves_pp[1]
      private$move_2_pp <- moves_pp[2]
      private$move_2_current_pp <- moves_pp[2]
      private$move_3_pp <- moves_pp[3]
      private$move_3_current_pp <- moves_pp[3]
      private$move_4_pp <- moves_pp[4]
      private$move_4_current_pp <- moves_pp[4]

      private$nature <- nature
      private$sprite_front_url <- api_data$sprites$front_default
      private$sprite_back_url <- api_data$sprites$back_default
    },

    #' @description
    #' Show current status of the Pokémon
    #'
    #' @param simple Logical, do you just want the simple status (name + HP) printed?
    #'
    #' @encoding UTF-8
    status = function(simple = FALSE) show_status(private, simple = simple),

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
    get_moves = function() c(private$move_1, private$move_2, private$move_3, private$move_4),

    #' @description
    #' Get the critical hit chance for a move used by the Pokémon
    #'
    #' @param move Name of the move used by the Pokémon
    #'
    #' @encoding UTF-8
    get_crit_chance = function(move) {
      if (move %nin% self$get_moves()) {
        stop(private$name, " does not know ", move, ". Please use one of ", toString(self$get_moves()))
      }

      calculate_critical_chance(
        move,
        speed = private$base_speed,
        high_crit = private$critical_hit_change,
        generation = private$generation
      )
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
    attack = NULL,
    base_defense = NULL,
    defense = NULL,
    base_sp_attack = NULL,
    sp_attack = NULL,
    base_sp_defense = NULL,
    sp_defense = NULL,
    base_speed = NULL,
    speed = NULL,

    current_hp = NULL,
    ailment = 0L,
    attack_change = 0L,
    defense_change = 0L,
    sp_attack_change = 0L,
    sp_defense_change = 0L,
    speed_change = 0L,
    accuracy_change = 0L,
    evasion_change = 0L,
    critical_hit_change = 0L,

    move_1 = NULL,
    move_1_pp = NULL,
    move_1_current_pp = NULL,
    move_2 = NULL,
    move_2_pp = NULL,
    move_2_current_pp = NULL,
    move_3 = NULL,
    move_3_pp = NULL,
    move_3_current_pp = NULL,
    move_4 = NULL,
    move_4_pp = NULL,
    move_4_current_pp = NULL,
    all_moves = NULL,

    ability = NULL,
    nature = NULL,
    held_item = NULL,

    sprite_front_url = NULL,
    sprite_back_url = NULL
  )
)

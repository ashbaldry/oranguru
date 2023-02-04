#' Create a New Pokémon
#'
#' @description
#' R6 object containing required information about a Pokémon.
#'
#' @encoding UTF-8
#' @export
Pokemon <- R6::R6Class(
  classname = "pokemon",
  public = list(
    #' @description
    #' Create a Pokémon
    #'
    #' @param api_data Either data pulled from `pokeapi::get_pokemon`, or
    #' `NULL` to select the data of a random Pokémon
    #' @param pokemon Name of the Pokémon to create
    #' @param level The level the Pokémon should be
    #' @param generation The generation that the Pokémon comes from
    #' @param nature Either a specified nature, or `NULL` for a random nature.
    #' **NB** Nature is only required from generation 3 onwards
    #' @param language ISO-2 character of the language to pull the name of
    #' the Pokémon
    #'
    #' @return
    #' A Pokémon
    initialize = function(api_data = NULL, pokemon = NULL, level = 50L,
                          generation = 1L, nature = NULL, language = "en") {
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
      moves_pp <- vapply(moves, get_move_pp, integer(1), USE.NAMES = FALSE)

      private$name <- name
      private$type <- types
      private$level <- as.integer(level)

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
    #' Show current status of Pokémon
    #'
    #' @param simple Logical, do you just want the simple status (name + HP) printed?
    status = function(simple = FALSE) showStatus(private, simple = simple)
  ),

  private = list(
    name = NULL,
    type = NULL,
    level = NULL,

    hp = NULL,
    attack = NULL,
    defense = NULL,
    sp_attack = NULL,
    sp_defense = NULL,
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

    ability = NULL,
    nature = NULL,
    held_item = NULL,

    sprite_front_url = NULL,
    sprite_back_url = NULL
  )
)

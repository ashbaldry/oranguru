#' Create a New Pokémon
#'
#' @description
#' R7 object containing required information about a Pokémon.
#'
#' @param api_data Either data pulled from `pokeapi::get_pokemon`, or
#' `NULL` to select the data of a random Pokémon
#' @param level The level the Pokémon should be
#' @param generation The generation that the Pokémon comes from
#' @param nature Either a specified nature, or `NULL` for a random nature.
#' **NB** Nature is only required from generation 3 onwards
#' @param language ISO-2 character of the language to pull the name of
#' the Pokémon
#'
#' @return
#' A Pokémon.
#'
#' @encoding UTF-8
#' @import R7
#' @export
pokemon <- R7::new_class(
  name = "Pokemon",
  package = "pokemon",
  properties = list(
    name = R7::class_character,
    type = R7::class_character,
    level = R7::class_integer,

    hp = R7::class_integer,
    attack = R7::class_integer,
    defense = R7::class_integer,
    sp_attack = R7::class_integer,
    sp_defense = R7::class_integer,
    speed = R7::class_integer,

    current_hp = R7::class_integer,
    attack_change = R7::class_integer,
    defense_change = R7::class_integer,
    sp_attack_change = R7::class_integer,
    sp_defense_change = R7::class_integer,
    speed_change = R7::class_integer,
    accuracy_change = R7::class_integer,
    evasion_change = R7::class_integer,
    critical_hit_change = R7::class_integer,

    move_1 = R7::class_character,
    move_1_pp = R7::class_integer,
    move_1_current_pp = R7::class_integer,
    move_2 = R7::class_character,
    move_2_pp = R7::class_integer,
    move_2_current_pp = R7::class_integer,
    move_3 = R7::class_character,
    move_3_pp = R7::class_integer,
    move_3_current_pp = R7::class_integer,
    move_4 = R7::class_character,
    move_4_pp = R7::class_integer,
    move_4_current_pp = R7::class_integer,

    ability = R7::class_character,
    nature = R7::class_character,
    held_item = R7::class_character,

    sprite_front_url = R7::class_character,
    sprite_back_url = R7::class_character
  ),
  constructor = function(api_data = NULL, level = 50L, generation = 8L,
                         nature = NULL, language = "en") {
    check_level(level)
    check_generation(generation)

    if (is.null(api_data)) {
      pokemon_id <- pokeapi::random_pk_id("pokemon")
      api_data <- pokeapi::get_pokemon(pokemon_id)
    }

    id <- api_data$id

    name <- get_pokemon_name(id, language = language)
    types <- vapply(api_data$types, \(x) x$type$name, character(1))
    if (generation > 2) nature <- find_nature(nature)

    base_stats <- setNames(
      vapply(api_data$stats, \(x) x$base_stat, integer(1)),
      vapply(api_data$stats, \(x) x$stat$name, character(1))
    )
    hp <- calculate_hp(base_stats[["hp"]], level = level, generation = generation)
    other_stats <- vapply(
      setNames(nm = names(base_stats)[-1]),
      \(x) calculate_stat(base_stats[[x]], x, nature = nature),
      integer(1)
    )

    moves <- learn_moves(api_data$moves, level = level, generation = generation)

    R7::new_object(
      R7::R7_object(),
      name = name,
      type = types,
      level = as.integer(level),

      hp = hp,
      attack = other_stats[["attack"]],
      defense = other_stats[["defense"]],
      sp_attack = other_stats[["special-attack"]],
      sp_defense = other_stats[["special-defense"]],
      speed = other_stats[["speed"]],

      current_hp = hp,
      attack_change = 0L,
      defense_change = 0L,
      sp_attack_change = 0L,
      sp_defense_change = 0L,
      speed_change = 0L,
      accuracy_change = 0L,
      evasion_change = 0L,
      critical_hit_change = 0L,

      move_1 = moves[1],
      move_2 = moves[2],
      move_3 = moves[3],
      move_4 = moves[4],

      move_1_pp = 10L,
      move_1_current_pp = 10L,
      move_2_pp = 10L,
      move_2_current_pp = 10L,
      move_3_pp = 10L,
      move_3_current_pp = 10L,
      move_4_pp = 10L,
      move_4_current_pp = 10L,

      nature = nature,
      ability = character(),
      held_item = character(),

      sprite_front_url = api_data$sprites$front_default,
      sprite_back_url = api_data$sprites$back_default
    )
  }
)

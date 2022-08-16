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

    ability = R7::class_character,
    nature = R7::class_character,

    move_1 = R7::class_character,
    move_2 = R7::class_character,
    move_3 = R7::class_character,
    move_4 = R7::class_character,

    held_item = R7::class_character,

    sprite_front_url = R7::class_character,
    sprite_back_url = R7::class_character
  )
)

#' Create a New Pokémon
#'
#' @encoding UTF-8
#' @export
new_pokemon <- function(api_data = NULL, level = 50L, generation = 8,
                        nature = NULL, language = "en") {
  if (is.null(api_data)) {
    pokemon_id <- pokeapi::random_pk_id("pokemon")
    api_data <- pokeapi::get_pokemon(pokemon_id)
  }

  id <- api_data$id

  name <- get_pokemon_name(id, language = language)
  types <- vapply(api_data$types, \(x) x$type$name, character(1))
  if (generation > 2) nature <- check_nature(nature)

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

  pokemon(
    name = name,
    type = types,
    nature = nature,
    level = level,

    hp = hp,
    attack = other_stats[["attack"]],
    defense = other_stats[["defense"]],
    sp_attack = other_stats[["special-attack"]],
    sp_defense = other_stats[["special-defense"]],
    speed = other_stats[["speed"]],

    move_1 = moves[1],
    move_2 = moves[2],
    move_3 = moves[3],
    move_4 = moves[4],

    sprite_front_url = api_data$sprites$front_default,
    sprite_back_url = api_data$sprites$back_default
  )
}

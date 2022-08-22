#' Pokémon Team
#'
#' @export
team <- R7::new_class(
  name = "team",
  package = "pokemon",

  properties = list(
    active = R7::class_integer,
    pokemon_1 = pokemon,
    pokemon_2 = pokemon,
    pokemon_3 = pokemon,
    pokemon_4 = pokemon,
    pokemon_5 = pokemon,
    pokemon_6 = pokemon
  ),

  constructor = function(generation = 8L) {
    R7::new_object(
      R7::R7_object(),
      active = 1L,
      pokemon_1 = pokemon(generation = generation),
      pokemon_2 = pokemon(generation = generation),
      pokemon_3 = pokemon(generation = generation),
      pokemon_4 = pokemon(generation = generation),
      pokemon_5 = pokemon(generation = generation),
      pokemon_6 = pokemon(generation = generation)
    )
  }
)

#' Pokémon Team
#'
#' @description
#' R7 object containing required information about a Pokémon.
#'
#' @param pokemon_1,pokemon_2,pokemon_3,pokemon_4,pokemon_5,pokemon_6 Pre-created Pokémon
#' @param random Logical, should the team be randomised? Default set to `TRUE`
#' @param generation The generation that the Pokémon comes from
#'
#' @return
#' A Team of 6 Pokémon.
#'
#' @encoding UTF-8
#' @import R7
#' @include pokemon.R
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

  constructor = function(pokemon_1, pokemon_2, pokemon_3, pokemon_4, pokemon_5, pokemon_6,
                         random = TRUE, generation = 8L) {
    if (random) {
      pokemon_ids <- get_random_pokemon_id(generation, n = 6)

      R7::new_object(
        R7::R7_object(),
        active = 1L,
        pokemon_1 = pokemon(pokemon = pokemon_ids[1], generation = generation),
        pokemon_2 = pokemon(pokemon = pokemon_ids[2], generation = generation),
        pokemon_3 = pokemon(pokemon = pokemon_ids[3], generation = generation),
        pokemon_4 = pokemon(pokemon = pokemon_ids[4], generation = generation),
        pokemon_5 = pokemon(pokemon = pokemon_ids[5], generation = generation),
        pokemon_6 = pokemon(pokemon = pokemon_ids[6], generation = generation)
      )
    } else if (missing(pokemon_1) || missing(pokemon_2) || missing(pokemon_3) ||
               missing(pokemon_4) || missing(pokemon_5) || missing(pokemon_6)) {
      stop("Must have a full set of 6 Pokémon to create a team")
    } else {
      R7::new_object(
        R7::R7_object(),
        active = 1L,
        pokemon_1 = pokemon_1,
        pokemon_2 = pokemon_2,
        pokemon_3 = pokemon_3,
        pokemon_4 = pokemon_4,
        pokemon_5 = pokemon_5,
        pokemon_6 = pokemon_6
      )
    }
  }
)

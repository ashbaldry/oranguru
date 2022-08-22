#' Get Pokémon Name
#' @noRd
get_pokemon_name <- function(id, language = "en") {
  if (id < 10000) {
    pokemon <- pokeapi::get_pokemon_species(id)
    pokemon_names <- pokemon$names
  } else {
    pokemon <- pokeapi::get_pokemon_form(id)
    pokemon_names <- pokemon$names
  }

  language_name <- vapply(pokemon_names, \(x) x$language$name == language, logical(1))
  if (!any(language_name)) {
    language_name <- vapply(pokemon_names, \(x) x$language$name == "en", logical(1))
  }

  pokemon_names[language_name][[1]]$name
}

#' @noRd
get_random_pokemon_id <- function(generation = 8L) {
  subset(pokemon_generation, generation_id == generation) |>
    get_column("pokemon_id") |>
    sample(1)
}

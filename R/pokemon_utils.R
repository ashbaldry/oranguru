#' Get Pokémon Name
#'
#' @param id ID of the Pokémon to get the name of
#' @param langauge 2 digit language code of the language to get. Reverts to English if
#' selected country is not available
#'
#' @noRd
get_pokemon_name <- function(id, language = "en") {
  if (id < 10000L) {
    pokemon <- pokeapi::get_pokemon_species(id)
    pokemon_names <- pokemon$names
  } else {
    pokemon <- pokeapi::get_pokemon_form(id)
    pokemon_names <- pokemon$names
  }

  language_name <- vapply(pokemon_names, \(x) x$language$name == language, logical(1L))
  if (!any(language_name)) {
    language_name <- vapply(pokemon_names, \(x) x$language$name == "en", logical(1L))
  }

  pokemon_names[language_name][[1]]$name
}

#' @noRd
get_random_pokemon_id <- function(generation = 1L, n = 1L) {
  # nolint start: object_usage_linter
  subset(pokemon_generation, generation_id == generation, select = "pokemon_id", drop = TRUE) |>
  # nolint end
    remove_alternate_forms() |>
    sample(size = n, replace = FALSE)
}

#' @noRd
remove_alternate_forms <- function(x) {
  x[x < 10000L]
}

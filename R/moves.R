#' Learn Pokémon Moves
#'
#' @param moves A list of moves extracted from `pokeapi::get_pokemon`
#' @param level The level of the Pokémon. Used to work out which
#' moves are viable
#' @param generation The generation to look at. Used to work out
#' which moves are viable
#' @param n The maximum number of moves learnt by the Pokémon. Must be
#' between 1 and 4 (default is 4).
#'
#' @examplesIf interactive()
#' pikachu <- pokeapi::get_pokemon(25)
#' pikachu_moves <- learn_moves(pikachu$moves)
#'
#' @encoding UTF-8
#' @export
learn_moves <- function(moves, level = 50L, generation = 1L, n = 4L) {
  if (!is.numeric(n) || n < 1L || n > 4L || as.integer(n) != n) {
    stop("Maximum number of moves learnt must be between one and four")
  }
  learnable_moves <- find_valid_moves(moves, level, generation)
  sample(learnable_moves, min(n, length(learnable_moves)))
}

#' @rdname learn_moves
#' @export
choose_moves <- function(moves, level = 50L, generation = 1L, n = 4L) {
  if (!is.numeric(n) || n < 1L || n > 4L || as.integer(n) != n) {
    stop("Maximum number of moves learnt must be between one and four")
  }

  learnable_moves <- find_valid_moves(moves, level, generation)
  # TODO: Enable user to add own moves to pokemon
  sample(learnable_moves, min(n, length(learnable_moves)))
}

find_valid_moves <- function(moves, level = 50L, generation = 1L) {
  check_level(level)
  check_generation(generation)
  # Handling if someone sends the whole result of pokeapi::get_pokemon
  if ("moves" %in% names(moves)) {
    moves <- moves$moves
  }

  moves_df <- do.call(rbind, lapply(moves, clean_learned_moves))
  moves_df$generation_id <- GAME_GENERATIONS[moves_df$version]
  # nolint start: object_usage_linter
  moves_df <- subset(moves_df, generation_id == generation, level_learned <= level)
  # nolint end

  unique(moves_df$move)
}

clean_learned_moves <- function(move) {
  data.frame(
    move = move$move$name,
    level_learned = vapply(move$version_group_details, \(x) x$level_learned_at, integer(1)),
    version = vapply(move$version_group_details, \(x) x$version_group$name, character(1))
  )
}

#' @noRd
get_move_info <- function(move, info) {
  if (is.na(move)) {
    NA_integer_
  } else {
    # nolint start: object_usage_linter
    subset(moves, identifier == move) |>
      # nolint end
      get_column(info) |>
      as.integer()
  }
}

GAME_GENERATIONS <- c(
  "red-blue" = 1L,
  "yellow" = 1L,
  "gold-silver" = 2L,
  "crystal" = 2L,
  "ruby-sapphire" = 3L,
  "emerald" = 3L,
  "fire-red-leaf-green" = 3L,
  "diamond-pearl" = 4L,
  "platium" = 4L,
  "heart-gold-soul-silver" = 4L,
  "black-white" = 5L,
  "black-2-white-2" = 5L,
  "x-y" = 6L,
  "omega-ruby-alpha-sapphire" = 6L,
  "sun-moon" = 7L,
  "ultra-sun-ultra-moon" = 7L,
  "sword-shield" = 8L,
  "brilliant-diamond-shining-pearl" = 8L
)

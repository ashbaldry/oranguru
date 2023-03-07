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

#' @noRd
find_valid_moves <- function(moves, level = 50L, generation = 1L) {
  check_level(level)
  check_generation(generation)
  # Handling if someone sends the whole result of pokeapi::get_pokemon
  if ("moves" %in% names(moves)) {
    moves <- moves$moves
  }

  unlist(lapply(moves, check_move_availability, level = level, generation = generation))
}

#' @noRd
check_move_availability <- function(move, level = 50L, generation = 1L) {
  generations <- vapply(
    move$version_group_details,
    \(x) GAME_GENERATIONS[[x$version_group$name]],
    integer(1)
  )

  valid_generation <- generations == generation
  if (any(valid_generation)) {
    level_learned <- vapply(
      move$version_group_details[valid_generation],
      \(x) x$level_learned_at,
      integer(1)
    )

    if (any(level_learned <= level)) move$move$name else NULL
  } else {
    NULL
  }
}

#' @noRd
get_move_info <- function(move, info) {
  if (is.na(move)) {
    NA_integer_
  } else {
    # nolint start: object_usage_linter
    subset(moves, identifier == move, select = info, drop = TRUE)
    # nolint end
  }
}

#' @noRd
get_move_meta_info <- function(move, info) {
  if (is.na(move)) {
    NA_integer_
  } else {
    # nolint start: object_usage_linter
    id <- subset(moves, identifier == move, select = "id", drop = TRUE)
    subset(moves_meta, move_id == id, select = info, drop = TRUE)
    # nolint end
  }
}

GAME_GENERATIONS <- c(
  "red-blue" = 1L,
  "yellow" = 1L,
  "gold-silver" = 2L,
  "crystal" = 2L,
  "ruby-sapphire" = 3L,
  "emerald" = 3L,
  "firered-leafgreen" = 3L,
  "diamond-pearl" = 4L,
  "platinum" = 4L,
  "heartgold-soulsilver" = 4L,
  "black-white" = 5L,
  "black-2-white-2" = 5L,
  "x-y" = 6L,
  "omega-ruby-alpha-sapphire" = 6L,
  "sun-moon" = 7L,
  "ultra-sun-ultra-moon" = 7L,
  "sword-shield" = 8L,
  "brilliant-diamond-shining-pearl" = 8L,
  "scarlet-violet" = 9L,
  "colosseum" = 0L,
  "xd" = 0L,
  "lets-go-pikachu-lets-go-eevee" = 0L
)

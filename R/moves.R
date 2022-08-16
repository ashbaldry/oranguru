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
learn_moves <- function(moves, level = 50, generation = 8, n = 4) {
  learnable_moves <- find_valid_moves(moves, level, generation)
  sample(learnable_moves, min(n, length(learnable_moves)))
}

#' @rdname learn_moves
#' @export
choose_moves <- function(moves, level = 50, generation = 8, n = 4) {
  learnable_moves <- find_valid_moves(moves, level, generation)
  # TODO: Enable user to add own moves to pokemon
  sample(learnable_moves, min(n, length(learnable_moves)))
}

find_valid_moves <- function(moves, level = 50, generation = 8) {
  check_level(level)
  check_generation(generation)
  if (n < 1 || n > 4 || as.integer(n) != n) {
    stop("Maximum number of moves learnt must be between one and four")
  }
  # Handling if someone sends the whole result of pokeapi::get_pokemon
  if ("moves" %in% names(moves)) {
    moves <- moves$moves
  }

  moves_df <- do.call(rbind, lapply(moves, get_move_info))
  moves_df$generation_id <- GAME_GENERATIONS[moves_df$version]
  moves_df <- subset(moves_df, generation_id == generation, level_learned <= level)

  unique(moves_df$move)
}

get_move_info <- function(move) {
  data.frame(
    move = move$move$name,
    level_learned = vapply(move$version_group_details, \(x) x$level_learned_at, integer(1)),
    version = vapply(move$version_group_details, \(x) x$version_group$name, character(1))
  )
}

GAME_GENERATIONS <- c(
  "red-blue" = 1,
  "yellow" = 1,
  "gold-silver" = 2,
  "crystal" = 2,
  "ruby-sapphire" = 3,
  "emerald" = 3,
  "fire-red-leaf-green" = 3,
  "diamond-pearl" = 4,
  "platium" = 4,
  "heart-gold-soul-silver" = 4,
  "black-white" = 5,
  "black-2-white-2" = 5,
  "x-y" = 6,
  "omega-ruby-alpha-sapphire" = 6,
  "sun-moon" = 7,
  "ultra-sun-ultra-moon" = 7,
  "sword-shield" = 8,
  "brilliant-diamond-shining-pearl" = 8
)
